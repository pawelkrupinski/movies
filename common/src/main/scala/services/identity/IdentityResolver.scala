package services.identity

import services.movies.{ListingKey, TitleNormalizer}

/**
 * `resolve(E)`: film identity as a pure, deterministic function of a SET of listings and the
 * answers of a lookup source (docs/design/identity-resolver.md, phase 2). Production code that
 * serves nothing: its only consumers are the shadow report, the recording sweep and curation.
 *
 * Two stages, every step a function of the set:
 *
 *   A. CANDIDATE GENERATION ([[CandidateGeneration]]). Each listing's own detail page merges into
 *      its [[Evidence]]; listings with identical evidence are one NODE. Every node's
 *      [[CandidateQueries]] are asked — all of them, up front, in sorted order, none conditional on
 *      another's answer (A1) — and every film any answer names is looked up once. The nodes are
 *      grouped into FAMILIES ([[Families]]), the block closure of `FamilyClosure` over their title
 *      keys and the films they match.
 *
 *   B. GLOBAL ASSIGNMENT. Every node scores every candidate it has an EVIDENCE PATH to (its own
 *      query named it, or a film title relates to its title) with the calibrated model
 *      ([[IdentityCalibration]] over [[IdentityMeasures]] — the same measurements the weights were
 *      fitted on, including `venues.corroborating`, the family's venue co-occurrence; [[FamilyScope]]).
 *      A candidate a node's evidence DENIES (`ListingConstraints.learnedListingFilm`: a learned
 *      rule, or its OWN facts' probability below the certified cut, and only when the node
 *      publishes a fact the film can be compared on — a title relation alone is scored, never a
 *      veto; or a film of the node's credited director its title does not name, when its title
 *      names another of that director's) is not eligible. A node ACCEPTS a candidate ALONE by the
 *      rules of [[Acceptance]]; otherwise it follows its cluster. Then:
 *        1. constraint edges between nodes sharing a block key ([[ConstraintEdges]]), solved by
 *           [[ConstraintSolver]] (cannot wins, and no component holds two films however the
 *           must-links chain; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING ([[ClusterVoting]]): a cluster no member of which accepted a film
 *           scores its members' evidence POOLED into one listing (the heaviest title, the modal
 *           year, every director), and the winner, if accepted (`Acceptance.pooled`), becomes
 *           every member's film. A winner some members' own evidence denies is not a veto of the
 *           whole cluster: those members split off and the rest take it when their own pooled
 *           facts carry it. A winner no member's title names — only a credited director's
 *           filmography reached it — must also be the one film the pooled facts and the
 *           calibration both rank first;
 *        3. the constraints are re-solved with those films, and each final cluster is a
 *           [[ResolverDecision]] with its confidence, basis and explanation ([[ResolverDecisions]]).
 *
 * Every family is resolved on its own. That is sound only while no edge crosses a family, which
 * holds by construction — every edge joins two nodes sharing a block key — and is CHECKED: a
 * crossing edge fails the resolve rather than scoping it.
 *
 * `Mutation` is for the teeth tests only: each variant breaks exactly one of the properties the
 * specs prove, and the specs must catch it.
 */
object IdentityResolver {

  private[identity] enum Mutation {
    case None
    /** A2 broken: must-links applied in arrival order, first wins. */
    case FirstWins
    /** A1 broken: listings walked in arrival order, a title query whose sanitised form an
     *  earlier listing already asked is skipped — the old "sister-row shortcut". */
    case LazyLookups
    /** Group-level voting off: a cluster with no own match stays unmatched. */
    case NoVoting
    /** Families narrowed to the sanitised title: an edge through an original title, a search
     *  form or a shared film then crosses a family and the resolve must refuse. */
    case NarrowFamilies
  }

  /** Thrown when `count` edges cross a family: a rule was added without its block key. */
  final class FamilyCrossing(val count: Int, message: String) extends IllegalStateException(message)

  /** `pins` are the curation's hard constraints (`ListingConstraints.pinned`): a pinned film
   *  replaces a listing's own match, a denied one is never eligible, a pinned group is must-linked
   *  above every derived tier, and a derived edge the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              pins: PinConstraints = PinConstraints(Nil),
              decorations: TitleDecorations = TitleDecorations.resolver): Resolution =
    resolveWith(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)

  /** Stage A of a resolve — candidate generation, scoring and families — wired once, for the resolve
   *  and for [[candidatesOf]]. */
  private final class Stages(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                             calibration: IdentityCalibration, mutation: Mutation, pins: PinConstraints, decorations: TitleDecorations,
                             corpus: Option[CorpusContext] = None) {
    private val arrival = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
    // One listing per key, the smallest by the total order — never the first to arrive.
    private val all     = listings.toSeq.sorted.distinctBy(_.key)
    val ordered: Seq[Listing] = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all
    val generation = new CandidateGeneration(ordered, lookups, normalizer, pins, decorations, lazyLookups = mutation == Mutation.LazyLookups, corpus)
    val acceptance = new Acceptance(calibration)
    val scoring    = new CandidateScoring(generation, calibration, acceptance.weights, pins)
    val links      = new TitleLinks(generation.nodes, normalizer, pins, generation.context.wholeTitle, generation.context.bannerSegment)
    val families   = new Families(scoring, acceptance, links, normalizer, narrow = mutation == Mutation.NarrowFamilies)
  }

  /** One candidate as a node's family scored it, for a report reading why a listing took what it took. */
  final case class CandidateScore(tmdbId: Int, title: String, year: Option[Int], probability: Double, searchRank: Option[Int],
                                  denial: Option[String], seasonProduction: Boolean, houseProduction: Boolean, explanation: String) {
    def denied: Boolean = denial.isDefined
    def render: String =
      f"$tmdbId%8d ${ResolverDecision.percent(probability)}%6s rank ${searchRank.fold("-")(_.toString)}%2s" +
        s"${denial.fold("")(why => s" DENIED ($why)")}${if (seasonProduction) " season" else ""}${if (houseProduction) " house" else ""} " +
        s"'$title'${year.fold("")(filmYear => s" ($filmYear)")} — $explanation"
  }

  /** A focused node, its candidates as its family scores them (best first), and each banner its
   *  candidates bill it under with the houses contending for that banner, best first, and the one learned. */
  final case class NodeCandidates(label: String, candidates: Seq[CandidateScore], banners: Seq[String])

  /** Why a listing resolved as it did and sits where it does — the resolve's own stages, told:
   *  its node; every family key it has and why, and every title piece it does not block under and
   *  why not; the chain of shared keys linking it to its family's heaviest member (why THIS
   *  family); its decision with the decision's own explanation; and its candidates as its family
   *  scored them. */
  final case class ListingExplanation(node: String, family: Int, familyListings: Int, keys: Seq[(String, String)],
                                      dropped: Seq[(String, String)], chain: Seq[(String, String)], decision: ResolverDecision,
                                      candidates: Seq[CandidateScore]) {
    def render: Seq[String] =
      Seq(s"listing in node $node, family $family ($familyListings listing(s))",
          s"  decision: ${decision.render}") ++
        keys.map { case (key, why) => s"  key $key — $why" } ++
        dropped.map { case (piece, why) => s"  not a key: $piece — $why" } ++
        (if (chain.isEmpty) Seq("  the family's heaviest node itself") else s"  in this family by:" +: chain.map { case (key, next) => s"    —$key→ $next" }) ++
        candidates.take(8).map(candidate => s"  candidate ${candidate.render}")
  }

  def explain(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              pins: PinConstraints = PinConstraints(Nil),
              decorations: TitleDecorations = TitleDecorations.resolver)(wanted: Set[ListingKey]): Seq[ListingExplanation] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)
    import stages.families.{blockKeysOf, familyOf, scopeOf}
    lazy val resolution = run(stages, Mutation.None)
    // One explanation per node holding a wanted listing: a node's listings share everything told.
    stages.generation.nodes.filter(_.listings.exists(listing => wanted(listing.key))).map { node =>
      val listing  = node.listings.find(listing => wanted(listing.key)).get.key
      val context  = stages.generation.context
      val keyed    = TitleLinks.keyed(node, normalizer, pins, context.wholeTitle, context.bannerSegment)
      val titled   = keyed.keys
      val accepted = blockKeysOf(node.id).toSeq.sorted.filterNot(titled).map(key => key -> "a film it, or a node sharing its keys, accepted")
      val members  = stages.generation.nodes.filter(member => familyOf(member.id) == familyOf(node.id))
      val anchor   = members.minBy(member => (-member.weight, member.id))
      // Breadth-first over shared keys from the node to the anchor: the links that put it here.
      val byKey    = members.flatMap(member => blockKeysOf(member.id).map(_ -> member)).groupMap(_._1)(_._2)
      val via      = scala.collection.mutable.HashMap(node.id -> Option.empty[(String, EvidenceNode)])
      val queue    = scala.collection.mutable.Queue(node)
      while (queue.nonEmpty && !via.contains(anchor.id)) {
        val current = queue.dequeue()
        blockKeysOf(current.id).toSeq.sorted.foreach(key => byKey.getOrElse(key, Nil).foreach { next =>
          if (!via.contains(next.id)) { via(next.id) = Some(key -> current); queue.enqueue(next) } })
      }
      def path(at: EvidenceNode): List[(String, String)] = via.get(at.id).flatten match {
        case Some((key, from)) => path(from) :+ (key -> at.label)
        case None              => Nil
      }
      val decision = resolution.decisionOf(listing)
      ListingExplanation(node.label, familyOf(node.id), members.map(_.weight).sum, keyed.kept ++ accepted, keyed.dropped,
        if (anchor eq node) Nil else path(anchor), decision,
        scopeOf(node).of(node).map(scored => CandidateScore(scored.candidate.tmdbId, scored.candidate.film.title, scored.candidate.film.year,
          scored.probability, scored.rank, scored.denial, scored.seasonProduction, scored.houseProduction,
          calibration.explain(IdentityMeasures.ListingFilm, scored.measures))))
    }
  }

  /** One of the largest families, taken apart: its size, the block keys holding the most of its
   *  nodes, and for each of those the nodes the family's largest piece keeps once that key is dropped
   *  — a key that alone glues the family together leaves a small piece. For a report finding why
   *  a family grew far past one film. */
  final case class FamilyAnatomy(listings: Int, nodes: Int, keys: Seq[(String, Int)], withoutKey: Seq[(String, Int)],
                                 connectors: Seq[(String, Seq[String])] = Nil, withoutKind: Seq[(String, Int)] = Nil) {
    def render: Seq[String] =
      s"family of $listings listing(s) in $nodes node(s)" +:
        (s"  keys by nodes: ${keys.map { case (key, count) => s"$key ×$count" }.mkString(", ")}" +:
          (withoutKind.map { case (kind, largest) => s"  without every $kind key: largest piece $largest node(s)" } ++
            withoutKey.map { case (key, largest) => s"  without $key: largest piece $largest node(s)" } ++
            connectors.map { case (node, shared) => s"  connector $node: ${shared.mkString(", ")}" }))
  }

  def familyAnatomy(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                    calibration: IdentityCalibration = IdentityCalibration.resolver,
                    decorations: TitleDecorations = TitleDecorations.resolver)(largest: Int): Seq[FamilyAnatomy] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations)
    import stages.families.{blockKeysOf, familyOf}
    stages.generation.nodes.groupBy(node => familyOf(node.id)).values.toSeq.sortBy(members => -members.map(_.weight).sum).take(largest).map { members =>
      val keysOf = members.map(node => node.id -> blockKeysOf(node.id)).toMap
      val byKey  = keysOf.toSeq.flatMap { case (id, keys) => keys.map(_ -> id) }.groupMap(_._1)(_._2)
      val ranked = byKey.toSeq.map { case (key, ids) => key -> ids.size }.sortBy { case (key, count) => (-count, key) }
      def largestPieceKeeping(kept: String => Boolean): Int =
        FamilyClosure.families(keysOf.map { case (id, keys) => id -> keys.filter(kept) }).groupBy(_._2).values.map(_.size).maxOption.getOrElse(0)
      def largestPieceWithout(dropped: String): Int = largestPieceKeeping(_ != dropped)
      // Which KIND of key meshes it: a sanitised title or segment (t:), a search form (q:), a film
      // members accepted (id:), a pin's group.
      val withoutKind = Seq("t:", "q:", "id:").map(kind => kind -> largestPieceKeeping(key => !key.startsWith(kind))) :+
        ("t: and q:" -> largestPieceKeeping(key => !key.startsWith("t:") && !key.startsWith("q:")))
      // The nodes joining the most keys other members carry: in a mesh no single key bridges, these weld it.
      // Each title key with the inputs the segment rule read: its spread and whether it is a whole title.
      val context = stages.generation.context
      def explained(key: String) =
        if (key.startsWith("t:")) s"$key(spread ${context.segmentSpread(key.drop(2))}${if (context.wholeTitle(key.drop(2))) ", whole" else ""})" else key
      val connectors = members.map(node => node -> keysOf(node.id).filter(key => byKey(key).sizeIs > 1).toSeq.sorted)
        .filter(_._2.sizeIs >= 2).sortBy { case (node, shared) => (-shared.size, node.id) }.take(40)
        .map { case (node, shared) => s"${node.label} «${node.evidence.published.rawTitle.getOrElse(node.evidence.title)}»" -> shared.map(explained) }
      FamilyAnatomy(members.map(_.weight).sum, members.size, ranked.take(25), ranked.take(15).map { case (key, _) => key -> largestPieceWithout(key) },
        connectors, withoutKind)
    }
  }

  /** Every node holding a listing `focused` selects — the resolve's own stages, so the report shows
   *  what the resolve read. */
  def candidatesOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                   calibration: IdentityCalibration = IdentityCalibration.resolver,
                   pins: PinConstraints = PinConstraints(Nil),
                   decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeCandidates] = {
    val stages = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)
    import stages.scoring.{houseRanking, houses}
    stages.generation.nodes.filter(_.listings.exists(focused)).map { node =>
      val scored  = stages.families.scopeOf(node).of(node)
      val banners = scored.flatMap(candidate => IdentityMeasures.billing(node.evidence.measured, candidate.candidate.film)).map(_.listingHouse).distinct.sorted
      NodeCandidates(node.label,
        scored.map { candidate =>
          CandidateScore(candidate.candidate.tmdbId, candidate.candidate.film.title, candidate.candidate.film.year, candidate.probability, candidate.rank,
            candidate.denial, candidate.seasonProduction, candidate.houseProduction, calibration.explain(IdentityMeasures.ListingFilm, candidate.measures))
        },
        banners.map(banner => s"banner '$banner' → ${houses.of.getOrElse(banner, "no house")}; contenders: " +
          houseRanking.getOrElse(banner, Nil).take(4).map(_.render).mkString(", ")))
    }
  }

  /** The corpus-wide facts a resolve of `listings` reads ([[CorpusContext]]): what one family,
   *  resolved alone with `corpus = Some(...)`, needs to decide as the whole resolve does. */
  private[identity] def contextOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                  pins: PinConstraints = PinConstraints(Nil),
                                  decorations: TitleDecorations = TitleDecorations.None): CorpusContext =
    new CandidateGeneration(listings.toSeq.sorted.distinctBy(_.key), lookups, normalizer, pins, decorations, lazyLookups = false).context

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    calibration: IdentityCalibration, mutation: Mutation,
                                    pins: PinConstraints = PinConstraints(Nil),
                                    decorations: TitleDecorations = TitleDecorations.None,
                                    corpus: Option[CorpusContext] = None): Resolution = {
    run(new Stages(listings, lookups, normalizer, calibration, mutation, pins, decorations, corpus), mutation)
  }

  /** One family of a region's resolve ([[resolveRegion]]): its listings, its decisions, the keys it
   *  blocks under (its title keys and the films its members matched — what another family must share
   *  to merge with it), the questions and records its answers came from, and the corpus-wide facts
   *  it could read ([[CorpusContext.Reads]]). */
  private[identity] final case class RegionFamily(listings: Set[ListingKey], decisions: Seq[ResolverDecision], blockKeys: Set[String],
                                                  queries: Set[CandidateQuery], films: Set[Int], reads: CorpusContext.Reads,
                                                  nodeKeys: Map[ListingKey, String])

  /** Resolve `listings` — a union of whole families — against the corpus's `corpus` context, family
   *  by family: A3 with the corpus's facts, so each decides as the whole resolve would. */
  private[identity] def resolveRegion(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                      calibration: IdentityCalibration, pins: PinConstraints, decorations: TitleDecorations,
                                      corpus: CorpusContext): Seq[RegionFamily] = {
    val stages     = new Stages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations, Some(corpus))
    val resolution = run(stages, Mutation.None)
    import stages.{families, generation}
    generation.nodes.groupBy(node => families.familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members) =>
      val keys  = members.flatMap(_.listings.map(_.key)).toSet
      val pool  = families.scopes(family).pool
      val queries = members.flatMap(node => generation.queriesOf(node.id)).toSet
      val films = members.flatMap(node => generation.queriesOf(node.id).flatMap(query => generation.answers(query).toOption.getOrElse(Nil)).map(_.tmdbId)).toSet
      val decisions = resolution.decisions.filter(_.members.exists(keys))
      RegionFamily(keys, decisions, members.flatMap(node => families.blockKeysOf(node.id)).toSet, queries, films,
        CorpusContext.Reads.of(members, pool.map(_.film), films ++ pool.map(_.tmdbId) ++ decisions.flatMap(_.film), queries, normalizer.sanitize),
        members.flatMap(node => node.listings.map(listing =>
          listing.key -> CandidateGeneration.nodeKeyText(CandidateGeneration.nodeKey(listing, node.evidence, pins)))).toMap)
    }
  }

  private def run(stages: Stages, mutation: Mutation): Resolution = {
    import stages.{acceptance, families, generation, links, ordered, scoring}
    import generation.{answers, candidateOf, details, issued, nodeById, nodes, records}
    import families.{familyOf, scopes}

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    val edges     = new ConstraintEdges(scoring, families, links)
    val voting    = new ClusterVoting(scoring, families, acceptance)
    val decisions = new ResolverDecisions(scoring, families, acceptance)

    def solve(members: Seq[EvidenceNode], edges: Seq[ResolverEdge], filmOf: String => Option[Int]): Seq[Seq[EvidenceNode]] = {
      val constraints = edges.map(edge => ConstraintSolver.Constraint(edge.a, edge.b, edge.must, edge.tier, edge.reason))
      val presentation = if (mutation == Mutation.FirstWins) ConstraintSolver.Presentation.AsGiven else ConstraintSolver.Presentation.Canonical
      val presented = if (mutation == Mutation.FirstWins) {
        val position = ordered.zipWithIndex.map { case (listing, index) => listing.sortKey -> index }.toMap
        members.sortBy(node => node.listings.map(listing => position(listing.sortKey)).min)
      } else members
      val orderedConstraints = if (mutation == Mutation.FirstWins) {
        val position = presented.zipWithIndex.map { case (node, index) => node.id -> index }.toMap
        constraints.sortBy(constraint => (math.min(position(constraint.a), position(constraint.b)), math.max(position(constraint.a), position(constraint.b))))
      } else constraints
      ConstraintSolver.solveAs(presented.map(_.id), orderedConstraints, presentation, members.flatMap(node => filmOf(node.id).map(node.id -> _)).toMap)
        .map(_.map(nodeById))
    }

    val ownFilms: Map[String, Int] = families.bestOf.map { case (id, (scored, _)) => id -> scored.candidate.tmdbId }
    // Nodes one chain lists under ONE catalogue id that accepted different films, each on its own
    // evidence: neither acceptance stands, so the id's must-link unites them and the cluster votes on
    // its pooled evidence. Showcase's "9 to 5 (2026)" credits Colin Higgins at two venues; its two
    // bare venues, dated only by the re-release, had taken a two-minute 2026 short.
    val contested: Set[String] =
      nodes.flatMap(node => node.listings.flatMap(_.catalogueIds).distinct.map(_ -> node.id)).groupMap(_._1)(_._2).values
        .flatMap(ids => if (ids.distinct.flatMap(ownFilms.get).distinct.sizeIs > 1) ids else Nil).toSet
    val acceptedAll: Map[String, Int] = ownFilms.filterNot { case (id, _) => contested(id) } ++ families.pinnedFilm
    // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
    // edge between two families means the scoping would silently drop it, so the resolve stops.
    val roundAEdges = edges.of(nodes, acceptedAll.get)
    val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(edge => FamilyClosure.Edge(edge.a, edge.b, edge.must, edge.reason)))
    if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
    val roundAByFamily = roundAEdges.groupBy(edge => familyOf(edge.a))

    val perFamily = nodes.groupBy(node => familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members0) =>
      val members = members0.sortBy(_.id)
      val scope   = scopes(family)
      val accepted: Map[String, Int] = members.flatMap(node => acceptedAll.get(node.id).map(node.id -> _)).toMap

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil), accepted.get)
      // Group-level voting over the clusters no member matched alone.
      val unaccepted = roundA.filter(_.forall(node => !accepted.contains(node.id)))
      // A facts-free cluster in a split title family follows the family's clear majority
      // (`ClusterVoting.familyMajority`); every other cluster votes on its pooled evidence.
      val familyTaken: Map[String, (Int, Double, String)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else {
          // The title family: the round's title must-links (same title, search form, original
          // title, segment), however the solver then split the nodes they join.
          val titleEdges = roundAByFamily.getOrElse(family, Nil).filter(edge => edge.must && ConstraintEdges.TitleTiers(edge.tier))
          unaccepted.flatMap(cluster => voting.familyMajority(cluster, members, accepted, titleEdges).toSeq.flatMap(taken => cluster.map(_.id -> taken))).toMap
        }
      val voted: Map[String, (Int, Double)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else unaccepted.flatMap(cluster => if (cluster.exists(node => familyTaken.contains(node.id))) cluster.map(node => node.id -> (familyTaken(node.id)._1, familyTaken(node.id)._2))
                                     else voting.vote(cluster, scope)).toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(voted.get(id).map(_._1))
      val familyEdges = edges.of(members, filmOf)
      val clusters    = solve(members, familyEdges, filmOf)

      val clusterIndex = clusters.zipWithIndex.flatMap { case (cluster, index) => cluster.map(_.id -> index) }.toMap
      val violations   = familyEdges.count(edge => !edge.must && clusterIndex(edge.a) == clusterIndex(edge.b))
      (familyEdges, clusters.map(decisions.of(_, scope, filmOf, accepted, voted, familyEdges, clusterIndex, familyTaken)), violations)
    }

    val decided = perFamily.flatMap(_._2).sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(
      decisions      = decided,
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(node => node.listings.map(_.key -> familyOf(node.id))).toMap,
      edges          = perFamily.flatMap(_._1),
      queries        = issued.toSeq,
      filmLookups    = if (generation.partOfCorpus) 0 else records.size,
      unknownQueries = answers.count(!_._2.isKnown),
      unknownDetails = details.count(!_._2.isKnown),
      unknownFilms   = if (generation.partOfCorpus) 0 else records.count(!_._2.isKnown),
      violations     = perFamily.map(_._3).sum,
      films          = decided.flatMap(_.film).distinct.flatMap(id => candidateOf(id).map(id -> _.film)).toMap,
      scorings       = families.scorings)
  }
}
