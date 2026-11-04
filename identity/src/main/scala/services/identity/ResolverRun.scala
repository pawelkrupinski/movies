package services.identity

import services.movies.ListingKey

import IdentityResolver.{FamilyCrossing, Mutation, Stages}

/** Stage B of a resolve — the global assignment, per family — over `stages`' candidates and families
 *  ([[IdentityResolver]]'s `run`). `pieces`: whether a member billing its cluster's film as one piece beside another thing
 *  is decided apart, as no film ([[IdentityResolver.billedAsPiece]]) — the model's own takes; not a family's verdict
 *  ([[IdentityResolver.resolveScored]]). */
private[identity] final class ResolverRun(stages: Stages, mutation: Mutation, pieces: Boolean) {
  import stages.{acceptance, families, generation, links, ordered, scoring}
  import generation.{answers, candidateOf, details, issued, nodeById, nodes, records}
  import families.{familyOf, scopes}

  // ── B. global assignment, per family ─────────────────────────────────────────────────
  private val edges     = new ConstraintEdges(scoring, families, links)
  private val voting    = new ClusterVoting(scoring, families, acceptance)
  private val decisions = new ResolverDecisions(scoring, families, acceptance)

  private def solve(members: Seq[EvidenceNode], edges: Seq[ResolverEdge], filmOf: String => Option[Int]): Seq[Seq[EvidenceNode]] = {
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

  private val ownFilms: Map[String, Int] = families.bestOf.map { case (id, (scored, _)) => id -> scored.candidate.tmdbId }
  // Nodes one chain lists under ONE catalogue id that accepted different films, each on its own
  // evidence: neither acceptance stands, so the id's must-link unites them and the cluster votes on
  // its pooled evidence. Showcase's "9 to 5 (2026)" credits Colin Higgins at two venues; its two
  // bare venues, dated only by the re-release, had taken a two-minute 2026 short.
  private val contested: Set[String] =
    nodes.flatMap(node => node.listings.flatMap(_.catalogueIds).distinct.map(id => edges.catalogueKey(id, node) -> node.id)).groupMap(_._1)(_._2).values
      .flatMap(ids => if (ids.distinct.flatMap(ownFilms.get).distinct.sizeIs > 1) ids else Nil).toSet
  private val acceptedAll: Map[String, Int] = ownFilms.filterNot { case (id, _) => contested(id) } ++ families.pinnedFilm
  // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
  // edge between two families means the scoping would silently drop it, so the resolve stops.
  private val roundAEdges = edges.of(nodes, acceptedAll.get)
  private val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(edge => FamilyClosure.Edge(edge.a, edge.b, edge.must, edge.reason)))
  if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
  private val roundAByFamily = roundAEdges.groupBy(edge => familyOf(edge.a))

  /** Is the namesake rule on ([[NamesakeAmbiguity]]): each namesake-ambiguous listing decided apart, as no film. */
  private val NamesakeRule = true
  private lazy val evidenceOf: Map[ListingKey, Evidence] = generation.withEvidence.map { case (listing, evidence) => listing.key -> evidence }.toMap

  /** The resolution: every family's decisions, and the resolve's counts. */
  def resolution: Resolution = {
    val runs      = nodes.groupBy(node => familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members0) => new FamilyRun(family, members0.sortBy(_.id)) }
    val perFamily = runs.map(_.decided)
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
      scorings       = families.scorings,
      titleRelations = families.titleRelations,
      contested      = runs.flatMap(_.contested).toMap,
      namesakeAmbiguous = runs.flatMap(_.namesakeAmbiguous).toSet)
  }

  /** One family's assignment: round A's clusters, the votes of those no member matched alone, the final clusters, and
   *  each cluster's decisions. */
  private final class FamilyRun(family: Int, members: Seq[EvidenceNode]) {
    private val scope   = scopes(family)
    private val accepted: Map[String, Int] = members.flatMap(node => acceptedAll.get(node.id).map(node.id -> _)).toMap

    private val roundA = solve(members, roundAByFamily.getOrElse(family, Nil), accepted.get)
    // Group-level voting over the clusters no member matched alone.
    private val unaccepted = roundA.filter(_.forall(node => !accepted.contains(node.id)))
    // A facts-free cluster in a split title family follows the family's clear majority
    // (`ClusterVoting.familyMajority`); every other cluster votes on its pooled evidence.
    // The title family: the round's title must-links (same title, search form, original title, segment), however the
    // solver then split the nodes they join.
    private lazy val titleEdges = roundAByFamily.getOrElse(family, Nil).filter(edge => edge.must && ConstraintEdges.TitleTiers(edge.tier))
    private val familyTaken: Map[String, (Int, Double, String)] =
      if (mutation == Mutation.NoVoting) Map.empty
      else unaccepted.flatMap(cluster => voting.familyMajority(cluster, members, accepted, titleEdges).toSeq.flatMap(taken => cluster.map(_.id -> taken))).toMap
    /** Each listing of a facts-free cluster whose title family backs two films or more by its siblings' own statements
     *  ([[ClusterVoting.ownBacked]]), with those films and their venues: report only. */
    def contested: Seq[(ListingKey, Map[Int, Set[String]])] = unaccepted.flatMap { cluster =>
      val backed = voting.ownBacked(cluster, members, accepted, titleEdges)
      if (backed.sizeIs < 2) Nil else cluster.flatMap(_.listings.map(_.key -> backed))
    }
    private val votes: Map[String, ResolverDecisions.Vote] =
      if (mutation == Mutation.NoVoting) Map.empty
      else unaccepted.flatMap { cluster =>
        if (cluster.exists(node => familyTaken.contains(node.id)))
          cluster.map(node => node.id -> { val (film, probability, why) = familyTaken(node.id); ResolverDecisions.Vote(film, probability, Some(why)) })
        else voting.vote(cluster, scope).map { case (id, (film, probability)) => id -> ResolverDecisions.Vote(film, probability, None) }
      }.toMap
    private val filmOf: String => Option[Int] = id => accepted.get(id).orElse(votes.get(id).map(_.film))
    private val familyEdges = edges.of(members, filmOf)
    private val clusters    = solve(members, familyEdges, filmOf)

    private val clusterIndex = clusters.zipWithIndex.flatMap { case (cluster, index) => cluster.map(_.id -> index) }.toMap
    private val violations   = familyEdges.count(edge => !edge.must && clusterIndex(edge.a) == clusterIndex(edge.b))
    // A member billing several works is none of them, whichever film its cluster took or voted
    // ("We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw}, joined by its
    // title to the spelling crediting Harrison alone — itself a bill): it is decided apart, as no film.
    private val bills   = members.filter(node => acceptance.billsSeveralWorks(scope.of(node))).map(_.id).toSet
    private val billing = bills ++ IdentityResolver.billingUnderBanner(members, bills, filmOf, scope.of)
    private def answered(query: CandidateQuery): Seq[Hit] = answers.get(query).flatMap(_.toOption).getOrElse(Nil)
    // a piece the corpus bills beside two whole titles or more, whatever families they fell in
    private def recurs(shape: String) = generation.context.repeatedSegment(generation.normalizer.sanitize(shape))
    private def linked(node: EvidenceNode) = scope.feedLinked(node).map(_._2)
    private val backed  = Corroboration.support(clusters, _.iterator.flatMap(node => filmOf(node.id)).nextOption(), scope.of, linked)

    def decided: (Seq[ResolverEdge], Seq[ResolverDecision], Int) = (familyEdges, clusters.flatMap(decideWithNamesakes), violations)

    // ── NAMESAKE-AMBIGUOUS listings ([[NamesakeAmbiguity]]): each node read on a scope of its own, apart from its family ──
    private val namesakes: Map[String, (Set[ListingKey], Seq[Int])] =
      if (mutation == Mutation.NoVoting) Map.empty
      else members.filterNot(node => families.pinnedFilm.contains(node.id)).flatMap { node =>
        val read = node.listings.filterNot(NamesakeAmbiguity.ownDecided(_, scoring.pins)).flatMap { listing =>
          // the listing read as a node of its own: its own text, venue and cast, never its node's other listings'
          val alone  = new EvidenceNode(evidenceOf(listing.key), Seq(listing))
          val scored = new FamilyScope(Seq(alone), scoring, acceptance, () => (), titleOnly = true).of(alone)
          // a billing of several works is none of them, as in a cluster
          val works  = if (acceptance.billsSeveralWorks(scored)) Nil else NamesakeAmbiguity.namesakes(alone, scored)
          Option.when(works.nonEmpty)(listing.key -> works)
        }
        Option.when(read.nonEmpty)(node.id -> (read.map(_._1).toSet, read.flatMap(_._2).distinct.sorted))
      }.toMap
    private val namesakeKeys: Set[ListingKey] = namesakes.values.flatMap(_._1).toSet
    def namesakeAmbiguous: Set[ListingKey] = namesakeKeys

    /** A cluster's decisions with its namesake-ambiguous listings taken out, each as no film: the rest decided as the
     *  cluster would be, its film kept. */
    private def decideWithNamesakes(cluster: Seq[EvidenceNode]): Seq[ResolverDecision] = {
      val holding = if (NamesakeRule) cluster.filter(node => namesakes.contains(node.id)) else Nil
      if (holding.isEmpty) decide(cluster)
      else {
        val film = cluster.iterator.flatMap(node => filmOf(node.id)).nextOption()
        val rest = cluster.filterNot(node => namesakes.get(node.id).exists(_._1.size == node.listings.size))
        Option.when(rest.nonEmpty)(rest).toSeq.flatMap(decide(_, id => filmOf(id).orElse(film))).flatMap(without(namesakeKeys)) ++
          holding.map(namesakeNone)
      }
    }

    // a decision with `keys` taken out of its members, none when it keeps none
    private def without(keys: Set[ListingKey])(decision: ResolverDecision): Option[ResolverDecision] =
      if (!decision.members.exists(keys)) Some(decision)
      else Option.when(decision.members.exists(key => !keys(key)))(keeping(decision, decision.members.filterNot(keys)))

    private def keeping(decision: ResolverDecision, members: Seq[ListingKey]): ResolverDecision = {
      val kept = members.toSet
      decision.copy(members = members)(decision.trace.copy(nodes = decision.trace.nodes.filter { case (key, _) => kept(key) }))
    }

    private def namesakeNone(node: EvidenceNode): ResolverDecision = {
      val (keys, works) = namesakes(node.id)
      val neither = decisions.of(Seq(node), scope, _ => None, Map.empty, Map.empty, familyEdges, clusterIndex)
      val marked  = neither.copy(explanation = NamesakeAmbiguity.why(works) +: neither.explanation, fallback = None)(neither.trace.copy(namesake = true))
      keeping(marked, marked.members.filter(keys))
    }

    /** One cluster's decisions: the cluster's own, or — where some of its members bill several works, bill its film as
     *  one piece beside another thing, or take it uncorroborated — the rest's, and each such group's as none. */
    private def decide(cluster: Seq[EvidenceNode], taking: String => Option[Int] = id => filmOf(id)): Seq[ResolverDecision] = {
      val (bills, unbilled) = cluster.partition(node => billing(node.id))
      // a member billing its cluster's film as one piece beside another thing is none of it either — unless a pin took it
      val film   = cluster.iterator.flatMap(node => taking(node.id)).nextOption()
      val pinned = cluster.exists(node => families.pinnedFilm.contains(node.id))
      val piecesOf = billedAsPieces(unbilled, film, pinned)
      val placed = unbilled.filterNot(piecesOf.contains)
      // a member nothing of its own corroborates the film by, whose take a fact or an evidenced namesake speaks against
      val weak   = film.filterNot(_ => pinned).toSeq.flatMap(Corroboration.refused(placed, _, scope.of, linked, familyEdges, backed)).toMap
      val rest   = placed.filterNot(weak.contains)
      if ((bills.isEmpty && piecesOf.isEmpty && weak.isEmpty) || film.isEmpty)
        Seq(decisions.of(cluster, scope, taking, accepted, votes, familyEdges, clusterIndex))
      else
        restDecided(rest, film) ++
          Option.when(bills.nonEmpty)(none(bills, Seq("bills several works: none of them"), DecisionTrace.BillsBoth)) ++
          Option.when(piecesOf.nonEmpty)(none(unbilled.filter(piecesOf.contains), unbilled.flatMap(piecesOf.get).distinct, DecisionTrace.BilledAsPiece)) ++
          Option.when(weak.nonEmpty)(none(placed.filter(weak.contains), placed.flatMap(weak.get).distinct, DecisionTrace.Uncorroborated))
    }

    /** The members of `unbilled` billing `film` as one piece beside another thing, or beside their screening, with why. */
    private def billedAsPieces(unbilled: Seq[EvidenceNode], film: Option[Int], pinned: Boolean): Map[EvidenceNode, String] =
      film.filterNot(_ => pinned || !pieces).toSeq.flatMap { film =>
        val scored       = unbilled.flatMap(node => scope.of(node).find(_.candidate.tmdbId == film).map(node -> _))
        val corroborated = scored.flatMap(_._2.number("venues.corroborating")).maxOption.getOrElse(0.0)
        scored.flatMap { case (node, own) => IdentityResolver.billedAsPiece(node, own, corroborated, recurs, answered).map(node -> _) }
      }.toMap ++ film.filterNot(_ => pinned || !pieces).toSeq.flatMap(_ => IdentityResolver.besideItsScreening(unbilled)).toMap

    // a cluster of bills or pieces alone takes no film its vote reaches either
    private def none(nodes: Seq[EvidenceNode], why: Seq[String], veto: String): ResolverDecision = {
      val neither = decisions.of(nodes, scope, _ => None, Map.empty, Map.empty, familyEdges, clusterIndex)
      neither.copy(explanation = why ++ neither.explanation)(neither.trace.copy(vetoed = Some(DecisionTrace.Veto(veto, None))))
    }

    /** The decision of a cluster's `rest`, its members decided apart as none of `film` taken out. A member decided apart
     *  as none of the film lends the rest nothing: when the film was the cluster's pooled vote (no member of the rest took
     *  it alone, no pin, no family majority), the rest vote again on their own evidence and keep the film only if it still
     *  wins. DE: KAP-Torgau's "TACHELES – Kurzfilmprogramm" (a bill, kinoheld page endlich-tacheles) carried Endlich
     *  Tacheles onto the bare "Kurzfilmprogramm" billings. */
    private def restDecided(rest: Seq[EvidenceNode], film: Option[Int]): Seq[ResolverDecision] = {
      val pooledOnly = rest.exists(node => votes.contains(node.id)) && rest.forall(node => !accepted.contains(node.id) &&
        !families.pinnedFilm.contains(node.id) && votes.get(node.id).forall(_.majority.isEmpty))
      val restVotes =
        if (!pooledOnly) votes
        else votes -- rest.map(_.id) ++ voting.vote(rest, scope).collect {
          case (id, (taken, probability)) if film.contains(taken) => id -> ResolverDecisions.Vote(taken, probability, None)
        }
      val restFilm: String => Option[Int] = id => accepted.get(id).orElse(restVotes.get(id).map(_.film))
      Option.when(rest.nonEmpty)(decisions.of(rest, scope, restFilm, accepted, restVotes, familyEdges, clusterIndex)).toSeq
    }
  }
}
