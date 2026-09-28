package services.identity

import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.mutable

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

  /** Every question a resolve of `listings` asks — each detail page, each node's candidate
   *  queries, each named film's record — each once, `chunk` listings at a time: for a caller that
   *  wants the ASKING, not the resolution (the shadow fill, which files the answers). A resolve
   *  holds every answer and record of the corpus until it returns; this holds one chunk's. A
   *  node's questions are its own evidence's and a film lookup is its hits', so the chunks ask
   *  exactly the resolve's questions; a query or film an earlier chunk asked answers `Unknown`
   *  here, the film lookups it led to asked with it. A detail page is asked again when two chunks
   *  share it: it is evidence, and an `Unknown` would change the questions its listing asks. */
  def askAll(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, chunk: Int,
             decorations: TitleDecorations = TitleDecorations.resolver): Unit = {
    val once = new AskedOnce(lookups)
    listings.toSeq.sorted.distinctBy(_.key).grouped(chunk).foreach(part =>
      new CandidateGeneration(part, once, normalizer, PinConstraints(Nil), decorations, lazyLookups = false))
  }

  /** `lookups`, remembering only WHICH candidate queries and films it asked, never the answers. */
  private final class AskedOnce(lookups: IdentityLookups) extends IdentityLookups {
    private val queries = mutable.HashSet.empty[CandidateQuery]
    private val films   = mutable.HashSet.empty[Int]
    def hasDetail(listing: Listing): Boolean = lookups.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]] = lookups.detail(listing)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = if (queries.add(query)) lookups.candidates(query) else Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = if (films.add(tmdbId)) lookups.film(tmdbId) else Answer.Unknown
  }

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    calibration: IdentityCalibration, mutation: Mutation,
                                    pins: PinConstraints = PinConstraints(Nil),
                                    decorations: TitleDecorations = TitleDecorations.None): Resolution = {
    val arrival  = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
    // One listing per key, the smallest by the total order — never the first to arrive.
    val all      = listings.toSeq.sorted.distinctBy(_.key)
    val ordered  = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all

    // ── A. candidate generation, scoring and families ────────────────────────────────────
    val generation = new CandidateGeneration(ordered, lookups, normalizer, pins, decorations, lazyLookups = mutation == Mutation.LazyLookups)
    import generation.{answers, candidateById, details, issued, nodeById, nodes, records}
    val acceptance = new Acceptance(calibration)
    val scoring    = new CandidateScoring(generation, calibration, acceptance.weights, pins)
    val links      = new TitleLinks(nodes, normalizer, pins)
    val families   = new Families(scoring, acceptance, links, normalizer, narrow = mutation == Mutation.NarrowFamilies)
    import families.{familyOf, scopes}

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    val edges     = new ConstraintEdges(scoring, families, links)
    val voting    = new ClusterVoting(scoring, families, acceptance)
    val decisions = new ResolverDecisions(scoring, families, acceptance)

    def solve(members: Seq[EvidenceNode], edges: Seq[ResolverEdge], filmOf: String => Option[Int]): Seq[Seq[EvidenceNode]] = {
      val constraints = edges.map(e => ConstraintSolver.Constraint(e.a, e.b, e.must, e.tier, e.reason))
      val presentation = if (mutation == Mutation.FirstWins) ConstraintSolver.Presentation.AsGiven else ConstraintSolver.Presentation.Canonical
      val presented = if (mutation == Mutation.FirstWins) {
        val position = ordered.zipWithIndex.map { case (l, i) => l.sortKey -> i }.toMap
        members.sortBy(n => n.listings.map(l => position(l.sortKey)).min)
      } else members
      val cs = if (mutation == Mutation.FirstWins) {
        val position = presented.zipWithIndex.map { case (n, i) => n.id -> i }.toMap
        constraints.sortBy(c => (math.min(position(c.a), position(c.b)), math.max(position(c.a), position(c.b))))
      } else constraints
      ConstraintSolver.solveAs(presented.map(_.id), cs, presentation, members.flatMap(n => filmOf(n.id).map(n.id -> _)).toMap)
        .map(_.map(nodeById))
    }

    val acceptedAll: Map[String, Int] = families.bestOf.map { case (id, (s, _)) => id -> s.c.tmdbId } ++ families.pinnedFilm
    // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
    // edge between two families means the scoping would silently drop it, so the resolve stops.
    val roundAEdges = edges.of(nodes, acceptedAll.get)
    val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(e => FamilyClosure.Edge(e.a, e.b, e.must, e.reason)))
    if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
    val roundAByFamily = roundAEdges.groupBy(e => familyOf(e.a))

    val perFamily = nodes.groupBy(n => familyOf(n.id)).toSeq.sortBy(_._1).map { case (family, members0) =>
      val members = members0.sortBy(_.id)
      val scope   = scopes(family)
      val accepted: Map[String, Int] = members.flatMap(n => acceptedAll.get(n.id).map(n.id -> _)).toMap

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil), accepted.get)
      // Group-level voting over the clusters no member matched alone.
      val unaccepted = roundA.filter(_.forall(n => !accepted.contains(n.id)))
      // A facts-free cluster in a split title family follows the family's clear majority
      // (`ClusterVoting.familyMajority`); every other cluster votes on its pooled evidence.
      val familyTaken: Map[String, (Int, Double, String)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else {
          // The title family: the round's title must-links (same title, search form, original
          // title, segment), however the solver then split the nodes they join.
          val titleEdges = roundAByFamily.getOrElse(family, Nil).filter(e => e.must && ConstraintEdges.TitleTiers(e.tier))
          unaccepted.flatMap(c => voting.familyMajority(c, members, accepted, titleEdges).toSeq.flatMap(t => c.map(_.id -> t))).toMap
        }
      val voted: Map[String, (Int, Double)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else unaccepted.flatMap(c => if (c.exists(n => familyTaken.contains(n.id))) c.map(n => n.id -> (familyTaken(n.id)._1, familyTaken(n.id)._2))
                                     else voting.vote(c, scope)).toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(voted.get(id).map(_._1))
      val familyEdges = edges.of(members, filmOf)
      val clusters    = solve(members, familyEdges, filmOf)

      val clusterIndex = clusters.zipWithIndex.flatMap { case (c, i) => c.map(_.id -> i) }.toMap
      val violations   = familyEdges.count(e => !e.must && clusterIndex(e.a) == clusterIndex(e.b))
      (familyEdges, clusters.map(decisions.of(_, scope, filmOf, accepted, voted, familyEdges, clusterIndex, familyTaken)), violations)
    }

    val decided = perFamily.flatMap(_._2).sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(
      decisions      = decided,
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(n => n.listings.map(_.key -> familyOf(n.id))).toMap,
      edges          = perFamily.flatMap(_._1),
      queries        = issued.toSeq,
      filmLookups    = records.size,
      unknownQueries = answers.count(!_._2.isKnown),
      unknownDetails = details.count(!_._2.isKnown),
      unknownFilms   = records.count(!_._2.isKnown),
      violations     = perFamily.map(_._3).sum,
      films          = decided.flatMap(_.film).distinct.flatMap(id => candidateById.get(id).map(id -> _.film)).toMap)
  }
}
