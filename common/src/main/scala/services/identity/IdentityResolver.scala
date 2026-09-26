package services.identity

import services.movies.{ListingConstraints, ListingKey, TitleNormalizer}

import scala.collection.mutable

/**
 * `resolve(E)`: film identity as a pure, deterministic function of a SET of listings and the
 * answers of a lookup source (docs/design/identity-resolver.md, phase 2). Production code that
 * serves nothing: its only consumers are the shadow report and, later, curation.
 *
 * Two stages, every step a function of the set:
 *
 *   A. CANDIDATE GENERATION. Each listing's own detail page merges into its [[Evidence]];
 *      listings with identical evidence are one NODE. Every node's [[CandidateQueries]] are
 *      asked — all of them, up front, in sorted order, none conditional on another's answer (A1)
 *      — and every film any answer names is looked up once. The nodes are grouped into FAMILIES,
 *      the block closure of `FamilyClosure` over their title keys and the films they match, and a
 *      family's candidate POOL is every film any member's queries named.
 *
 *   B. GLOBAL ASSIGNMENT. Every node scores every candidate of its family's pool with the
 *      calibrated [[IdentityWeights]]; a candidate `ListingConstraints` says the node's own evidence
 *      denies is not eligible. A node's best eligible candidate is ACCEPTED when its CONFIDENCE
 *      (it and no rival is the film) reaches the model's threshold. Then:
 *        1. constraint edges between nodes sharing a block key — must-links by tier (1 same
 *           accepted film, 2 same sanitised title, 3 same search form or original title) and
 *           cannot-links (different accepted films; a node denying the other's film; the two
 *           describing different films; one venue listing both apart) — solved by
 *           [[ConstraintSolver]] (cannot wins; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING: a cluster no member of which accepted a film scores the pool with
 *           its members' evidence POOLED (every title shape, the modal year, every director), so a
 *           decorated spelling, a bare listing and a sibling that published the director are
 *           judged together; the winner, if accepted, becomes every member's film;
 *        3. the constraints are re-solved with those films, and each final cluster is a
 *           [[Decision]] with its confidence and explanation.
 *
 * Every family is resolved on its own. That is sound only while no edge crosses a family, which
 * holds by construction — every edge joins two nodes sharing a block key — and is CHECKED: a
 * crossing edge fails the resolve rather than scoping it (`FamilyClosure.check`).
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

  /** A node: the listings sharing one evidence. Named by its smallest listing's sort key. */
  private final class Node(val evidence: Evidence, val listings: Seq[Listing]) {
    val id: String       = listings.head.sortKey
    val weight: Int      = listings.size
    val venues: Set[String] = listings.map(_.venue).toSet
    def label: String    = s"'${evidence.cleanTitle}'${evidence.statedYear.fold("")(y => s" [$y]")}" +
      (if (evidence.directors.nonEmpty) s" {${evidence.directors.mkString(", ")}}" else "") + s" ×$weight"
  }

  /** What a caller may watch a resolve do, for a report or a calibration dataset: every
   *  (node, candidate) score of the final node-level pass. A no-op by default. */
  trait Trace {
    def scored(family: Int, listings: Seq[ListingKey], evidence: Evidence, candidate: Candidate, signals: Signals.Values,
               probability: Double): Unit
  }
  object Trace { val none: Trace = (_, _, _, _, _, _) => () }

  /** Thrown when an edge crosses a family: a rule was added without its block key. */
  final class FamilyCrossing(message: String) extends IllegalStateException(message)

  /** `pins` are the curation's hard constraints (`ListingConstraints.pinned`): a pinned film
   *  replaces a listing's own match, a denied one is never eligible, a pinned group is must-linked
   *  above every derived tier, and a derived edge the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              weights: IdentityWeights = IdentityWeights.default, trace: Trace = Trace.none,
              pins: PinConstraints = PinConstraints(Nil)): Resolution =
    resolveWith(listings, lookups, normalizer, weights, Mutation.None, trace, pins)

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    weights: IdentityWeights, mutation: Mutation, trace: Trace = Trace.none,
                                    pins: PinConstraints = PinConstraints(Nil)): Resolution = {
    val arrival  = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
    // One listing per key, the smallest by the total order — never the first to arrive.
    val all      = listings.toSeq.sorted.distinctBy(_.key)
    val ordered  = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all

    // ── A. candidate generation ──────────────────────────────────────────────────────────
    val details = mutable.LinkedHashMap.empty[(String, String), Answer[Option[DetailFacts]]]
    def detailOf(l: Listing): Option[DetailFacts] =
      if (!lookups.hasDetail(l)) None
      else details.getOrElseUpdate((l.venue, l.page.getOrElse("")), lookups.detail(l)).toOption.flatten
    val withEvidence = ordered.map(l => l -> Evidence.of(l, detailOf(l)))
    // A pinned listing is a node of its own kind: identical evidence under different pins is not
    // one question any more.
    val nodes = withEvidence.groupBy { case (l, e) => (e.key, pins.blockKeys(l.key)) }.values.toSeq
      .map(g => new Node(g.head._2, g.map(_._1).sorted))
      .sortBy(_.id)
    val nodeById = nodes.map(n => n.id -> n).toMap

    val queriesOf: Map[String, Seq[CandidateQuery]] = nodes.map(n => n.id -> CandidateQueries.of(n.evidence, normalizer)).toMap
    val answers = mutable.HashMap.empty[CandidateQuery, Answer[Seq[Hit]]]
    val issued  = mutable.ArrayBuffer.empty[CandidateQuery]
    def ask(q: CandidateQuery): Answer[Seq[Hit]] = answers.getOrElseUpdate(q, { issued += q; lookups.candidates(q) })
    if (mutation == Mutation.LazyLookups) {
      val answeredShapes = mutable.HashSet.empty[String]
      val arrivalNodes = ordered.flatMap(l => nodes.find(_.listings.contains(l))).distinct
      arrivalNodes.foreach(n => queriesOf(n.id).foreach {
        case q @ CandidateQuery.Title(text, _) =>
          val shape = normalizer.sanitize(text)
          if (!answeredShapes(shape)) { if (ask(q).toOption.exists(_.nonEmpty)) answeredShapes += shape }
          else answers.getOrElseUpdate(q, Answer.Known(Nil))
        case q => ask(q)
      })
    } else queriesOf.values.flatten.toSeq.distinct.sorted.foreach(ask)

    def hitsOf(n: Node, pick: CandidateQuery => Boolean): Seq[Hit] =
      queriesOf(n.id).filter(pick).flatMap(q => answers(q).toOption.getOrElse(Nil))
    val isTitle: CandidateQuery => Boolean = { case _: CandidateQuery.Title => true; case _ => false }
    // Each candidate a node's own title queries named, at the best rank any of them gave it.
    val ownSearch: Map[String, Map[Int, Int]] = nodes.map { n =>
      n.id -> queriesOf(n.id).filter(isTitle).flatMap(q => answers(q).toOption.getOrElse(Nil).zipWithIndex)
        .groupMapReduce(_._1.tmdbId)(_._2)(math.min)
    }.toMap
    val ownWalk   = nodes.map(n => n.id -> hitsOf(n, q => !isTitle(q)).map(_.tmdbId).toSet).toMap
    val hitsById  = nodes.flatMap(n => hitsOf(n, _ => true)).groupBy(_.tmdbId)
    val facts     = hitsById.keys.toSeq.sorted.map(id => id -> lookups.film(id)).toMap
    val candidateById = hitsById.map { case (id, hs) => id -> Candidate.of(id, hs, facts(id).toOption.flatten) }
    val views = nodes.map(n => n.id -> Signals.View.of(n.evidence, ownSearch(n.id), ownWalk(n.id), normalizer)).toMap

    // What a node's own evidence DENIES. With learned rules in the artefact, those — evaluated
    // generically on the signals; without, the constraint model's two listing-vs-film predicates.
    val learnedListingFilm = weights.cannotLinks.exists(_.scope == "listing-film")
    val deniedMemo = mutable.HashMap.empty[(String, Int), Boolean]
    def denies(n: Node, c: Candidate): Boolean = deniedMemo.getOrElseUpdate((n.id, c.tmdbId), {
      if (pins.deniedFilms(n.listings.head.key)(c.tmdbId)) true
      else if (learnedListingFilm) {
        val s = new Signals.Scorer(views(n.id), Seq(c), Signals.NoAgreement, normalizer).of(c)
        ListingConstraints.learnedCannotLink(weights.cannotLinks, "listing-film", s.category, s.number).isDefined
      } else {
        val film = c.facts
        ListingConstraints.slotDeniesFilm(n.evidence.slot, film.slot, normalizer).isDefined ||
          ListingConstraints.landingRefused(n.evidence.constraintEvidence, film.record, normalizer).isDefined
      }
    })

    // A candidate is scored for a listing only along an EVIDENCE PATH: one of the listing's own
    // queries named it, or one of its titles relates to the listing's (exact, original, segment).
    // A film some sibling's query named under an unrelated title has nothing tying it to this
    // listing, and scoring every such pair made a large family quadratic in its pool.
    def reachable(v: Signals.View, c: Candidate): Boolean =
      v.ownSearch.contains(c.tmdbId) || v.ownWalk(c.tmdbId) || Signals.titleRelation(v, c, normalizer) != "none"

    final case class Scored(c: Candidate, p: Double, signals: Signals.Values)

    /** The best of `ranked` with its CONFIDENCE — that it is the film and no rival is,
     *  p(best) × Π(1 − p(rival)) — when that clears the threshold. Accepting on the confidence
     *  rather than the best's own probability is what keeps a listing that names two equally
     *  likely films (a bare "Lalka" beside two 2026 "Lalka"s) from picking one on its own; it
     *  joins whichever its siblings' evidence decides, or stays unmatched. */
    def confidenceOf(ranked: Seq[Scored], film: Int): Double =
      ranked.find(_.c.tmdbId == film).fold(0.0)(_.p) * ranked.filterNot(_.c.tmdbId == film).map(1 - _.p).product
    def acceptedOf(ranked: Seq[Scored]): Option[(Scored, Double)] =
      ranked.headOption.map(b => b -> confidenceOf(ranked, b.c.tmdbId)).filter(_._2 >= weights.threshold)
    def scoreAll(v: Signals.View, pool: Seq[Candidate], agreeing: Int => Signals.Agreement, eligible: Candidate => Boolean): Seq[Scored] = {
      val scorer = new Signals.Scorer(v, pool, agreeing, normalizer)
      pool.filter(c => reachable(v, c) && eligible(c)).map { c => val s = scorer.of(c); Scored(c, weights.probability(s), s) }
        .sortBy(s => (-s.p, s.c.tmdbId))
    }
    // Venue agreement in a family: score every member WITHOUT agreement; each member whose own
    // best is accepted votes for it with its listings × its confidence — so a listing its own
    // evidence cannot separate from a same-titled film barely votes, and one whose director's
    // filmography names the film votes fully. The share is a film's vote over the listings whose
    // title relates to it. A function of the family's members, so of the set.
    def agreementIn(members: Seq[Node], pool: Seq[Candidate]): Int => Signals.Agreement = {
      val votes = members.flatMap { n =>
        acceptedOf(scoreAll(views(n.id), pool, Signals.NoAgreement, c => !denies(n, c)))
          .map { case (best, confidence) => (n, best.c.tmdbId, n.weight * confidence) }
      }
      val voted  = votes.groupMapReduce(_._2)(_._3)(_ + _)
      val counts = votes.groupMapReduce(_._2)(_._1.weight)(_ + _)
      val agreement = pool.map { c =>
        val related = members.filter(n => Signals.titleRelation(views(n.id), c, normalizer) != "none").map(_.weight).sum.toDouble
        c.tmdbId -> Signals.Agreement(if (related <= 0) 0.0 else math.min(1.0, voted.getOrElse(c.tmdbId, 0.0) / related),
          counts.getOrElse(c.tmdbId, 0))
      }.toMap
      id => agreement.getOrElse(id, Signals.Agreement(0.0, 0))
    }

    def titleKeys(n: Node): Set[String] =
      FamilyClosure.blockKeys(n.evidence.cleanTitle, n.evidence.originalTitle, None, normalizer,
        segments = services.resolution.SearchTitles.candidates(n.evidence.cleanTitle, n.evidence.originalTitle)) ++
        pins.blockKeys(n.listings.head.key)
    val pinnedFilm: Map[String, Int] = nodes.flatMap(n => pins.filmOf(n.listings.head.key).map(n.id -> _)).toMap
    def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
      FamilyClosure.families(nodes.map(n => n.id -> (
        if (mutation == Mutation.NarrowFamilies) Set("t:" + normalizer.sanitize(n.evidence.cleanTitle))
        else titleKeys(n) ++ ids.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap)
    def poolOf(members: Seq[Node]): Seq[Candidate] =
      members.flatMap(m => ownSearch(m.id).keys ++ ownWalk(m.id)).distinct.sorted.map(candidateById)

    // Families: the closure over title keys and the films members accept, grown until stable —
    // a node matching a film another family's listings match joins that family, and its pool.
    var matchedIds = Map.empty[String, Set[Int]]
    var familyOf   = familiesOf(matchedIds)
    var bestOf     = Map.empty[String, Scored]
    var stable     = false
    while (!stable) {
      val byFamily = nodes.groupBy(n => familyOf(n.id))
      bestOf = byFamily.values.flatMap { members =>
        val pool     = poolOf(members)
        val agreeing = agreementIn(members, pool)
        members.flatMap(n => acceptedOf(scoreAll(views(n.id), pool, agreeing, c => !denies(n, c))).map(n.id -> _._1))
      }.toMap
      val grown = nodes.map(n => n.id -> (matchedIds.getOrElse(n.id, Set.empty[Int]) ++ bestOf.get(n.id).map(_.c.tmdbId) ++
        pinnedFilm.get(n.id))).toMap
      stable = grown == matchedIds || mutation == Mutation.NarrowFamilies
      matchedIds = grown
      familyOf = familiesOf(matchedIds)
    }
    val blockKeysOf: Map[String, Set[String]] =
      nodes.map(n => n.id -> (titleKeys(n) ++ matchedIds.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    val sanitized  = (s: String) => normalizer.sanitize(s)
    val searchForm = (s: String) => normalizer.searchQuery(s)
    def pairsSharingAKey(members: Seq[Node]): Seq[(Node, Node)] = {
      val index = members.zipWithIndex.flatMap { case (n, i) => blockKeysOf(n.id).map(_ -> i) }.groupMap(_._1)(_._2)
      index.values.iterator.flatMap { is =>
        val sorted = is.distinct.sorted
        for (x <- sorted.iterator; y <- sorted.iterator if x < y) yield (x, y)
      }.toSeq.distinct.sorted.map { case (i, j) => (members(i), members(j)) }
    }
    // One venue listing both under one title with directors crediting no common person.
    def venueListsApart(x: Node, y: Node): Boolean =
      (x.venues intersect y.venues).nonEmpty && sanitized(x.evidence.cleanTitle) == sanitized(y.evidence.cleanTitle) &&
        ListingConstraints.venueCreditsApart(x.evidence.directors, y.evidence.directors, normalizer).isDefined
    // Two listings' own evidence apart: learned listing-listing rules when the artefact has them,
    // else the constraint model's predicates.
    val learnedListingListing = weights.cannotLinks.exists(_.scope == "listing-listing")
    def listingsApart(x: Node, y: Node): Option[String] =
      if (learnedListingListing) {
        val s = Signals.between(x.evidence, y.evidence, (x.venues intersect y.venues).nonEmpty, normalizer)
        ListingConstraints.learnedCannotLink(weights.cannotLinks, "listing-listing", s.category, s.number).map(_.toString)
      } else
        Option.when(ListingConstraints.cinemasDescribeDifferentFilms(x.evidence.record(x.listings.head.cinema, normalizer),
          y.evidence.record(y.listings.head.cinema, normalizer), normalizer).isDefined)("describe-different-films")
          .orElse(Option.when(venueListsApart(x, y))("venue-lists-apart"))
          .orElse(ListingConstraints.statedYearsApart(x.evidence.statedYear, y.evidence.statedYear).map(_ => "stated-years-apart"))

    // The pins' own edges between `members`, and the derived edges they leave standing.
    val nodeOfListing: Map[ListingKey, Node] = nodes.flatMap(n => n.listings.map(_.key -> n)).toMap
    def pinEdges(members: Seq[Node], filmOf: String => Option[Int]): Seq[ResolverEdge] = {
      val here = members.map(_.id).toSet
      def edge(e: FamilyClosure.Edge[ListingKey], tier: Int) =
        Option.when(here(nodeOfListing(e.a).id) && here(nodeOfListing(e.b).id) && nodeOfListing(e.a).id != nodeOfListing(e.b).id)(
          ResolverEdge(nodeOfListing(e.a).id, nodeOfListing(e.b).id, e.must, tier, e.reason))
      (pins.mustLinks.filter(e => nodeOfListing.contains(e.a) && nodeOfListing.contains(e.b)).flatMap(edge(_, 0)) ++
        pins.cannotLinks(members.flatMap(_.listings.map(_.key)), k => filmOf(nodeOfListing(k).id)).flatMap(edge(_, 0))).distinct
    }
    def admitted(e: ResolverEdge): Boolean =
      pins.admits(FamilyClosure.Edge(nodeById(e.a).listings.head.key, nodeById(e.b).listings.head.key, e.must, e.reason))

    def edgesOf(members: Seq[Node], filmOf: String => Option[Int], denied: (Node, Int) => Boolean): Seq[ResolverEdge] =
      pinEdges(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (x, y) =>
        val (ex, ey) = (x.evidence, y.evidence)
        val (fx, fy) = (filmOf(x.id), filmOf(y.id))
        val sameFilm = fx.isDefined && fx == fy
        def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(x.id, y.id, must, tier, reason)
        val cannots = if (sameFilm) Nil else Seq(
          Option.when(fx.isDefined && fy.isDefined)("different-films"),
          Option.when(fx.exists(denied(y, _)) || fy.exists(denied(x, _)))("denies-film"),
          listingsApart(x, y)
        ).flatten
        val titleX = sanitized(ex.cleanTitle)
        val originals = (ex.originalTitle.map(sanitized) ++ ey.originalTitle.map(sanitized)).filter(_.nonEmpty).toSet
        val musts = Seq(
          Option.when(sameFilm)((1, "same-film")),
          Option.when(titleX.nonEmpty && titleX == sanitized(ey.cleanTitle))((2, "same-title")),
          Option.when(searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle))((3, "same-search-form")),
          Option.when(originals.contains(titleX) || originals.contains(sanitized(ey.cleanTitle)) ||
            (ex.originalTitle.isDefined && ex.originalTitle.map(sanitized) == ey.originalTitle.map(sanitized) && originals.nonEmpty))((3, "original-title"))
        ).flatten
        cannots.map(edge(must = false, 0, _)) ++ musts.sortBy(_._1).take(1).map { case (t, r) => edge(must = true, t, r) }
      }.filter(admitted)

    def solve(members: Seq[Node], edges: Seq[ResolverEdge]): Seq[Seq[Node]] = {
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
      ConstraintSolver.solveAs(presented.map(_.id), cs, presentation).map(_.map(nodeById))
    }

    def decide(cluster: Seq[Node], pool: Seq[Candidate], agreeing: Int => Signals.Agreement, filmOf: String => Option[Int], accepted: Map[String, Int],
               voted: Map[String, (Int, Double)], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
      val films = cluster.flatMap(n => filmOf(n.id)).distinct
      require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
      val film  = films.headOption
      val pooledView = Signals.View.pooled(cluster.map(n => views(n.id) -> n.weight))
      val scored = scoreAll(pooledView, pool, agreeing, c => cluster.forall(n => !denies(n, c)))
      // A pinned film is an admin's assertion, not an estimate.
      val confidence =
        if (film.isDefined && cluster.exists(n => pinnedFilm.contains(n.id))) 1.0
        else film.fold(scored.map(1 - _.p).product)(confidenceOf(scored, _))
      val unknown = cluster.flatMap(n => queriesOf(n.id)).distinct.count(q => !answers(q).isKnown)
      val basis =
        if (film.isDefined && cluster.exists(n => pinnedFilm.contains(n.id))) ResolverDecision.Basis.Pinned
        else if (film.isDefined && cluster.exists(n => accepted.contains(n.id))) ResolverDecision.Basis.OwnMatch
        else if (film.isDefined) ResolverDecision.Basis.PooledMatch
        else if (pool.isEmpty && unknown > 0) ResolverDecision.Basis.NoEvidence
        else ResolverDecision.Basis.NoMatch
      val here  = cluster.head.id
      val ids   = cluster.map(_.id).toSet
      val own   = cluster.flatMap(n => bestOf.get(n.id).map(s => s"${n.label}: own match ${s.c.tmdbId} at ${ResolverDecision.percent(s.p)} (${weights.why(s.signals)})"))
      val joins = edges.filter(e => e.must && ids(e.a) && ids(e.b)).groupBy(_.reason).toSeq.sortBy(_._1)
        .map { case (r, es) => s"joined by $r ×${es.size}" }
      val apart = edges.filter(e => !e.must && (ids(e.a) ^ ids(e.b))).map { e =>
        val other = if (ids(e.a)) e.b else e.a
        s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${e.reason}"
      }.distinct.sorted
      val vote  = cluster.flatMap(n => voted.get(n.id)).headOption.map { case (id, p) => s"pooled evidence of ${cluster.size} node(s) → $id at ${ResolverDecision.percent(p)}" }
      val best  = scored.headOption.filter(s => film.forall(_ != s.c.tmdbId)).map(s => s"best rejected candidate ${s.c.tmdbId} at ${ResolverDecision.percent(s.p)} (${weights.why(s.signals)})")
      val gaps  = Option.when(unknown > 0)(s"$unknown lookup(s) unanswerable")
      ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, film, confidence, basis,
        (own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ vote ++ joins ++
          apart.take(4) ++ best ++ gaps).toSeq :+ s"node $here", contradictions = apart)
    }

    val deniedIds: (Node, Int) => Boolean = (n, id) => denies(n, candidateById(id))
    val acceptedAll: Map[String, Int] = bestOf.map { case (id, s) => id -> s.c.tmdbId } ++ pinnedFilm
    // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
    // edge between two families means the scoping would silently drop it, so the resolve stops.
    val roundAEdges = edgesOf(nodes, acceptedAll.get, deniedIds)
    val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(e => FamilyClosure.Edge(e.a, e.b, e.must, e.reason)))
    if (crossings.nonEmpty) throw new FamilyCrossing(s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
    val roundAByFamily = roundAEdges.groupBy(e => familyOf(e.a))

    val finalEdges = mutable.ArrayBuffer.empty[ResolverEdge]
    val decisions  = mutable.ArrayBuffer.empty[ResolverDecision]
    var violations = 0
    nodes.groupBy(n => familyOf(n.id)).toSeq.sortBy(_._1).foreach { case (family, members0) =>
      val members = members0.sortBy(_.id)
      val pool    = poolOf(members)
      val agreeing = agreementIn(members, pool)
      if (trace ne Trace.none) members.foreach { n =>
        scoreAll(views(n.id), pool, agreeing, c => !denies(n, c))
          .foreach(s => trace.scored(family, n.listings.map(_.key), n.evidence, s.c, s.signals, s.p))
      }
      val accepted: Map[String, Int] = members.flatMap(n => acceptedAll.get(n.id).map(n.id -> _)).toMap

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil))
      // Group-level voting over the clusters no member matched alone.
      val voted: Map[String, (Int, Double)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else roundA.filter(_.forall(n => !accepted.contains(n.id))).flatMap { cluster =>
          val pooledView = Signals.View.pooled(cluster.map(n => views(n.id) -> n.weight))
          acceptedOf(scoreAll(pooledView, pool, agreeing, c => cluster.forall(n => !denies(n, c))))
            .toSeq.flatMap { case (s, confidence) => cluster.map(n => n.id -> (s.c.tmdbId, confidence)) }
        }.toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(voted.get(id).map(_._1))
      val edges    = edgesOf(members, filmOf, deniedIds)
      val clusters = solve(members, edges)
      finalEdges ++= edges

      val clusterIndex = clusters.zipWithIndex.flatMap { case (c, i) => c.map(_.id -> i) }.toMap
      violations += edges.count(e => !e.must && clusterIndex(e.a) == clusterIndex(e.b))
      clusters.foreach { cluster =>
        decisions += decide(cluster, pool, agreeing, filmOf, accepted, voted, edges, clusterIndex)
      }
    }

    Resolution(
      decisions      = decisions.toSeq.sortBy(_.members.head)(using ListingKey.ordering),
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(n => n.listings.map(_.key -> familyOf(n.id))).toMap,
      edges          = finalEdges.toSeq,
      queries        = issued.toSeq,
      filmLookups    = facts.size,
      unknownQueries = answers.count(!_._2.isKnown),
      unknownDetails = details.count(!_._2.isKnown),
      violations     = violations)
  }
}
