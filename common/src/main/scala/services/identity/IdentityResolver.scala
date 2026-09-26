package services.identity

import services.identity.IdentityMeasures.{ListingFilm, ListingListing, Measure}
import services.movies.{ListingConstraints, ListingKey, TitleNormalizer}

import scala.collection.mutable

/**
 * `resolve(E)`: film identity as a pure, deterministic function of a SET of listings and the
 * answers of a lookup source (docs/design/identity-resolver.md, phase 2). Production code that
 * serves nothing: its only consumers are the shadow report, the recording sweep and curation.
 *
 * Two stages, every step a function of the set:
 *
 *   A. CANDIDATE GENERATION. Each listing's own detail page merges into its [[Evidence]];
 *      listings with identical evidence are one NODE. Every node's [[CandidateQueries]] are
 *      asked — all of them, up front, in sorted order, none conditional on another's answer (A1)
 *      — and every film any answer names is looked up once. The nodes are grouped into FAMILIES,
 *      the block closure of `FamilyClosure` over their title keys and the films they match.
 *
 *   B. GLOBAL ASSIGNMENT. Every node scores every candidate it has an EVIDENCE PATH to (its own
 *      query named it, or a film title relates to its title) with the calibrated model
 *      ([[IdentityCalibration]] over [[IdentityMeasures]] — the same measurements the weights were
 *      fitted on, including `venues.corroborating`, the family's venue co-occurrence). A candidate
 *      a node's evidence DENIES (`ListingConstraints.learned`: a learned rule, or its OWN facts'
 *      probability below the certified cut) is not eligible. A node ACCEPTS its best candidate ALONE when the
 *      calibrated probability clears the calibration's cut AND its own facts (not TMDB's ranking)
 *      favour it over the runner-up; otherwise it follows its cluster. Then:
 *        1. constraint edges between nodes sharing a block key — must-links by tier (0 pinned, 1
 *           same accepted film, 2 same sanitised title, 3 same search form or original title, 4 one's
 *           title a delimited segment of the other's) and
 *           cannot-links (different accepted films; a node denying the other's film; the two
 *           listings' own evidence apart, `ListingConstraints.learned` on "listing-listing") —
 *           solved by [[ConstraintSolver]] (cannot wins; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING: a cluster no member of which accepted a film scores its members'
 *           evidence POOLED into one listing (the heaviest title, the modal year, every director),
 *           and the winner, if accepted, becomes every member's film;
 *        3. the constraints are re-solved with those films, and each final cluster is a
 *           [[ResolverDecision]] with its confidence, basis and explanation.
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

  /** A node: the listings sharing one evidence. Named by its smallest listing's sort key. */
  private final class Node(val evidence: Evidence, val listings: Seq[Listing]) {
    val id: String          = listings.head.sortKey
    val weight: Int         = listings.size
    val venue: String       = listings.head.venue
    val venues: Set[String] = listings.map(_.venue).toSet
    def label: String       = s"'${evidence.title}'${evidence.statedYear.fold("")(y => s" [$y]")}" +
      (if (evidence.directors.nonEmpty) s" {${evidence.directors.mkString(", ")}}" else "") + s" ×$weight"
  }

  /** Thrown when `count` edges cross a family: a rule was added without its block key. */
  final class FamilyCrossing(val count: Int, message: String) extends IllegalStateException(message)

  /** Title relations close enough that a candidate a node's own queries did not name is still
   *  scored for it (an evidence path through the title). */
  private val Reaching = Set("exact", "original", "alternative", "segment", "contains")

  /** `pins` are the curation's hard constraints (`ListingConstraints.pinned`): a pinned film
   *  replaces a listing's own match, a denied one is never eligible, a pinned group is must-linked
   *  above every derived tier, and a derived edge the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.default,
              pins: PinConstraints = PinConstraints(Nil)): Resolution =
    resolveWith(listings, lookups, normalizer, calibration, Mutation.None, pins)

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    calibration: IdentityCalibration, mutation: Mutation,
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

    val queriesOf: Map[String, Seq[CandidateQuery]] = nodes.map(n => n.id -> CandidateQueries.of(n.evidence)).toMap
    val answers = mutable.HashMap.empty[CandidateQuery, Answer[Seq[Hit]]]
    val issued  = mutable.ArrayBuffer.empty[CandidateQuery]
    def ask(q: CandidateQuery): Answer[Seq[Hit]] = answers.getOrElseUpdate(q, { issued += q; lookups.candidates(q) })
    if (mutation == Mutation.LazyLookups) {
      val answeredShapes = mutable.HashSet.empty[String]
      val arrivalNodes = ordered.flatMap(l => nodes.find(_.listings.contains(l))).distinct
      arrivalNodes.foreach(n => queriesOf(n.id).foreach {
        case q @ CandidateQuery.Title(text) =>
          val shape = normalizer.sanitize(text)
          if (!answeredShapes(shape)) { if (ask(q).toOption.exists(_.nonEmpty)) answeredShapes += shape }
          else answers.getOrElseUpdate(q, Answer.Known(Nil))
        case q => ask(q)
      })
    } else queriesOf.values.flatten.toSeq.distinct.sorted.foreach(ask)

    val isTitle: CandidateQuery => Boolean = { case _: CandidateQuery.Title => true; case _ => false }
    // Each candidate a node's own title searches named, at the best (1-based) rank any gave it.
    val ownSearch: Map[String, Map[Int, Int]] = nodes.map { n =>
      n.id -> queriesOf(n.id).filter(isTitle).flatMap(q => answers(q).toOption.getOrElse(Nil).zipWithIndex)
        .groupMapReduce(_._1.tmdbId)(_._2 + 1)(math.min)
    }.toMap
    val ownWalk: Map[String, Set[Int]] = nodes.map(n =>
      n.id -> queriesOf(n.id).filterNot(isTitle).flatMap(q => answers(q).toOption.getOrElse(Nil)).map(_.tmdbId).toSet).toMap
    val hitsById = nodes.flatMap(n => queriesOf(n.id).flatMap(q => answers(q).toOption.getOrElse(Nil))).groupBy(_.tmdbId)
    val records  = hitsById.keys.toSeq.sorted.map(id => id -> lookups.film(id)).toMap
    val candidateById: Map[Int, Candidate] = hitsById.map { case (id, hs) => id -> Candidate.of(id, hs, records(id).toOption.flatten) }

    // ── scoring ──────────────────────────────────────────────────────────────────────────
    final case class Scored(c: Candidate, p: Double, measures: Map[String, Measure], denied: Boolean)

    /** What the LISTING'S OWN facts contribute — the title, year, director, runtime, original
     *  title and country measures — as opposed to the film database's ranking priors (search rank,
     *  popularity, rivals) and the family's pooled count (`venues.corroborating`). */
    val Priors = IdentityMeasures.RankingPriors + "venues.corroborating"
    def ownContributions(measures: Map[String, Measure]): Double =
      calibration.contributions(ListingFilm, measures).collect { case (name, w) if !Priors(name) => w }.sum
    /** The calibrated probability on the listing's own facts alone — what a cannot-link reads. A
     *  film the listing's facts do not contradict is never vetoed merely for ranking second in
     *  TMDB's search or for having same-titled rivals: that is ambiguity, not evidence of a
     *  different film. */
    def factsProbability(measures: Map[String, Measure]): Double =
      calibration.scopes(ListingFilm).calibration(calibration.scopes(ListingFilm).prior + ownContributions(measures))

    /** The listings of a family by title key, with their venues: `venues.corroborating`'s group. */
    final class FamilyScope(members: Seq[Node]) {
      val pool: Seq[Candidate] = members.flatMap(m => ownSearch(m.id).keys ++ ownWalk(m.id)).distinct.sorted.map(candidateById)
      private val groups: Map[String, Seq[(String, IdentityMeasures.Listing)]] =
        members.flatMap(n => n.listings.map(l => IdentityMeasures.key(n.evidence.title) -> (l.venue -> n.evidence.measured)))
          .groupMap(_._1)(_._2)

      /** Every candidate `l` has an evidence path to, scored; `denies` marks the ones its own
       *  evidence rules out (`ListingConstraints.learned`), which are never eligible. */
      def score(l: IdentityMeasures.Listing, venue: String, ranks: Map[Int, Int], walked: Set[Int],
                deniedByPins: Int => Boolean): Seq[Scored] = {
        val relation  = pool.map(c => c.tmdbId -> IdentityMeasures.titleRelation(l, c.film).value).toMap
        val reachable = pool.filter(c => ranks.contains(c.tmdbId) || walked(c.tmdbId) || Reaching(relation(c.tmdbId)))
        val close     = reachable.count(c => IdentityMeasures.Rivalling(relation(c.tmdbId)))
        val group     = groups.getOrElse(IdentityMeasures.key(l.title), Nil)
        reachable.map { c =>
          val rivals   = close - (if (IdentityMeasures.Rivalling(relation(c.tmdbId))) 1 else 0)
          val measures = IdentityMeasures.listingFilm(l, c.film, ranks.get(c.tmdbId), rivals,
            IdentityMeasures.corroboratingVenues(c.film, group, venue))
          val p = calibration.probability(ListingFilm, measures)
          Scored(c, p, measures, deniedByPins(c.tmdbId) ||
            ListingConstraints.learned(calibration, ListingFilm, measures, factsProbability(measures)).isDefined)
        }.sortBy(s => (-s.p, s.c.tmdbId))
      }

      private val memo = mutable.HashMap.empty[String, Seq[Scored]]
      def of(n: Node): Seq[Scored] = memo.getOrElseUpdate(n.id,
        score(n.evidence.measured, n.venue, ownSearch(n.id), ownWalk(n.id), pins.deniedFilms(n.listings.head.key)))

      /** The cluster's members read as ONE listing: the title most of its listings carry (the
       *  smaller node on a tie), the year most of them state, every director and country, the
       *  median runtime, the modal original title, and every candidate any of them named. */
      def pooled(cluster: Seq[Node]): Seq[Scored] = {
        def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
          values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (v, w) => (-w, v) }.headOption.map(_._1)
        val lead     = cluster.sortBy(n => (-n.weight, n.id)).head
        val runtimes = cluster.flatMap(n => n.evidence.runtime.toSeq.flatMap(r => Seq.fill(n.weight)(r))).sorted
        val listing  = lead.evidence.measured.copy(
          year          = modal(cluster.flatMap(n => n.evidence.statedYear.map(_ -> n.weight))),
          originalTitle = modal(cluster.flatMap(n => n.evidence.originalTitle.map(_ -> n.weight))),
          directors     = cluster.flatMap(_.evidence.directors).distinct.sorted,
          runtime       = runtimes.lift(runtimes.size / 2),
          countries     = cluster.flatMap(_.evidence.countries).distinct.sorted)
        val ranks = cluster.flatMap(n => ownSearch(n.id)).groupMapReduce(_._1)(_._2)(math.min)
        score(listing, lead.venue, ranks, cluster.flatMap(n => ownWalk(n.id)).toSet,
          id => cluster.exists(n => pins.deniedFilms(n.listings.head.key)(id)))
          .map(s => if (s.denied || cluster.forall(n => !of(n).exists(o => o.c.tmdbId == s.c.tmdbId && o.denied))) s else s.copy(denied = true))
      }
    }

    /** The calibrated probability that `film` is the listing's film — the decision's confidence,
     *  on the scale the rating gate reads. Rivals are already in it (the `rivals` measure). */
    def confidenceOf(ranked: Seq[Scored], film: Int): Double =
      ranked.filterNot(_.denied).find(_.c.tmdbId == film).fold(0.0)(_.p)
    /** The best eligible candidate, when the calibration accepts it. */
    def acceptedOf(ranked: Seq[Scored]): Option[(Scored, Double)] =
      ranked.find(!_.denied).map(b => b -> b.p).filter(x => calibration.showsRatings(x._2))

    def ownEvidence(s: Scored): Double = ownContributions(s.measures)
    /** A node accepts a film ON ITS OWN only when its own facts favour it over the runner-up: a
     *  bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not
     *  decided alone — it follows the film its title's credited siblings chose (the cluster's), or
     *  the pooled vote. */
    def acceptedAlone(ranked: Seq[Scored]): Option[(Scored, Double)] = {
      val eligible = ranked.filterNot(_.denied)
      acceptedOf(ranked).filter { case (best, _) => eligible.lift(1).forall(r => ownEvidence(best) > ownEvidence(r)) }
    }

    // ── families ─────────────────────────────────────────────────────────────────────────
    def titleKeys(n: Node): Set[String] =
      FamilyClosure.blockKeys(n.evidence.cleanTitle, n.evidence.originalTitle, None, normalizer,
        segments = IdentityMeasures.titleShapes(n.evidence.measured) :+ n.evidence.cleanTitle) ++
        pins.blockKeys(n.listings.head.key)
    val pinnedFilm: Map[String, Int] = nodes.flatMap(n => pins.filmOf(n.listings.head.key).map(n.id -> _)).toMap
    def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
      FamilyClosure.families(nodes.map(n => n.id -> (
        if (mutation == Mutation.NarrowFamilies) Set("t:" + normalizer.sanitize(n.evidence.cleanTitle))
        else titleKeys(n) ++ ids.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap)

    // Families: the closure over title keys and the films members accept, grown until stable —
    // a node matching a film another family's listings match joins that family, and its pool.
    var matchedIds = Map.empty[String, Set[Int]]
    var familyOf   = familiesOf(matchedIds)
    var scopes     = Map.empty[Int, FamilyScope]
    var bestOf     = Map.empty[String, (Scored, Double)]
    var stable     = false
    while (!stable) {
      scopes = nodes.groupBy(n => familyOf(n.id)).map { case (f, ms) => f -> new FamilyScope(ms.sortBy(_.id)) }
      bestOf = nodes.flatMap(n => acceptedAlone(scopes(familyOf(n.id)).of(n)).map(n.id -> _)).toMap
      val grown = nodes.map(n => n.id -> (matchedIds.getOrElse(n.id, Set.empty[Int]) ++ bestOf.get(n.id).map(_._1.c.tmdbId) ++
        pinnedFilm.get(n.id))).toMap
      stable = grown == matchedIds || mutation == Mutation.NarrowFamilies
      matchedIds = grown
      if (!stable) familyOf = familiesOf(matchedIds)
    }
    val blockKeysOf: Map[String, Set[String]] =
      nodes.map(n => n.id -> (titleKeys(n) ++ matchedIds.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap
    def scopeOf(n: Node): FamilyScope = scopes(familyOf(n.id))
    def denies(n: Node, film: Int): Boolean =
      scopeOf(n).of(n).find(_.c.tmdbId == film).fold(pins.deniedFilms(n.listings.head.key)(film) || {
        // A film this node has no evidence path to: its own evidence against the film's record.
        candidateById.get(film).exists { c =>
          val m = IdentityMeasures.listingFilm(n.evidence.measured, c.film, None, 0, 0)
          ListingConstraints.learned(calibration, ListingFilm, m, factsProbability(m)).isDefined
        }
      })(_.denied)

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
    // The two listings' own evidence apart (the learned "listing-listing" scope).
    def listingsApart(x: Node, y: Node): Option[String] = {
      val m = IdentityMeasures.listingListing(x.evidence.measured, y.evidence.measured,
        sameVenue = (x.venues intersect y.venues).nonEmpty, sharedChainId = None)
      ListingConstraints.learned(calibration, ListingListing, m, calibration.probability(ListingListing, m)).map(_.toString)
    }

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

    val segmentsOf: Map[String, Set[String]] = nodes.map(n => n.id ->
      (IdentityMeasures.titleShapes(n.evidence.measured).map(searchForm).toSet - searchForm(n.evidence.cleanTitle)).filter(_.nonEmpty)).toMap
    def segmentOf(whole: Node, decorated: Node): Boolean =
      segmentsOf(decorated.id).contains(searchForm(whole.evidence.cleanTitle))

    def edgesOf(members: Seq[Node], filmOf: String => Option[Int]): Seq[ResolverEdge] =
      pinEdges(members, filmOf) ++ pairsSharingAKey(members).flatMap { case (x, y) =>
        val (ex, ey) = (x.evidence, y.evidence)
        val (fx, fy) = (filmOf(x.id), filmOf(y.id))
        val sameFilm = fx.isDefined && fx == fy
        def edge(must: Boolean, tier: Int, reason: String) = ResolverEdge(x.id, y.id, must, tier, reason)
        val cannots = if (sameFilm) Nil else Seq(
          Option.when(fx.isDefined && fy.isDefined)("different-films"),
          Option.when(fx.exists(denies(y, _)) || fy.exists(denies(x, _)))("denies-film"),
          listingsApart(x, y)
        ).flatten
        val titleX = sanitized(ex.cleanTitle)
        val originals = (ex.originalTitle.map(sanitized) ++ ey.originalTitle.map(sanitized)).filter(_.nonEmpty).toSet
        val musts = Seq(
          Option.when(sameFilm)((1, "same-film")),
          Option.when(titleX.nonEmpty && titleX == sanitized(ey.cleanTitle))((2, "same-title")),
          Option.when(searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle))((3, "same-search-form")),
          Option.when(originals.contains(titleX) || originals.contains(sanitized(ey.cleanTitle)) ||
            (ex.originalTitle.isDefined && ex.originalTitle.map(sanitized) == ey.originalTitle.map(sanitized) && originals.nonEmpty))((3, "original-title")),
          // One listing's whole title is a delimited SEGMENT of the other's ("Oficjalna premiera:
          // Lalka" and "Lalka"): the decorated spelling joins its plain sibling's cluster, so group
          // voting and the venue signal reach it. Whole segments only — "Zärtlich kreist die Faust"
          // has no delimiter before "Faust" — and the ambiguity rule leaves a spelling whose segment
          // names two films apart.
          Option.when(segmentOf(x, y) || segmentOf(y, x))((4, "title-segment"))
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

    def decide(cluster: Seq[Node], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
               voted: Map[String, (Int, Double)], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int]): ResolverDecision = {
      val films = cluster.flatMap(n => filmOf(n.id)).distinct
      require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
      val film    = films.headOption
      val pinned  = film.isDefined && cluster.exists(n => pinnedFilm.contains(n.id))
      val scored  = scope.pooled(cluster)
      val eligible = scored.filterNot(_.denied)
      val confidence =
        if (pinned) 1.0
        else film.fold(eligible.map(1 - _.p).product)(confidenceOf(scored, _))
      val unknown = cluster.flatMap(n => queriesOf(n.id)).distinct.count(q => !answers(q).isKnown)
      val basis =
        if (pinned) ResolverDecision.Basis.Pinned
        else if (film.isDefined && cluster.exists(n => accepted.contains(n.id))) ResolverDecision.Basis.OwnMatch
        else if (film.isDefined) ResolverDecision.Basis.PooledMatch
        else if (scored.isEmpty) (if (unknown > 0) ResolverDecision.Basis.NoEvidence else ResolverDecision.Basis.NoCandidate)
        else if (scored.head.denied) ResolverDecision.Basis.Vetoed
        else ResolverDecision.Basis.BelowThreshold
      val ids   = cluster.map(_.id).toSet
      val own   = cluster.flatMap(n => bestOf.get(n.id).map { case (s, c) =>
        s"${n.label}: own match ${s.c.tmdbId} at ${ResolverDecision.percent(c)} (${calibration.explain(ListingFilm, s.measures)})" })
      val joins = edges.filter(e => e.must && ids(e.a) && ids(e.b)).groupBy(_.reason).toSeq.sortBy(_._1)
        .map { case (r, es) => s"joined by $r ×${es.size}" }
      val apart = edges.filter(e => !e.must && (ids(e.a) ^ ids(e.b))).map { e =>
        val other = if (ids(e.a)) e.b else e.a
        s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${e.reason}"
      }.distinct.sorted
      val vote  = cluster.flatMap(n => voted.get(n.id)).headOption.map { case (id, p) => s"pooled evidence of ${cluster.size} node(s) → $id at ${ResolverDecision.percent(p)}" }
      val best  = scored.headOption.filter(s => film.forall(_ != s.c.tmdbId)).map(s =>
        s"best ${if (s.denied) "vetoed" else "rejected"} candidate ${s.c.tmdbId} at ${ResolverDecision.percent(s.p)} (${calibration.explain(ListingFilm, s.measures)})")
      val gaps  = Option.when(unknown > 0)(s"$unknown lookup(s) unanswerable")
      ResolverDecision(cluster.flatMap(_.listings.map(_.key)).sorted, film, confidence, basis,
        (own.take(4) ++ Option.when(own.size > 4)(s"… ${own.size - 4} more own match(es)") ++ vote ++ joins ++
          apart.take(4) ++ best ++ gaps).toSeq :+ s"node ${cluster.head.listings.head.key}", contradictions = apart)
    }

    val acceptedAll: Map[String, Int] = bestOf.map { case (id, (s, _)) => id -> s.c.tmdbId } ++ pinnedFilm
    // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
    // edge between two families means the scoping would silently drop it, so the resolve stops.
    val roundAEdges = edgesOf(nodes, acceptedAll.get)
    val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(e => FamilyClosure.Edge(e.a, e.b, e.must, e.reason)))
    if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
    val roundAByFamily = roundAEdges.groupBy(e => familyOf(e.a))

    val finalEdges = mutable.ArrayBuffer.empty[ResolverEdge]
    val decisions  = mutable.ArrayBuffer.empty[ResolverDecision]
    var violations = 0
    nodes.groupBy(n => familyOf(n.id)).toSeq.sortBy(_._1).foreach { case (family, members0) =>
      val members = members0.sortBy(_.id)
      val scope   = scopes(family)
      val accepted: Map[String, Int] = members.flatMap(n => acceptedAll.get(n.id).map(n.id -> _)).toMap

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil))
      // Group-level voting over the clusters no member matched alone.
      val voted: Map[String, (Int, Double)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else roundA.filter(_.forall(n => !accepted.contains(n.id))).flatMap { cluster =>
          acceptedOf(scope.pooled(cluster)).toSeq.flatMap { case (s, confidence) => cluster.map(n => n.id -> (s.c.tmdbId, confidence)) }
        }.toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(voted.get(id).map(_._1))
      val edges    = edgesOf(members, filmOf)
      val clusters = solve(members, edges)
      finalEdges ++= edges

      val clusterIndex = clusters.zipWithIndex.flatMap { case (c, i) => c.map(_.id -> i) }.toMap
      violations += edges.count(e => !e.must && clusterIndex(e.a) == clusterIndex(e.b))
      clusters.foreach(cluster => decisions += decide(cluster, scope, filmOf, accepted, voted, edges, clusterIndex))
    }

    val decided = decisions.toSeq.sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(
      decisions      = decided,
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(n => n.listings.map(_.key -> familyOf(n.id))).toMap,
      edges          = finalEdges.toSeq,
      queries        = issued.toSeq,
      filmLookups    = records.size,
      unknownQueries = answers.count(!_._2.isKnown),
      unknownDetails = details.count(!_._2.isKnown),
      unknownFilms   = records.count(!_._2.isKnown),
      violations     = violations,
      films          = decided.flatMap(_.film).distinct.flatMap(id => candidateById.get(id).map(id -> _.film)).toMap)
  }
}
