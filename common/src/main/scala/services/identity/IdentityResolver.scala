package services.identity

import services.identity.IdentityMeasures.ListingFilm
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
 *      a node's evidence DENIES (`ListingConstraints.learnedListingFilm`: a learned rule, or its OWN
 *      facts' probability below the certified cut, and only when the node publishes a fact the film
 *      can be compared on — a title relation alone is scored, never a veto; or a film of the node's
 *      credited director its title does not name, when its title names another of that director's)
 *      is not eligible. A node ACCEPTS its best candidate ALONE when the
 *      calibrated probability clears the calibration's cut AND its own facts (not TMDB's ranking)
 *      favour it over the runner-up; otherwise it follows its cluster. Then:
 *        1. constraint edges between nodes sharing a block key — must-links by tier (0 pinned, 1
 *           same accepted film, 2 same sanitised title, 3 same search form — unless either title names
 *           a film or a listing's title beside it — or original title, 4 one's title a delimited
 *           segment of the other's, unless the rest of the other's names a film of its own) and
 *           cannot-links (different accepted films; a node denying the other's film; the two
 *           listings' own evidence apart, `ListingConstraints.learnedListingListing`: a learned rule
 *           or the "listing-listing" cut, only when the two compare a fact both published) —
 *           solved by [[ConstraintSolver]] (cannot wins, and no component holds two films however
 *           the must-links chain; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING: a cluster no member of which accepted a film scores its members'
 *           evidence POOLED into one listing (the heaviest title, the modal year, every director),
 *           and the winner, if accepted — and no rival the title names by the same pieces fits the
 *           pooled facts better, nor is it one of two films the title names by disjoint pieces
 *           (`Acceptance.pooled`) — becomes every member's film. A winner some members' own
 *           evidence denies is not a veto of the whole cluster: those members split off and the
 *           rest take it when their own pooled facts carry it (`vote`). A winner no member's title
 *           names — only a credited director's filmography reached it — must also be the one
 *           film the pooled facts and the calibration both rank first (`votedFor`);
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

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                    calibration: IdentityCalibration, mutation: Mutation,
                                    pins: PinConstraints = PinConstraints(Nil),
                                    decorations: TitleDecorations = TitleDecorations.None): Resolution = {
    val arrival  = mutation == Mutation.LazyLookups || mutation == Mutation.FirstWins
    // One listing per key, the smallest by the total order — never the first to arrive.
    val all      = listings.toSeq.sorted.distinctBy(_.key)
    val ordered  = if (arrival) listings.toSeq.distinctBy(_.sortKey).filter(all.toSet) else all

    // ── A. candidate generation ──────────────────────────────────────────────────────────
    val generation = new CandidateGeneration(ordered, lookups, normalizer, pins, decorations, lazyLookups = mutation == Mutation.LazyLookups)
    import generation.{answers, candidateById, details, issued, nodeById, nodes, ownSearch, queriesOf, records}
    // ── scoring ──────────────────────────────────────────────────────────────────────────
    val acceptance = new Acceptance(calibration)
    val weights    = acceptance.weights

    val scoring = new CandidateScoring(generation, calibration, weights, pins)
    import scoring.{evidenceDenies, houses}


    /** Does `n`'s own title evidence name `film`: its title searches returned it, or its title (a
     *  whole spelling, its original title or a segment) names the film's and not another
     *  instalment of its series (`IdentityMeasures.namesFilm`)? */
    def titleNames(n: EvidenceNode, film: Candidate): Boolean =
      ownSearch(n.id).contains(film.tmdbId) || IdentityMeasures.namesFilm(n.evidence.measured, film.film)
    /** The group vote over a cluster's POOLED scoring: the accepted film — but a film no member's
     *  title names, which only a credited director's filmography reached, only when nothing else
     *  the walk reached fits the pooled facts as well: every rival's own facts fit worse or equally,
     *  and the calibration rates it strictly lower. A walk is a path to candidates; it cannot pick
     *  among a director's films the listing's facts favour another of (a lecture on "Trzy kolory:
     *  Niebieski" is not "Czerwony"), or that the calibration cannot tell apart. */
    def votedFor(cluster: Seq[EvidenceNode], ranked: Seq[Scored]): Option[(Scored, Double)] =
      acceptance.pooled(ranked).filter { case (s, _) =>
        cluster.exists(titleNames(_, s.c)) ||
          ranked.filterNot(r => r.denied || (r eq s)).forall(r => r.p < s.p && weights.own(r) <= weights.own(s))
      }



    // ── families ─────────────────────────────────────────────────────────────────────────
    def titleKeys(n: EvidenceNode): Set[String] =
      FamilyClosure.blockKeys(n.evidence.cleanTitle, n.evidence.originalTitle, None, normalizer,
        segments = IdentityMeasures.titleShapes(n.evidence.published) :+ n.evidence.cleanTitle) ++
        pins.blockKeys(n.listings.head.key)
    val pinnedFilm: Map[String, Int] = nodes.flatMap(n => pins.filmOf(n.listings.head.key).map(n.id -> _)).toMap
    def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
      FamilyClosure.families(nodes.map(n => n.id -> (
        if (mutation == Mutation.NarrowFamilies) Set("t:" + normalizer.sanitize(n.evidence.cleanTitle))
        else titleKeys(n) ++ ids.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap)

    val sanitized  = (s: String) => normalizer.sanitize(s)
    val searchForm = (s: String) => normalizer.searchQuery(s)
    val segmentsOf: Map[String, Set[String]] = nodes.map(n => n.id ->
      (IdentityMeasures.titleShapes(n.evidence.published).map(sanitized).toSet - sanitized(n.evidence.cleanTitle)).filter(_.nonEmpty)).toMap
    def segmentOf(whole: EvidenceNode, decorated: EvidenceNode): Boolean =
      segmentsOf(decorated.id).contains(sanitized(whole.evidence.cleanTitle))
    /** Do two nodes' titles must-link them (tiers 2–4: same sanitised title, same search form,
     *  an original title naming the other, or one a whole segment of the other)? */
    def titleLinked(x: EvidenceNode, y: EvidenceNode): Boolean = {
      val (ex, ey) = (x.evidence, y.evidence)
      val originals = (ex.originalTitle ++ ey.originalTitle).map(sanitized).filter(_.nonEmpty).toSet
      (sanitized(ex.cleanTitle).nonEmpty && sanitized(ex.cleanTitle) == sanitized(ey.cleanTitle)) ||
        (searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle)) ||
        originals.contains(sanitized(ex.cleanTitle)) || originals.contains(sanitized(ey.cleanTitle)) ||
        segmentOf(x, y) || segmentOf(y, x)
    }

    /** A node's ALONE acceptance, withdrawn when a title-linked sibling that accepted nothing itself
     *  DENIES the film: a bare "Samson i Dalila" beside the same venue family's "…: live in hd
     *  2026/27" has no evidence of its own against DeMille's 1949 film, but its sibling's season
     *  is. The node then goes to the group vote with its siblings, where every member's denial holds. */
    def withoutSiblingDenials(members: Seq[EvidenceNode], scope: FamilyScope,
                              alone: Map[String, (Scored, Double)]): Map[String, (Scored, Double)] =
      alone.filter { case (id, (best, _)) =>
        val n = nodeById(id)
        !members.exists(y => y.id != id && !alone.contains(y.id) && titleLinked(n, y) &&
          scope.of(y).exists(o => o.c.tmdbId == best.c.tmdbId && o.denied))
      }

    // Families: the closure over title keys and the films members accept, grown until stable —
    // a node matching a film another family's listings match joins that family, and its pool.
    var matchedIds = Map.empty[String, Set[Int]]
    var familyOf   = familiesOf(matchedIds)
    var scopes     = Map.empty[Int, FamilyScope]
    var bestOf     = Map.empty[String, (Scored, Double)]
    var stable     = false
    while (!stable) {
      scopes = nodes.groupBy(n => familyOf(n.id)).map { case (f, ms) => f -> new FamilyScope(ms.sortBy(_.id), scoring) }
      bestOf = nodes.groupBy(n => familyOf(n.id)).toSeq.flatMap { case (f, members) =>
        val scope = scopes(f)
        withoutSiblingDenials(members, scope, members.flatMap(n => acceptance.alone(scope.of(n)).map(n.id -> _)).toMap)
      }.toMap
      val grown = nodes.map(n => n.id -> (matchedIds.getOrElse(n.id, Set.empty[Int]) ++ bestOf.get(n.id).map(_._1.c.tmdbId) ++
        pinnedFilm.get(n.id))).toMap
      stable = grown == matchedIds || mutation == Mutation.NarrowFamilies
      matchedIds = grown
      if (!stable) familyOf = familiesOf(matchedIds)
    }
    val blockKeysOf: Map[String, Set[String]] =
      nodes.map(n => n.id -> (titleKeys(n) ++ matchedIds.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap
    def scopeOf(n: EvidenceNode): FamilyScope = scopes(familyOf(n.id))
    def denies(n: EvidenceNode, film: Int): Boolean =
      scopeOf(n).of(n).find(_.c.tmdbId == film).fold(pins.deniedFilms(n.listings.head.key)(film) || {
        // A film this node has no evidence path to: its own evidence against the film's record.
        candidateById.get(film).exists { c =>
          evidenceDenies(n.evidence.measured, c.film, IdentityMeasures.listingFilm(n.evidence.measured, c.film, None, 0, 0, houses, scopeOf(n).qualifiers))
        }
      })(_.denied)

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    def pairsSharingAKey(members: Seq[EvidenceNode]): Seq[(EvidenceNode, EvidenceNode)] = {
      val index = members.zipWithIndex.flatMap { case (n, i) => blockKeysOf(n.id).map(_ -> i) }.groupMap(_._1)(_._2)
      index.values.iterator.flatMap { is =>
        val sorted = is.distinct.sorted
        for (x <- sorted.iterator; y <- sorted.iterator if x < y) yield (x, y)
      }.toSeq.distinct.sorted.map { case (i, j) => (members(i), members(j)) }
    }
    // The two listings' own evidence apart: the seasons their titles name, or the learned
    // "listing-listing" scope when they compare a fact both published.
    def listingsApart(x: EvidenceNode, y: EvidenceNode): Option[String] = {
      val (a, b) = (x.evidence.published, y.evidence.published)
      lazy val m = IdentityMeasures.listingListing(a, b, sameVenue = (x.venues intersect y.venues).nonEmpty, sharedChainId = None)
      ListingConstraints.seasonsApart(a.seasonYear, b.seasonYear, b.year)
        .orElse(ListingConstraints.seasonsApart(b.seasonYear, a.seasonYear, a.year))
        .orElse(ListingConstraints.learnedListingListing(calibration, m))
        .map(_.toString)
    }

    // The pins' own edges between `members`, and the derived edges they leave standing.
    val nodeOfListing: Map[ListingKey, EvidenceNode] = nodes.flatMap(n => n.listings.map(_.key -> n)).toMap
    def pinEdges(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] = {
      val here = members.map(_.id).toSet
      def edge(e: FamilyClosure.Edge[ListingKey], tier: Int) =
        Option.when(here(nodeOfListing(e.a).id) && here(nodeOfListing(e.b).id) && nodeOfListing(e.a).id != nodeOfListing(e.b).id)(
          ResolverEdge(nodeOfListing(e.a).id, nodeOfListing(e.b).id, e.must, tier, e.reason))
      (pins.mustLinks.filter(e => nodeOfListing.contains(e.a) && nodeOfListing.contains(e.b)).flatMap(edge(_, 0)) ++
        pins.cannotLinks(members.flatMap(_.listings.map(_.key)), k => filmOf(nodeOfListing(k).id)).flatMap(edge(_, 0))).distinct
    }
    def admitted(e: ResolverEdge): Boolean =
      pins.admits(FamilyClosure.Edge(nodeById(e.a).listings.head.key, nodeById(e.b).listings.head.key, e.must, e.reason))


    /** Does the rest of `decorated`'s title name a film of its own, beside the title `whole` — a
     *  candidate it may still take whose naming pieces share no word with `whole`? "Lalka (Dolly)"
     *  carries "Lalka" whole, but its "Dolly" names Blackhurst's film: it is not merely a decorated
     *  "Lalka", and the segment must not decide between the two for it. */
    def namesBeside(decorated: EvidenceNode, whole: String): Boolean = {
      val words = services.movies.TitleContainment.tokens(whole).toSet
      scopeOf(decorated).of(decorated).exists { s =>
        val pieces = IdentityMeasures.namingPieces(decorated.evidence.measured, s.c.film)
        !s.denied && pieces.nonEmpty && pieces.forall(p => (p.toSet intersect words).isEmpty)
      }
    }
    /** Does `n`'s title carry, beside its search form `form`, a delimited segment that is ANOTHER
     *  listing's whole title sharing no word with the form? Kino Oaza's "\"Kumotry\" - film, V
     *  FESTIWAL WAPI 2026" searches as its festival's suffix, which every film of the festival shares,
     *  while its quoted segment is the title other venues list the film by: the form is then the
     *  festival's, not the film's, and says nothing about which film the spelling is. */
    val wholeTitles: Set[String] = nodes.map(n => sanitized(n.evidence.cleanTitle)).filter(_.nonEmpty).toSet
    def titlesBeside(n: EvidenceNode, form: String): Boolean = {
      val words = services.movies.TitleContainment.tokens(form).toSet
      // Segments are SANITISED (no spaces), so compare with the form sanitised too: the title's own
      // segment ("Pieśni lasu" in "Pieśni lasu | Pokaz …") is never another listing's title beside it.
      val formKey = sanitized(form)
      segmentsOf(n.id).exists(seg => wholeTitles(seg) && seg != formKey && !formKey.contains(seg) && !seg.contains(formKey) &&
        (services.movies.TitleContainment.tokens(seg).toSet intersect words).isEmpty)
    }
    /** Does `n`'s title carry another listing's title BESIDE its search form `form` ([[titlesBeside]]),
     *  so that a search form it shares with another title is no evidence the two are one film? Not
     *  "names a film beside it": a spelling whose original title reaches its OWN film ("Pieśni lasu |
     *  Pokaz …", "Whispers in the Woods") would then lose the plain listings it is the only bridge for. */
    def besideItsForm(n: EvidenceNode, form: String): Boolean = titlesBeside(n, form)

    /** The must-link tiers a title draws (`edgesOf`): same title, same search form or original
     *  title, one title a segment of the other. */
    val TitleTiers: Set[Int] = Set(2, 3, 4)

    def edgesOf(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Seq[ResolverEdge] =
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
          // Unless the form is only what the two titles share BESIDE the films they name: a
          // festival's spellings of its films all search as the festival's suffix.
          Option.when(searchForm(ex.cleanTitle).nonEmpty && searchForm(ex.cleanTitle) == searchForm(ey.cleanTitle) &&
            !besideItsForm(x, searchForm(ex.cleanTitle)) && !besideItsForm(y, searchForm(ey.cleanTitle)))((3, "same-search-form")),
          Option.when(originals.contains(titleX) || originals.contains(sanitized(ey.cleanTitle)) ||
            (ex.originalTitle.isDefined && ex.originalTitle.map(sanitized) == ey.originalTitle.map(sanitized) && originals.nonEmpty))((3, "original-title")),
          // One listing's whole title is a delimited SEGMENT of the other's ("Oficjalna premiera:
          // Lalka" and "Lalka"): the decorated spelling joins its plain sibling's cluster, so group
          // voting and the venue signal reach it. Whole segments only — "Zärtlich kreist die Faust"
          // has no delimiter before "Faust" — and the ambiguity rule leaves a spelling whose segment
          // names two films apart, as it does one whose rest names a film of its own (`namesBeside`).
          Option.when((segmentOf(x, y) && !namesBeside(y, ex.cleanTitle)) || (segmentOf(y, x) && !namesBeside(x, ey.cleanTitle)))((4, "title-segment"))
        ).flatten
        cannots.map(edge(must = false, 0, _)) ++ musts.sortBy(_._1).take(1).map { case (t, r) => edge(must = true, t, r) }
      }.filter(admitted)

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

    /** The GROUP VOTE of a cluster no member matched alone: each voting member → the film and its
     *  confidence. The pooled scoring marks a film denied when ANY member's own evidence denies it
     *  (`FamilyScope.pooled`). When that vetoes the cluster's best film but the cluster's POOLED
     *  facts (the modal year, the median runtime, every director — weighted by listings) still carry
     *  it, the denying members are split off instead — the rest vote without them, and their
     *  denial becomes a cannot-link to the film the rest take ("denies-film"), so no cluster holds both. The rest take the film only
     *  when their OWN pooled facts carry it past the calibration's cut by themselves — at least one
     *  published fact compared (`IdentityMeasures.comparesAFact`), and `factsProbability`, without
     *  the ranking priors, clearing the cut: bare siblings never outvote a member's denial on a title
     *  and the database's ranking. */
    def vote(cluster: Seq[EvidenceNode], scope: FamilyScope): Seq[(String, (Int, Double))] = {
      def to(voters: Seq[EvidenceNode], accepted: (Scored, Double)) = voters.map(n => n.id -> (accepted._1.c.tmdbId, accepted._2))
      val ranked = scope.pooled(cluster)
      votedFor(cluster, ranked).map(to(cluster, _)).getOrElse(ranked.headOption.filter(s => s.denied && weights.carriedByOwnFacts(s)).toSeq.flatMap { vetoed =>
        val rest = cluster.filterNot(n => scope.of(n).exists(o => o.c.tmdbId == vetoed.c.tmdbId && o.denied))
        Option.when(rest.nonEmpty && rest.size < cluster.size)(rest).flatMap(rest => votedFor(rest, scope.pooled(rest))
          .filter { case (s, _) => s.c.tmdbId == vetoed.c.tmdbId && weights.carriedByOwnFacts(s) }
          .map(to(rest, _))).getOrElse(Nil)
      })
    }

    /** The film a cluster of listings that publish NOTHING but their titles takes from its TITLE
     *  FAMILY — the siblings outside it a title must-link of round A joins it to (`titleEdges`,
     *  [[TitleTiers]]) that accepted a film on their own evidence — when those siblings are split
     *  across films. A bare "Sense and Sensibility" beside 836 venues' "Sense and Sensibility
     *  (2026) {Oakley}" and 9 venues' "(1995) {Ang Lee}" has no evidence of its own for either:
     *  the database's ranking prefers the older, the listings around it the current release.
     *
     *  It takes the family's majority film only when the majority is CLEAR: the one-sided 95%
     *  Wilson lower bound ([[RateBounds.lower95]]) of that film's share of the family's venues —
     *  venues, as the calibration counts units, and the cluster's own venues among them as NOT the
     *  majority's, since nothing they publish says so — clears the calibration's show-ratings cut,
     *  which is the probability it is filed at. A family too thin to outweigh the cluster decides
     *  nothing, and the cluster votes on its pooled evidence as before: 78 Cineworld venues' bare
     *  "The Omen" beside 2 venues' credited 1976 film and 4 venues' 2006 one is the 50th-anniversary
     *  re-release, and 4 of 84 is no majority. Siblings whose titles name a season do not count
     *  (Kino Amok's bare "Manon", a Met broadcast, beside 16 venues' "RBO Sezon Kinowy 2026-27:
     *  Manon"). `None` also when a member publishes a fact or the siblings hold fewer than two films. */
    def familyMajority(cluster: Seq[EvidenceNode], members: Seq[EvidenceNode], accepted: Map[String, Int],
                       titleEdges: Seq[ResolverEdge]): Option[(Int, Double, String)] =
      Option.when(!cluster.exists(_.evidence.measured.publishesAFact)) {
        val inside   = cluster.map(_.id).toSet
        val linked   = titleEdges.flatMap(e => if (inside(e.a)) Seq(e.b) else if (inside(e.b)) Seq(e.a) else Nil).toSet -- inside
        // A sibling whose title names a SEASON names a house's production of the work, which the
        // bare title does not: its venues say nothing about which house's the bare one is.
        val venuesOf = members.filter(y => linked(y.id) && accepted.contains(y.id) && y.evidence.measured.seasonYear.isEmpty)
          .groupMapReduce(y => accepted(y.id))(_.venues)(_ ++ _)
        Option.when(venuesOf.sizeIs >= 2) {
          val (film, venues) = venuesOf.toSeq.sortBy { case (f, vs) => (-vs.size, f) }.head
          val own   = cluster.flatMap(_.venues).toSet -- venues
          val total = (venuesOf.values.flatten ++ own).toSet.size
          val bound = RateBounds.lower95(venues.size, total)
          Option.when(calibration.showsRatings(bound) && !cluster.exists(denies(_, film)))(
            (film, bound, s"title family's majority film $film: ${venues.size} of $total venue(s), at least ${ResolverDecision.percent(bound)}"))
        }.flatten
      }.flatten

    def decide(cluster: Seq[EvidenceNode], scope: FamilyScope, filmOf: String => Option[Int], accepted: Map[String, Int],
               voted: Map[String, (Int, Double)], edges: Seq[ResolverEdge], clusterIndex: Map[String, Int],
               familyTaken: Map[String, (Int, Double, String)]): ResolverDecision = {
      val films = cluster.flatMap(n => filmOf(n.id)).distinct
      require(films.sizeIs <= 1, s"a cluster holds two films ${films.mkString(",")}: a cannot-link was not drawn")
      val film    = films.headOption
      val pinned  = film.isDefined && cluster.exists(n => pinnedFilm.contains(n.id))
      val scored  = scope.pooled(cluster)
      val eligible = scored.filterNot(_.denied)
      // A cluster the title family alone decided is filed at the family's bound when its own
      // scoring rates the film lower.
      val familyBound = film.filter(_ => cluster.forall(n => !accepted.contains(n.id))).flatMap(f =>
        cluster.flatMap(n => familyTaken.get(n.id)).filter(_._1 == f).map(_._2).minOption)
      val confidence =
        if (pinned) 1.0
        else film.fold(eligible.map(1 - _.p).product)(f => math.max(acceptance.confidenceOf(scored, f), familyBound.getOrElse(0.0)))
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
        s"${n.label}: own match ${s.c.tmdbId} at ${ResolverDecision.percent(c)}${acceptance.liftedBy(scope.of(n), s, c)} " +
          s"(${calibration.explain(ListingFilm, s.measures)})" })
      val joins = edges.filter(e => e.must && ids(e.a) && ids(e.b)).groupBy(_.reason).toSeq.sortBy(_._1)
        .map { case (r, es) => s"joined by $r ×${es.size}" }
      val apart = edges.filter(e => !e.must && (ids(e.a) ^ ids(e.b))).map { e =>
        val other = if (ids(e.a)) e.b else e.a
        s"kept apart from ${nodeById(other).label} (cluster ${clusterIndex(other)}): ${e.reason}"
      }.distinct.sorted
      val vote  = cluster.flatMap(n => familyTaken.get(n.id).map(_._3)).headOption.orElse(
        cluster.flatMap(n => voted.get(n.id)).headOption.map { case (id, p) => s"pooled evidence of ${cluster.size} node(s) → $id at ${ResolverDecision.percent(p)}" })
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

      val roundA = solve(members, roundAByFamily.getOrElse(family, Nil), accepted.get)
      // Group-level voting over the clusters no member matched alone.
      val unaccepted = roundA.filter(_.forall(n => !accepted.contains(n.id)))
      // A facts-free cluster in a split title family follows the family's clear majority
      // (`familyMajority`); every other cluster votes on its pooled evidence.
      val familyTaken: Map[String, (Int, Double, String)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else {
          // The title family: the round's title must-links (same title, search form, original
          // title, segment), however the solver then split the nodes they join.
          val titleEdges = roundAByFamily.getOrElse(family, Nil).filter(e => e.must && TitleTiers(e.tier))
          unaccepted.flatMap(c => familyMajority(c, members, accepted, titleEdges).toSeq.flatMap(t => c.map(_.id -> t))).toMap
        }
      val voted: Map[String, (Int, Double)] =
        if (mutation == Mutation.NoVoting) Map.empty
        else unaccepted.flatMap(c => if (c.exists(n => familyTaken.contains(n.id))) c.map(n => n.id -> (familyTaken(n.id)._1, familyTaken(n.id)._2))
                                     else vote(c, scope)).toMap
      val filmOf: String => Option[Int] = id => accepted.get(id).orElse(voted.get(id).map(_._1))
      val edges    = edgesOf(members, filmOf)
      val clusters = solve(members, edges, filmOf)
      finalEdges ++= edges

      val clusterIndex = clusters.zipWithIndex.flatMap { case (c, i) => c.map(_.id -> i) }.toMap
      violations += edges.count(e => !e.must && clusterIndex(e.a) == clusterIndex(e.b))
      clusters.foreach(cluster => decisions += decide(cluster, scope, filmOf, accepted, voted, edges, clusterIndex, familyTaken))
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
