package services.identity

import services.identity.IdentityMeasures.{ListingFilm, Measure}
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
 *           same accepted film, 2 same sanitised title, 3 same search form or original title, 4 one's
 *           title a delimited segment of the other's, unless the rest of the other's names a film of
 *           its own) and
 *           cannot-links (different accepted films; a node denying the other's film; the two
 *           listings' own evidence apart, `ListingConstraints.learnedListingListing`: a learned rule
 *           or the "listing-listing" cut, only when the two compare a fact both published) —
 *           solved by [[ConstraintSolver]] (cannot wins; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING: a cluster no member of which accepted a film scores its members'
 *           evidence POOLED into one listing (the heaviest title, the modal year, every director),
 *           and the winner, if accepted — and no rival the title names by the same pieces fits the
 *           pooled facts better, nor is it one of two films the title names by disjoint pieces
 *           (`pooledAccepted`) — becomes every member's film. A winner some members' own
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

  /** A node: the listings sharing one evidence. Named by its smallest listing's sort key. */
  private final class Node(val evidence: Evidence, val listings: Seq[Listing]) {
    val id: String          = listings.head.sortKey
    val weight: Int         = listings.size
    val venue: String       = listings.head.venue
    val venues: Set[String] = listings.map(_.venue).toSet
    def label: String       = s"'${evidence.title}'${evidence.statedYear.fold("")(y => s" [$y]")}" +
      (if (evidence.directors.nonEmpty) s" {${evidence.directors.mkString(", ")}}" else "") + s" ×$weight"
  }

  /** A venue's own name and its city's, as words: what a title piece naming the venue spells. */
  private def placesOf(cinema: models.Cinema): Seq[Seq[String]] =
    (Seq(cinema.displayName) ++ models.City.forCinema(cinema).map(_.labels.nominative))
      .map(services.movies.TitleContainment.tokens).filter(_.nonEmpty)

  /** Thrown when `count` edges cross a family: a rule was added without its block key. */
  final class FamilyCrossing(val count: Int, message: String) extends IllegalStateException(message)

  /** `pins` are the curation's hard constraints (`ListingConstraints.pinned`): a pinned film
   *  replaces a listing's own match, a denied one is never eligible, a pinned group is must-linked
   *  above every derived tier, and a derived edge the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
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
    /** A candidate scored for `listing`, which its own title searches ranked at `rank` (best,
     *  1-based). `seasonProduction`: the film's record names the listing's season production
     *  (`IdentityMeasures.namesSeasonProduction`). `deniedByPin`: a pin, not the listing's
     *  evidence, is (part of) why it is `denied`. */
    final case class Scored(c: Candidate, p: Double, measures: Map[String, Measure], denied: Boolean,
                            listing: IdentityMeasures.Listing, rank: Option[Int], seasonProduction: Boolean = false,
                            deniedByPin: Boolean = false)

    /** What the LISTING'S OWN facts contribute — the title, year, director, runtime, original
     *  title and country measures — as opposed to the film database's ranking priors (search rank,
     *  popularity, rivals) and the family's pooled count (`venues.corroborating`). */
    val Priors = IdentityMeasures.RankingPriors ++ IdentityMeasures.PooledMeasures
    def ownContributions(measures: Map[String, Measure]): Double =
      calibration.contributions(ListingFilm, measures).collect { case (name, w) if !Priors(name) => w }.sum
    /** The calibrated probability on the listing's own facts alone — what a cannot-link reads. A
     *  film the listing's facts do not contradict is never vetoed merely for ranking second in
     *  TMDB's search or for having same-titled rivals: that is ambiguity, not evidence of a
     *  different film. */
    def factsProbability(measures: Map[String, Measure]): Double = {
      // A measure that only AGREES (`IdentityMeasures.AgreesOnly`) never pushes toward a veto.
      val veto = calibration.contributions(ListingFilm, measures).collect {
        case (name, w) if !Priors(name) && !(IdentityMeasures.AgreesOnly(name) && w < 0) => w }.sum
      calibration.scopes(ListingFilm).calibration(calibration.scopes(ListingFilm).prior + veto)
    }
    /** Which house each listing banner is, learned from how every node's candidates bill its works
     *  (`IdentityMeasures.Houses`): the title relation reads a record of the listing's house as
     *  naming it, and a season production must be of it when it is known. */
    val houses: IdentityMeasures.Houses = IdentityMeasures.Houses.learn(nodes.flatMap { n =>
      IdentityMeasures.Houses.evidence(n.evidence.measured, (ownSearch(n.id).keys ++ ownWalk(n.id)).toSeq.distinct.sorted.map(candidateById(_).film))
    })
    def namesItsSeasonProduction(l: IdentityMeasures.Listing, f: IdentityMeasures.Film): Boolean =
      IdentityMeasures.namesSeasonProduction(l, f) && !IdentityMeasures.billing(l, f).exists(houses.other)

    /** Does the listing's own evidence rule the film out: a learned cannot-link, its facts'
     *  probability below the certified cut (both on [[vetoingMeasures]]), a season its title names
     *  that the film is not of, or its season's production of its work by ANOTHER house than its
     *  banner's (`houses`). */
    def evidenceDenies(l: IdentityMeasures.Listing, f: IdentityMeasures.Film, measures: Map[String, Measure]): Boolean = {
      val vetoing = vetoingMeasures(l, measures)
      ListingConstraints.seasonsApart(l.seasonYear, IdentityMeasures.filmSeason(f), f.year).isDefined ||
        (IdentityMeasures.namesSeasonProduction(l, f) && !namesItsSeasonProduction(l, f)) ||
        ListingConstraints.learnedListingFilm(calibration, vetoing, factsProbability(vetoing)).isDefined
    }
    /** The measures a veto may read — the score still reads them all — without
     *  - the published year when it denies a film the same director made
     *    (`IdentityMeasures.ownAgreement`): there it dates the screening (`IdentityMeasures.PublishedYear`);
     *  - an original title that only repeats the listing's own title when it weighs against the film
     *    (`IdentityMeasures.repeatsItsTitle`): it is the title again, and a title alone never vetoes. */
    def vetoingMeasures(l: IdentityMeasures.Listing, measures: Map[String, Measure]): Map[String, Measure] = {
      def weighsAgainst(name: String) = calibration.contributions(ListingFilm, measures).exists { case (n, w) => n == name && w < 0 }
      val screeningYear = IdentityMeasures.sameDirector(measures) && IdentityMeasures.ownAgreement(measures)._2("year")
      val repeatedTitle = IdentityMeasures.repeatsItsTitle(l) && weighsAgainst("originalTitle")
      measures -- (if (screeningYear) IdentityMeasures.PublishedYear else Set.empty) -- (if (repeatedTitle) Set("originalTitle") else Set.empty)
    }

    /** Does `n`'s title name the film only by a PIECE that is its venue's own name or place — every
     *  listing's, the venue's name or its city's? Kino Twierdza's "TWIERDZA - VINCENT. LEGENDA
     *  OCEANU" bills the venue, not *The Rock*, whose Polish title is "Twierdza"; the Alamo
     *  Drafthouse circuit's "Dismember the Alamo 2026 - Chicago" at its Chicago venue names the
     *  city, not the musical. A title that is the venue's name and nothing more (a year aside) still
     *  names its film, whatever the venue is called. */
    def namesOnlyItsVenue(n: Node, f: IdentityMeasures.Film): Boolean = {
      val whole  = services.movies.TitleContainment.tokens(n.evidence.title)
      val pieces = IdentityMeasures.namingPieces(n.evidence.measured, f)
      // The rest of the title must say something beside the venue: "Charlotte (2021)" at a
      // Charlotte venue is the film, its year only dating it.
      def besideIt(p: Seq[String]) = whole.diff(p).exists(_.exists(Character.isLetter))
      pieces.nonEmpty && pieces.forall(p => besideIt(p) && n.listings.forall(l => placesOf(l.cinema).exists(_.containsSlice(p))))
    }

    /** The listings of a family by title key, with their venues: `venues.corroborating`'s group. */
    final class FamilyScope(members: Seq[Node]) {
      val pool: Seq[Candidate] = members.flatMap(m => ownSearch(m.id).keys ++ ownWalk(m.id)).distinct.sorted.map(candidateById)
      /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
       *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
       *  record titled only a listing's qualifier does not name it. The family's own pool, so a
       *  family resolves alone as it does among the others. */
      val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(pool.flatMap(c => Seq(c.film.title) ++ c.film.originalTitle))
      private val groups: Map[String, Seq[(String, IdentityMeasures.Listing)]] =
        members.flatMap(n => n.listings.map(l => IdentityMeasures.key(n.evidence.title) -> (l.venue -> n.evidence.measured)))
          .groupMap(_._1)(_._2)
      private val backing = new IdentityMeasures.VenueBacking(groups.getOrElse(_, Nil))

      /** Every candidate `l` has an evidence path to, scored; `denies` marks the ones its own
       *  evidence rules out (`ListingConstraints.learnedListingFilm`), which are never eligible. */
      def score(l: IdentityMeasures.Listing, venue: String, ranks: Map[Int, Int], walked: Set[Int],
                deniedByPins: Int => Boolean): Seq[Scored] = {
        val relation  = pool.map(c => c.tmdbId -> IdentityMeasures.titleRelation(l, c.film, houses, qualifiers).value).toMap
        val reachable = pool.filter(c => ranks.contains(c.tmdbId) || walked(c.tmdbId) || IdentityMeasures.NamingRelations(relation(c.tmdbId)))
        val close     = reachable.count(c => IdentityMeasures.Rivalling(relation(c.tmdbId)))
        val group     = IdentityMeasures.key(l.title)
        val scored = reachable.map { c =>
          val rivals   = close - (if (IdentityMeasures.Rivalling(relation(c.tmdbId))) 1 else 0)
          val measures = IdentityMeasures.listingFilm(l, c.film, ranks.get(c.tmdbId), rivals,
            backing.corroborating(group, c.film, venue), houses, qualifiers)
          val p = calibration.probability(ListingFilm, measures)
          Scored(c, p, measures, deniedByPins(c.tmdbId) || evidenceDenies(l, c.film, measures), l, ranks.get(c.tmdbId),
            namesItsSeasonProduction(l, c.film), deniedByPins(c.tmdbId))
        }
        // The listing's whole title (or its original or an alternative title) and its credited
        // director name ONE film together: another film of that director, which its title does not
        // name, is not the listing's — however its runtime or year fits. A director's filmography is a path to candidates, never a reason to leave
        // the one its title names (Syndicated's "Zodiac", Fincher, 139 minutes, is not Fight Club).
        val titled = scored.exists(s => !s.denied && IdentityMeasures.Rivalling(relation(s.c.tmdbId)) && IdentityMeasures.sameDirector(s.measures))
        scored.map(s =>
          if (titled && !IdentityMeasures.NamingRelations(relation(s.c.tmdbId)) && IdentityMeasures.sameDirector(s.measures)) s.copy(denied = true)
          else s)
          .sortBy(s => (-s.p, s.c.tmdbId))
      }

      private val memo = mutable.HashMap.empty[String, Seq[Scored]]
      def of(n: Node): Seq[Scored] = memo.getOrElseUpdate(n.id,
        score(n.evidence.measured, n.venue, ownSearch(n.id), ownWalk(n.id),
          id => pins.deniedFilms(n.listings.head.key)(id) || namesOnlyItsVenue(n, candidateById(id).film)))

      /** The cluster's members read as ONE listing: the title most of its listings carry (the
       *  smaller node on a tie), the year most of them publish (a title's bracket or season stays the lead title's own measure), every director and country, the
       *  median runtime, the modal original title, and every candidate any of them named. */
      def pooled(cluster: Seq[Node]): Seq[Scored] = {
        def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
          values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (v, w) => (-w, v) }.headOption.map(_._1)
        val lead     = cluster.sortBy(n => (-n.weight, n.id)).head
        val runtimes = cluster.flatMap(n => n.evidence.runtime.toSeq.flatMap(r => Seq.fill(n.weight)(r))).sorted
        val listing  = lead.evidence.measured.copy(
          year          = modal(cluster.flatMap(n => n.evidence.year.map(_ -> n.weight))),
          originalTitle = modal(cluster.flatMap(n => n.evidence.originalTitle.map(_ -> n.weight))),
          directors     = cluster.flatMap(_.evidence.directors).distinct.sorted,
          runtime       = runtimes.lift(runtimes.size / 2),
          countries     = cluster.flatMap(_.evidence.countries).distinct.sorted)
        val ranks = cluster.flatMap(n => ownSearch(n.id)).groupMapReduce(_._1)(_._2)(math.min)
        score(listing, lead.venue, ranks, cluster.flatMap(n => ownWalk(n.id)).toSet,
          id => cluster.exists(n => pins.deniedFilms(n.listings.head.key)(id) || namesOnlyItsVenue(n, candidateById(id).film)))
          .map(s => if (s.denied || cluster.forall(n => !of(n).exists(o => o.c.tmdbId == s.c.tmdbId && o.denied))) s else s.copy(denied = true))
      }
    }

    def ownEvidence(s: Scored): Double = ownContributions(s.measures)
    /** Does anything the listing PUBLISHED weigh against the film — an own-fact measure the listing
     *  did not leave missing, with a negative weight? */
    def speaksAgainst(s: Scored): Boolean =
      calibration.contributions(ListingFilm, s.measures).exists { case (name, w) =>
        !Priors(name) && w < 0 && !s.measures.get(name).exists(_.isInstanceOf[IdentityMeasures.Missing])
      }

    /** The listing's EXACT TOP HIT, accepted on what the calibration measured for its evidence as a
     *  CLASS (`IdentityCalibration.classProbability`): the one film its whole title names exactly
     *  that its own title search returned first, in TMDB's order ([[IdentityMeasures.exactTopHits]]),
     *  when the listing's own evidence rules it out on nothing, published nothing against it, and
     *  gives no rival a better fit. A bare title credits it with the naive-Bayes sum of missing
     *  facts, a low popularity and its same-titled rivals, which undersells what the class measured
     *  on the labels; here the search standing LENDS confidence and never withdraws it. */
    def topHit(ranked: Seq[Scored]): Option[(Scored, Double)] = ranked.headOption.flatMap { any =>
      val eligible = ranked.filterNot(_.denied)
      IdentityMeasures.exactTopHits(any.listing, ranked.map(s => (s.c.tmdbId, s.c.film, s.rank))) match {
        case Seq(id) =>
          eligible.find(_.c.tmdbId == id)
            .filter(best => !speaksAgainst(best) && eligible.forall(r => (r eq best) || ownEvidence(r) <= ownEvidence(best)))
            .flatMap(best => calibration.classProbability(ListingFilm, best.measures).map(cp => best -> math.max(best.p, cp)))
            .filter(x => calibration.showsRatings(x._2))
        case _ => None
      }
    }

    /** The probability that `film` is the listing's film — the decision's confidence, on the scale
     *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
     *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
     *  lent when the listing's facts accepted it ([[calibrated]]). */
    def confidenceOf(ranked: Seq[Scored], film: Int): Double =
      topHit(ranked).filter(_._1.c.tmdbId == film).map(_._2)
        .orElse(calibrated(ranked).filter(_._1.c.tmdbId == film).map(_._2))
        .orElse(pooledAccepted(ranked).filter(_._1.c.tmdbId == film).map(_._2))
        .getOrElse(ranked.filterNot(_.denied).find(_.c.tmdbId == film).fold(0.0)(_.p))
    /** `best`'s probability with the database's ranking priors and the family's pooled count
     *  LENDING confidence but never withdrawing it — each one's negative weight capped at 0 — when
     *  the listing's own facts decide: it compares a fact, nothing it published weighs against the
     *  film, and its facts favour the film over every other eligible candidate. Otherwise the
     *  calibrated probability: a namesake the facts fit alike is told apart only by the ranking,
     *  which then keeps its full weight. */
    def priorsLent(best: Scored, eligible: Seq[Scored]): Double =
      if (!IdentityMeasures.comparesAFact(ListingFilm, best.measures) || speaksAgainst(best) ||
          eligible.exists(r => (r ne best) && ownEvidence(r) >= ownEvidence(best))) best.p
      else {
        val scope = calibration.scopes(ListingFilm)
        val lent  = calibration.contributions(ListingFilm, best.measures).map { case (name, w) => if (Priors(name)) math.max(0.0, w) else w }.sum
        math.max(best.p, scope.calibration(scope.prior + lent))
      }
    /** The best eligible candidate, when its probability — the priors lending, never withdrawing
     *  ([[priorsLent]]) — clears the calibration's cut. */
    def calibrated(ranked: Seq[Scored]): Option[(Scored, Double)] = {
      val eligible = ranked.filterNot(_.denied)
      eligible.headOption.map(b => b -> priorsLent(b, eligible)).filter(x => calibration.showsRatings(x._2))
    }
    /** The eligible candidate whose record names the listing's SEASON PRODUCTION — the season and
     *  the work its title names. `None`: no candidate does. `Some(Some(x))`: `x` is the listing's
     *  film on that identity, whatever the calibrated probability (the fitted weights do not read a
     *  season yet): a season names its production as a published year names a film, and the
     *  namesakes it rules out are already denied (`ListingConstraints.seasonsApart`).
     *  `Some(None)`: two records do (two houses' stagings of one work in one season) and the
     *  listing's own facts favour neither — ambiguity, so nothing is taken, on the season or on
     *  the database's ranking. The confidence stays the calibrated probability. */
    def seasonProductionOf(ranked: Seq[Scored]): Option[Option[(Scored, Double)]] =
      ranked.filter(s => !s.denied && s.seasonProduction).sortBy(s => (-ownEvidence(s), s.c.tmdbId)) match {
        case Seq()        => None
        case Seq(one)     => Some(Some(one -> one.p))
        case a +: b +: _  => Some(Option.when(ownEvidence(a) > ownEvidence(b))(a -> a.p))
      }
    /** The EDITION of the accepted film that the listing's whole title names, when there is exactly
     *  one: a later record carrying the film's title under a qualifier (`IdentityMeasures.editionOf`
     *  — "Radiohead X Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu"), which the listing
     *  names by its whole title while it names the film only by a piece. The listing's facts chose
     *  the work, and an edition carries its work's facts — the venue credits Murnau, TMDB the
     *  edition's maker — so they do not deny the edition; a pin still does. With the confidence of
     *  the work. The film itself when the listing names it whole, or no edition, or two. */
    def editionNamed(ranked: Seq[Scored])(accepted: (Scored, Double)): (Scored, Double) = {
      val (work, confidence) = accepted
      def namedWhole(s: Scored) = IdentityMeasures.Rivalling(s.measures.get("title").collect { case IdentityMeasures.Category(c) => c }.getOrElse(""))
      if (namedWhole(work)) accepted
      else ranked.filter(e => (e ne work) && !e.deniedByPin && namedWhole(e) && IdentityMeasures.editionOf(e.c.film, work.c.film)) match {
        case Seq(edition) => edition -> confidence
        case _            => accepted
      }
    }

    /** A node accepts a film ON ITS OWN only when its own facts favour it over the runner-up: a
     *  bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not
     *  decided alone — it follows the film its title's credited siblings chose (the cluster's), or
     *  the pooled vote — unless it is the listing's exact top hit, which is measured as a class. */
    def acceptedAlone(ranked: Seq[Scored]): Option[(Scored, Double)] = {
      val eligible = ranked.filterNot(_.denied)
      seasonProductionOf(ranked).getOrElse(
        calibrated(ranked).filter { case (best, _) => eligible.lift(1).forall(favours(best, _)) }
          .orElse(topHit(ranked))).map(editionNamed(ranked))
    }
    /** Do the listing's own facts favour `best` over `other`? Its own evidence — without the title
     *  when the title names the two by disjoint pieces ([[IdentityMeasures.namedApart]]: "Lalka
     *  (Dolly)"), since it then names both alike and how each piece spells its film is no fact
     *  about which film the listing is. */
    def favours(best: Scored, other: Scored): Boolean =
      if (IdentityMeasures.namedApart(best.listing, best.c.film, other.c.film)) factsOf(best) > factsOf(other)
      else ownEvidence(best) > ownEvidence(other)
    /** What the listing's published FACTS alone contribute: its own evidence without the title relation. */
    def factsOf(s: Scored): Double =
      ownEvidence(s) - calibration.contributions(ListingFilm, s.measures).collect { case ("title", w) => w }.sum

    /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
     *  best eligible candidate the calibration accepts — unless the title names another candidate
     *  by the very same pieces and the pooled FACTS ([[factsOf]]) fit that one better: four
     *  "Camino dla opornych" whose original title "Santiago" names two films, and whose 113
     *  minutes fit the fourth the search returned, do not take the 93-minute first. A candidate
     *  the title names less specifically ("Mad Max" inside "Mad Max 2: The Road Warrior") or not
     *  at all is no such rival, and namesakes the facts fit alike stay the calibration's to tell
     *  apart — its ranking priors and the family's venue count are measured evidence there (a
     *  bare "Resident Evil" at 148 venues). Two films the title names by disjoint pieces
     *  ([[IdentityMeasures.namedApart]]) are not namesakes: only the facts may pick one of
     *  "Lalka (Dolly)"'s two. Each the edition of it the listing names, if any ([[editionNamed]]). */
    def pooledAccepted(ranked: Seq[Scored]): Option[(Scored, Double)] = {
      val eligible = ranked.filterNot(_.denied)
      seasonProductionOf(ranked).getOrElse(
        calibrated(ranked).filter { case (best, _) =>
          eligible.forall(r => (r eq best) || {
            val pieces = IdentityMeasures.namingPieces(r.listing, r.c.film)
            val alike  = pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.c.film)
            val apart  = IdentityMeasures.namedApart(best.listing, best.c.film, r.c.film)
            !(alike && factsOf(r) > factsOf(best)) && !(apart && factsOf(r) >= factsOf(best))
          })
        }.orElse(topHit(ranked))).map(editionNamed(ranked))
    }

    /** Does `n`'s own title evidence name `film`: its title searches returned it, or its title (a
     *  whole spelling, its original title or a segment) names the film's? */
    def titleNames(n: Node, film: Candidate): Boolean =
      ownSearch(n.id).contains(film.tmdbId) ||
        IdentityMeasures.NamingRelations(IdentityMeasures.titleRelation(n.evidence.measured, film.film).value)
    /** The group vote over a cluster's POOLED scoring: the accepted film — but a film no member's
     *  title names, which only a credited director's filmography reached, only when nothing else
     *  the walk reached fits the pooled facts as well: every rival's own facts fit worse or equally,
     *  and the calibration rates it strictly lower. A walk is a path to candidates; it cannot pick
     *  among a director's films the listing's facts favour another of (a lecture on "Trzy kolory:
     *  Niebieski" is not "Czerwony"), or that the calibration cannot tell apart. */
    def votedFor(cluster: Seq[Node], ranked: Seq[Scored]): Option[(Scored, Double)] =
      pooledAccepted(ranked).filter { case (s, _) =>
        cluster.exists(titleNames(_, s.c)) ||
          ranked.filterNot(r => r.denied || (r eq s)).forall(r => r.p < s.p && ownEvidence(r) <= ownEvidence(s))
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

    val sanitized  = (s: String) => normalizer.sanitize(s)
    val searchForm = (s: String) => normalizer.searchQuery(s)
    val segmentsOf: Map[String, Set[String]] = nodes.map(n => n.id ->
      (IdentityMeasures.titleShapes(n.evidence.measured).map(sanitized).toSet - sanitized(n.evidence.cleanTitle)).filter(_.nonEmpty)).toMap
    def segmentOf(whole: Node, decorated: Node): Boolean =
      segmentsOf(decorated.id).contains(sanitized(whole.evidence.cleanTitle))
    /** Do two nodes' titles must-link them (tiers 2–4: same sanitised title, same search form,
     *  an original title naming the other, or one a whole segment of the other)? */
    def titleLinked(x: Node, y: Node): Boolean = {
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
    def withoutSiblingDenials(members: Seq[Node], scope: FamilyScope,
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
      scopes = nodes.groupBy(n => familyOf(n.id)).map { case (f, ms) => f -> new FamilyScope(ms.sortBy(_.id)) }
      bestOf = nodes.groupBy(n => familyOf(n.id)).toSeq.flatMap { case (f, members) =>
        val scope = scopes(f)
        withoutSiblingDenials(members, scope, members.flatMap(n => acceptedAlone(scope.of(n)).map(n.id -> _)).toMap)
      }.toMap
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
          evidenceDenies(n.evidence.measured, c.film, IdentityMeasures.listingFilm(n.evidence.measured, c.film, None, 0, 0, houses, scopeOf(n).qualifiers))
        }
      })(_.denied)

    // ── B. global assignment, per family ─────────────────────────────────────────────────
    def pairsSharingAKey(members: Seq[Node]): Seq[(Node, Node)] = {
      val index = members.zipWithIndex.flatMap { case (n, i) => blockKeysOf(n.id).map(_ -> i) }.groupMap(_._1)(_._2)
      index.values.iterator.flatMap { is =>
        val sorted = is.distinct.sorted
        for (x <- sorted.iterator; y <- sorted.iterator if x < y) yield (x, y)
      }.toSeq.distinct.sorted.map { case (i, j) => (members(i), members(j)) }
    }
    // The two listings' own evidence apart: the seasons their titles name, or the learned
    // "listing-listing" scope when they compare a fact both published.
    def listingsApart(x: Node, y: Node): Option[String] = {
      val (a, b) = (x.evidence.measured, y.evidence.measured)
      lazy val m = IdentityMeasures.listingListing(a, b, sameVenue = (x.venues intersect y.venues).nonEmpty, sharedChainId = None)
      ListingConstraints.seasonsApart(a.seasonYear, b.seasonYear, b.year)
        .orElse(ListingConstraints.seasonsApart(b.seasonYear, a.seasonYear, a.year))
        .orElse(ListingConstraints.learnedListingListing(calibration, m))
        .map(_.toString)
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


    /** Does the rest of `decorated`'s title name a film of its own, beside `whole`'s title — a
     *  candidate it may still take whose naming pieces share no word with `whole`'s? "Lalka (Dolly)"
     *  carries "Lalka" whole, but its "Dolly" names Blackhurst's film: it is not merely a decorated
     *  "Lalka", and the segment must not decide between the two for it. */
    def namesBeside(decorated: Node, whole: Node): Boolean = {
      val words = services.movies.TitleContainment.tokens(whole.evidence.cleanTitle).toSet
      scopeOf(decorated).of(decorated).exists { s =>
        val pieces = IdentityMeasures.namingPieces(decorated.evidence.measured, s.c.film)
        !s.denied && pieces.nonEmpty && pieces.forall(p => (p.toSet intersect words).isEmpty)
      }
    }

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
          // names two films apart, as it does one whose rest names a film of its own (`namesBeside`).
          Option.when((segmentOf(x, y) && !namesBeside(y, x)) || (segmentOf(y, x) && !namesBeside(x, y)))((4, "title-segment"))
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

    /** Why an own match's confidence stands above its calibrated probability: its exact top hit's
     *  class, or the ranking priors lending ([[priorsLent]]). */
    def liftedBy(ranked: Seq[Scored], s: Scored, confidence: Double): String =
      if (confidence <= s.p) ""
      else if (topHit(ranked).exists(_._1.c.tmdbId == s.c.tmdbId)) " as its exact top hit"
      else " with the ranking priors lending, never withdrawing"

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
    def carriedByOwnFacts(s: Scored): Boolean =
      IdentityMeasures.comparesAFact(ListingFilm, s.measures) && calibration.showsRatings(factsProbability(s.measures))
    def vote(cluster: Seq[Node], scope: FamilyScope): Seq[(String, (Int, Double))] = {
      def to(voters: Seq[Node], accepted: (Scored, Double)) = voters.map(n => n.id -> (accepted._1.c.tmdbId, accepted._2))
      val ranked = scope.pooled(cluster)
      votedFor(cluster, ranked).map(to(cluster, _)).getOrElse(ranked.headOption.filter(s => s.denied && carriedByOwnFacts(s)).toSeq.flatMap { vetoed =>
        val rest = cluster.filterNot(n => scope.of(n).exists(o => o.c.tmdbId == vetoed.c.tmdbId && o.denied))
        Option.when(rest.nonEmpty && rest.size < cluster.size)(rest).flatMap(rest => votedFor(rest, scope.pooled(rest))
          .filter { case (s, _) => s.c.tmdbId == vetoed.c.tmdbId && carriedByOwnFacts(s) }
          .map(to(rest, _))).getOrElse(Nil)
      })
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
        s"${n.label}: own match ${s.c.tmdbId} at ${ResolverDecision.percent(c)}${liftedBy(scope.of(n), s, c)} " +
          s"(${calibration.explain(ListingFilm, s.measures)})" })
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
        else roundA.filter(_.forall(n => !accepted.contains(n.id))).flatMap(vote(_, scope)).toMap
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
