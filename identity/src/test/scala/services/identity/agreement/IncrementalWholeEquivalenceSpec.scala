package services.identity.agreement

import models.{KinoMuza, KinoPalacowe, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{AnswersChanged, Answer, CandidateQuery, DetailFacts, FilmTable, GeneratedIdentityCorpus, Hit, IdentityCalibration,
  IdentityLookups, IdentityMeasures, IdentityResolver, IncrementalResolver, InMemoryIdentityModelStore, Listing, PosterAnswers, PosterHash, Question,
  Resolution, ResolverDecision, ScreeningDays, TakeFlips, TitleDecorations, UnifiedEvidence, UnifiedRules}
import services.movies.{ListingKey, SingleCountryNormalizer}
import tools.IndependentCases

import scala.collection.mutable
import scala.util.Random
import scala.util.chaining.scalaUtilChainingOps
import scala.util.hashing.MurmurHash3

/**
 * The owner's no-drift principle (docs/design/identity-no-drift.md): every change re-evaluates everything exactly as if
 * all the data had been there from the start — grouping and takes alike. The incremental path (the model kept event by
 * event, the agreement stage kept between applies, restarts over the stores with the cluster keys kept, lost or numbered
 * otherwise) is held, after EVERY event of a random history, to a whole resolve of the inputs as they are then: the
 * model's resolve of the listings held, and a fresh stage applied once to it. The inputs move as production's do —
 * listings arrive in batches and leave, TMDB's, the families' and the posters' answers are filed a round at a time, a
 * question is attempted and fails before its answer lands, the clock crosses a new year — and nothing else does: no
 * tolerance, no allowed difference.
 *
 * Under production's correction budget the stage reads only so many model takes an apply, so a budgeted stage is held to
 * the whole resolve once it has no backlog left; a restart alone, whatever keys it is handed, moves no take; and a twin
 * stage whose names run otherwise groups, takes and waits as the stage does.
 *
 * The cases: generated corpora ([[GeneratedIdentityCorpus]], with family records and venue posters of the films drawn),
 * and the shapes production met ([[IncrementalWholeEquivalenceSpec.Recorded]]): a fill of a widely billed current
 * release, a model take corrected by its poster and two families.
 */
class IncrementalWholeEquivalenceSpec extends AnyFlatSpec with Matchers {
  import IncrementalWholeEquivalenceSpec._

  private val Seeds = 1L to 40L
  private def cases: Seq[(String, Long => Case)] = ("generated" -> Case.generated) +: Recorded.all

  private def every(budget: Budget)(check: History => Option[String]): Seq[String] =
    IndependentCases.flatMap(for { (label, make) <- cases; seed <- Seeds } yield (label, make, seed)) { case (label, make, seed) =>
      check(new History(make(seed), seed, budget)).map(s"$label: " + _)
    }

  "the incremental identity" should "equal a whole resolve of its inputs after every event of a random history" in {
    every(Budget.Unlimited)(_.stepwise()) shouldBe empty
  }

  // STEP 6 (the owner, 2026-10-31): a Pending take is the decision on the inputs as they stand -- unanswered questions
  // unknown -- served provisionally, never a previous take substituted. Equal to a whole resolve of the same inputs at
  // every step, film and all, and not vacuously: histories serve films while Pending.
  it should "serve a Pending take as a whole resolve of the inputs as they stand decides it, film and all" in {
    val provisional = new java.util.concurrent.atomic.AtomicInteger
    every(Budget.Unlimited)(_.stepwise(provisional = () => { provisional.incrementAndGet(); () })) shouldBe empty
    withClue("listing-steps served a film while Pending: ") { provisional.get should be > 0 }
  }

  it should "equal it once settled under production's correction budget, restarted with its keys kept, lost or renumbered" in {
    every(Budget.Production)(_.settledEnd()) shouldBe empty
  }

  it should "move no take on a restart alone, whatever cluster keys it is handed and however little of the budget is left" in {
    every(Budget.Production)(_.restartsAlone()) shouldBe empty
  }

  it should "group, take and wait as a twin whose names run otherwise does: counted from elsewhere, then lost, renumbered or permuted" in {
    val parted = new java.util.concurrent.atomic.AtomicInteger
    every(Budget.Production)(_.namesReadByNothing(asking = false, parted = () => parted.incrementAndGet())) shouldBe empty
    withClue("histories whose names parted from the twin's: ") { parted.get should be > cases.size * Seeds.size / 2 }
  }

  // A settled verdict's never-answered questions are read off its receipt (`AgreementStage.unreached`), never kept by a key:
  // a restart seeded them under the key each verdict was filed by until 2026-10-25, and a renumbered key asked another's.
  it should "ask what the twin asks" in {
    every(Budget.Production)(_.namesReadByNothing(asking = true, parted = () => ())) shouldBe empty
  }

  // THE FLIP PROPERTY (convergence-failures-2026-10-29 G3): the convergence leg's takes-drift rows fail on FLIPS too
  // (services.identity.TakeFlips, prod's kinowo_identity_listing_flips) -- a take that goes back to a film it left, moves
  // more than once, or moves where no input did -- which an end-state comparison cannot see: run 3509's rows failed on
  // flips alone, after the flip gate was born. Under production's correction budget the stage serves interim takes, and an
  // interim take a pending correction then overrides is exactly such a flip. Held here, in seconds, over generated
  // histories: every listing the incremental path flips, the whole resolve of the same inputs flips too (leaving and
  // returning listings, answers filed late, can move a whole resolve's take back and forth as well).
  it should "flip no take that a whole resolve of the same inputs, step by step, does not flip" in {
    val flipped = new java.util.concurrent.atomic.AtomicInteger
    every(Budget.Production)(_.flipsBeyondWhole(flipped = () => flipped.incrementAndGet())) shouldBe empty
    // not vacuous: the recorder sees flips -- ones the whole resolve makes too, so allowed -- in some of the histories
    withClue("histories where a listing flipped, the whole resolve's way: ") { flipped.get should be > 0 }
  }

  // P2 (identity-flips-structural-2026-10-30, property 1): the whole resolve itself, as answers are only filed, decides a
  // listing once and never again otherwise -- a decision made before an answer it reads is filed is a flip a whole
  // resolve makes too, which the flip property above cannot see (UK "Rose (2023)")
  it should "refine each take as answers are only filed: no film yet, then one decided value" in {
    every(Budget.Unlimited)(_.monotoneUnderFiling()) shouldBe empty
    // the Pending-through-decide path is in the sequences it checks: a film served provisionally, then decided
    val provisional = cases.exists { case (_, make) => Seeds.take(8).exists(seed =>
      new History(make(seed), seed, Budget.Unlimited).filingOutcomes().valuesIterator.exists(_.exists(o => isPending(o) && o != "pending:none"))) }
    withClue("a film served while Pending, in some history: ") { provisional shouldBe true }
  }

  // P2, P3 (identity-flips-structural-2026-10-30, property 2): the same filings in another order come to the same
  // outcomes -- each listing's, in every prefix, Pending or that one (property 1 per order, and the ends equal)
  it should "come to the same takes whatever order the same answers are filed in" in {
    IndependentCases.flatMap(for { (label, make) <- cases; seed <- Seeds } yield (label, make, seed)) { case (label, make, seed) =>
      val ends = (0L to 2L).map(order => new History(make(seed), seed, Budget.Unlimited, order).filingOutcomes().view.mapValues(_.last).toMap)
      val orders = (0L to 2L).flatMap(order => new History(make(seed), seed, Budget.Unlimited, order).monotoneUnderFiling().map(v => s"order $order: $v"))
      orders.headOption.orElse(ends.head.collectFirst { case (key, end) if ends.tail.exists(_.get(key).exists(_ != end)) =>
        s"seed $seed, ${key.venue} | ${key.rawTitle}: ${ends.map(_.getOrElse(key, "-")).mkString(" / ")} in orders 0 / 1 / 2" }).map(s"$label: " + _)
    } shouldBe empty
  }

  // P4 (identity-flips-structural-2026-10-30, property 4): no decision turns on an id -- the same corpus with its films
  // filed under other TMDB ids comes to the image of the same takes
  it should "take the same films whatever TMDB ids they are filed under" in {
    val reversed: Int => Int = i => 1011 - i                                // 1001..1010 the other way round
    def image(outcome: String): String =
      if (isPending(outcome)) s"pending:${image(outcome.stripPrefix("pending:"))}"
      else outcome.stripPrefix("tmdb:").toIntOption.filter(id => id > 1000 && id <= 1010).fold(outcome)(id => s"tmdb:${2011 - id}")
    // every round's outcome, not the end's alone: decided at the same rounds, the same films then, and Pending at the same
    // rounds -- the Pending-through-decide path (step 6) included. What a Pending take serves meanwhile is not compared: a
    // grouping tie the order of ids breaks shows there first (seed 7's Kino Apollo "Faust 2 (2026)", two listings of one
    // title, one joining the 2025 film's cluster by id order) -- the tie audit's (structural plan step 7), not this one's
    def state(outcome: String) = if (isPending(outcome)) "pending" else image(outcome)
    IndependentCases.flatMap(Seeds) { seed =>
      val plain = new History(Case.generated(seed), seed, Budget.Unlimited).filingOutcomes()
      val back  = (question: String) => "\\b10(0[1-9]|10)\\b".r.replaceAllIn(question, m => (2011 - m.matched.toInt).toString)
      val other = new History(Case.relabelled(seed, reversed), seed, Budget.Unlimited, canonical = back).filingOutcomes()
      plain.collectFirst { case (key, seq) if other.get(key).exists(o => o.map(o => if (isPending(o)) "pending" else o) != seq.map(state)) =>
        s"generated: seed $seed, ${key.venue} | ${key.rawTitle}: ${seq.mkString(" -> ")}, relabelled ${other(key).mkString(" -> ")}" }
    } shouldBe empty
  }

  // P3 (identity-flips-structural-2026-10-30, property 3): a budget decides how soon, never what -- at every step of the
  // same history, a listing's outcome under production's correction budget is the unbounded one's, or Pending: never
  // another decided value (Chichester x2, Cosi, Hamlet, Rocky Horror, La Fanciulla, E4 were each such a take)
  it should "decide under a correction budget what it decides unbounded, or wait" in {
    // production's budget binds over a country's corrections, never over these few clusters: one an apply, and none
    // (PL "Lalka" stood on its stored, unread correction past a restart, served 1001 as decided -- caught here)
    val histories = for { budget <- Seq(Budget.Tight, Budget.Spent); (label, make) <- cases; seed <- Seeds } yield (budget, label, make, seed)
    IndependentCases.flatMap(histories) { case (budget, label, make, seed) =>
      val (bounded, unbounded) = (new History(make(seed), seed, budget), new History(make(seed), seed, Budget.Unlimited))
      (0 to 30).iterator.map { step =>
        val ((event, b), (_, u)) = (bounded.outcomesAfter(step == 30), unbounded.outcomesAfter(step == 30))
        b.collectFirst { case (key, film) if !isPending(film) && u.get(key).exists(_ != film) =>
          s"$label ($budget): seed $seed, step $step ($event), ${key.venue} | ${key.rawTitle}: $film under the budget, ${u(key)} unbounded" }
      }.collectFirst { case Some(found) => found }
    } shouldBe empty
  }

  "its cases" should "reach the stage's takes — agreed, filled, corrected — and the model's matches" in {
    val bases = cases.flatMap { case (_, make) => Seeds.take(8).flatMap(seed => takes(new History(make(seed), seed, Budget.Unlimited).wholeAtEnd()).map(_._4)) }.toSet
    bases should contain allOf (ResolverDecision.Basis.Agreed, ResolverDecision.Basis.Filled, ResolverDecision.Basis.Corrected, ResolverDecision.Basis.OwnMatch)
  }
}

object IncrementalWholeEquivalenceSpec {

  /** What `d` serves: its TMDB film, its fallback's `source:id`, or none -- whatever its outcome. */
  def servedOf(d: ResolverDecision): String = d.film.map(id => s"tmdb:$id").orElse(d.fallback.map(f => s"${f.source}:${f.id}")).getOrElse("none")
  /** An outcome as the properties compare it: a decided film, "none", or "pending:<what it serves meanwhile>". */
  def outcomeString(outcome: services.identity.TakeOutcome, d: ResolverDecision): String = outcome match {
    case services.identity.TakeOutcome.Pending(_)  => s"pending:${servedOf(d)}"
    case services.identity.TakeOutcome.Taken(film) => film
    case services.identity.TakeOutcome.NoFilm(_)   => "none"
  }
  def isPending(outcome: String): Boolean = outcome.startsWith("pending")
  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.fromResource("services/identity/test-calibration.json").get

  /** How many model takes a stage reads afresh an apply: none bound, production's, or none (a deploy's spent budget). */
  enum Budget(val perApply: Int) {
    case Unlimited  extends Budget(Int.MaxValue)
    case Production extends Budget(AgreementStage.CorrectionsPerApply)
    case Spent      extends Budget(0)
    /** One correction an apply: what binds over these few clusters, as production's binds over a country's. */
    case Tight      extends Budget(1)
  }

  /** Each listing's cluster (its members) and take, in one order: what "equal" means — grouping and takes, exactly. */
  def takes(resolution: Resolution): Seq[(Seq[ListingKey], Option[Int], Option[(String, String)], ResolverDecision.Basis)] =
    resolution.decisions.filter(_.members.nonEmpty)
      .map(d => (d.members.sorted(using ListingKey.ordering), d.film, d.fallback.map(f => f.source -> f.id), d.basis))
      .sortBy(_._1.head)(using ListingKey.ordering)

  /** The first takes two resolutions differ on, named. */
  def difference(incremental: Resolution, whole: Resolution): Option[String] = {
    val (a, b) = (takes(incremental), takes(whole))
    Option.when(a != b) {
      val only = (a.toSet -- b).toSeq.sortBy(_._1.head)(using ListingKey.ordering).take(2)
      val want = (b.toSet -- a).toSeq.sortBy(_._1.head)(using ListingKey.ordering).take(2)
      s"incremental ${only.mkString("; ")} | whole ${want.mkString("; ")}"
    }
  }

  /** What a history runs over: the listings as their venues publish them, TMDB, each family's records, the posters'
   *  hashes (a venue poster's by its URL, TMDB's of a film), the IMDb ids TMDB finds, and the fill rules selected. */
  final case class Case(listings: Seq[Listing], tmdb: IdentityLookups, families: Map[VoterFamily, Map[String, SourceRecord]],
                        venuePosters: Map[String, PosterHash] = Map.empty, filmPosters: Map[Int, Seq[PosterHash]] = Map.empty,
                        finds: Map[String, Int] = Map.empty, rules: UnifiedRules = UnifiedRules.resolver)

  object Case {
    private def poster(film: Int) = PosterHash(MurmurHash3.stringHash(s"poster|$film").toLong << 32 | (MurmurHash3.stringHash(s"poster2|$film") & 0xffffffffL))

    /** A generated corpus: four families holding most of its films (Filmweb's records linking no IMDb id), most listings
     *  showing a poster of the film they really are, TMDB's posters of its own films. */
    def generated(seed: Long): Case = relabelled(seed, 1000 + _)
    /** The generated case with its films filed under the TMDB ids `tmdbIdOf` names. */
    def relabelled(seed: Long, tmdbIdOf: Int => Int): Case = {
      val corpus = GeneratedIdentityCorpus.generate(seed, normalizer, films = 10, listings = 48, tmdbShare = 0.6, tmdbIdOf = tmdbIdOf)
      val rnd    = new Random(seed * 104729)
      def imdbOf(film: GeneratedIdentityCorpus.Film) = s"tt${9000000 + film.id}"
      val families = Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map { family =>
        family -> corpus.films.filter(_ => rnd.nextInt(10) < 7).map { film =>
          val ids = (if (family == VoterFamily.Filmweb) Map.empty else Map("imdb" -> imdbOf(film))) ++ film.tmdbId.map("tmdb" -> _.toString)
          // IMDb's records are filed under their IMDb id, as production's are; the rest under their own
          (if (family == VoterFamily.Imdb) imdbOf(film) else s"${family.label}-${film.id}") -> SourceRecord(IdentityMeasures.Film(film.title, None, Nil, Some(film.year), Some(film.runtime),
            Some(Seq(film.director)), None, None), ids)
        }.toMap
      }.toMap
      val listings = corpus.listings.map { listing =>
        if (rnd.nextInt(10) < 7) listing.copy(poster = corpus.truth.get(listing.key).map(film => s"venue|$film|${listing.venue}")) else listing
      }
      Case(listings, corpus.lookups, families,
        venuePosters = listings.flatMap(l => l.poster.zip(corpus.truth.get(l.key)).map { case (url, film) => url -> PosterHash(poster(film).bits ^ 0x3L) }).toMap,
        filmPosters = corpus.films.flatMap(f => f.tmdbId.map(_ -> Seq(poster(f.id)))).toMap,
        finds = corpus.films.flatMap(f => f.tmdbId.map(imdbOf(f) -> _)).toMap)
    }
  }

  /** The shapes production met, each a case every seed's history runs in its own order. */
  object Recorded {
    private val day = java.time.LocalDate.parse("2026-12-20")

    /** US "SEVENTEEN World Tour 'NEW_'" ×391: IMDb alone holds the tour film, billed widely as a current release — filled
     *  by `venues.current` while the film is this year's or last; a new year that makes it older takes it no more. */
    val fill: Case = {
      val tour     = IdentityMeasures.Film("Seventeen World Tour (New_)", None, Nil, Some(2025), None, None, None, None)
      val listings = Seq(KinoMuza, Multikino, KinoPalacowe).map(venue => FilmTable.listing(venue, "SEVENTEEN World Tour 'NEW_'"))
      Case(listings, new FilmTable(Nil, normalizer), Map(VoterFamily.Imdb -> Map("tt46658626" -> SourceRecord(tour, Map("imdb" -> "tt46658626")))) ++
        Seq(VoterFamily.Wiki, VoterFamily.Filmweb, VoterFamily.RottenTomatoes).map(_ -> Map.empty[String, SourceRecord]),
        rules = UnifiedRules("t", UnifiedEvidence.Guards, Seq("venues.current")))
    }

    /** PL Kino za Rogiem's "Lalka" (2026-10-06): the model takes Kawalski's 2026 film; the venue's poster is Has's 1968
     *  one, and IMDb and Wikidata take Has's — corrected to it. */
    val correction: Case = {
      val table   = new FilmTable(Seq(FilmTable.F(1001, "Lalka", 2026, "Maciej Kawalski", 162, popularity = 95.0),
        FilmTable.F(1002, "Lalka", 1968, "Wojciech Has", 159, popularity = 2.0)), normalizer)
      val url     = "https://kino.example/lalka.jpg"
      val shown   = PosterHash(0x5a5a5a5a5a5aL)
      val has     = SourceRecord(IdentityMeasures.Film("Lalka", None, Seq("The Doll"), Some(1968), Some(159), Some(Seq("Wojciech Has")), None, None), Map("tmdb" -> "1002"))
      val listing = FilmTable.listing(Multikino, "Lalka", runtime = Some(162)).copy(poster = Some(url), screenings = ScreeningDays.of(Seq(day)))
      Case(Seq(listing), table, VoterFamily.values.map(family => family -> Map.empty[String, SourceRecord]).toMap ++
        Seq(VoterFamily.Imdb, VoterFamily.Wiki).map(_ -> Map("x" -> has)),
        venuePosters = Map(url -> shown), filmPosters = Map(1001 -> Seq(PosterHash(-1L)), 1002 -> Seq(PosterHash(shown.bits ^ 0x7L))))
    }

    val all: Seq[(String, Long => Case)] = Seq("fill" -> (_ => fill), "correction" -> (_ => correction))
  }

  /** The last round anything is filed at, and the round the clock crosses into a new year at. */
  val Final   = 4
  val NewYear = 3

  /**
   * A case's inputs at a round: every answer — a TMDB search or record, a family's question, a poster, an IMDb id's find —
   * is filed at a round its own text fixes (most at the start), some never; some are ATTEMPTED (asked and failed) a round
   * before they are filed, or for good. The clock crosses into a new year at round [[NewYear]]. Everything is a function
   * of the seed, the question and the round: the world keeps no history, so a whole resolve reads what is filed now.
   */
  final class World(seed: Long, of: Case, order: Long = 0, canonical: String => String = identity) {
    var round = 0
    /** The clock kept in its first year whatever the round: a history of filings alone, the year no input that moves. */
    var yearHeld = false
    /** `question` as the world schedules it: by its `canonical` text, so a corpus relabelled is filed as the original. */
    private def roll(salt: String, question: String, n: Int): Int = (MurmurHash3.stringHash(s"$seed|$salt|${canonical(question)}") & 0x7fffffff) % n
    /** The round `question` is filed at: 0 for most, a later one for some, never for a few — but TMDB's, which the
     *  model's every take waits on: a cluster with one of them unanswered is never the stage's to read. */
    def filedAt(question: String): Int = roll("filed", question, 20) match {
      case n if n < 11                              => 0
      // filed late: at a round its text fixes -- or, in another filing ORDER, at another (property 2): the same filings
      case n if n < 19 || question.startsWith("tmdb|") => if (order == 0) 1 + n % Final else 1 + roll(s"order$order", question, Final)
      case _                                        => Int.MaxValue
    }
    def filed(question: String): Boolean = round >= filedAt(question)
    /** Asked and failed: a round before its filing, for some; for good, for half of those never filed. */
    def attempted(question: String): Boolean = filed(question) || (roll("tried", question, 2) == 0 && round >= (filedAt(question) match {
      case Int.MaxValue => 1 + roll("when", question, Final)
      case at           => at - 1
    }))

    /** The clock: the last day of a year before [[NewYear]], the first of the next from it. */
    val clock: java.time.Clock = new java.time.Clock {
      def getZone: java.time.ZoneId = java.time.ZoneOffset.UTC
      override def withZone(zone: java.time.ZoneId): java.time.Clock = this
      def instant(): java.time.Instant = java.time.Instant.parse(if (round >= NewYear && !yearHeld) "2027-01-01T12:00:00Z" else "2026-12-31T12:00:00Z")
    }

    /** TMDB, as the model and the stage read it: the case's, each question a gap until filed. */
    val lookups: IdentityLookups = new IdentityLookups {
      def hasDetail(listing: Listing): Boolean = of.tmdb.hasDetail(listing)
      def detail(listing: Listing): Answer[Option[DetailFacts]] = of.tmdb.detail(listing)
      def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
        if (filed(s"tmdb|${query.sortKey}")) of.tmdb.candidates(query) else Answer.Unknown
      def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
        if (filed(s"tmdb|film|$tmdbId")) of.tmdb.film(tmdbId) else Answer.Unknown
    }
    /** What this round filed of TMDB's questions among those asked: the model is told of exactly these. */
    def filedNow(asked: Iterable[CandidateQuery], records: Iterable[Int]): AnswersChanged =
      AnswersChanged(asked.filter(q => filedAt(s"tmdb|${q.sortKey}") == round).toSet, records.filter(id => filedAt(s"tmdb|film|$id") == round).toSet)

    /** TMDB's find of an IMDb id, a gap until filed. */
    val tmdbOf: String => Answer[Option[Int]] = imdb => if (filed(s"find|$imdb")) Answer.Known(of.finds.get(imdb)) else Answer.Unknown

    /** Each family answering from its records, each question a gap until filed. */
    val families: Map[VoterFamily, FamilyAnswers] = of.families.map { case (voter, records) =>
      val held = new HeldFamilyAnswers(voter, records)
      voter -> new FamilyAnswers {
        def family: VoterFamily = voter
        private def gated[A](question: String)(answer: => Answer[A]): Answer[A] = if (filed(s"${voter.label}|$question")) answer else Answer.Unknown
        def titled(text: String): Answer[Seq[SourceHit]]     = gated(s"title|$text")(held.titled(text))
        def directedBy(name: String): Answer[Seq[SourceHit]] = gated(s"director|$name")(held.directedBy(name))
        def record(id: String): Answer[Option[SourceRecord]] = gated(s"record|$id")(held.record(id))
        override def fresh(question: String): Boolean        = filed(s"${voter.label}|$question")
      }
    }

    /** The posters' hashes, each a gap until filed. */
    val posters: PosterAnswers = new PosterAnswers {
      private def gated[A](question: AgreementStage.PosterQuestion)(answer: => A): Answer[A] =
        if (filed(PosterAnswers.idOf(question))) Answer.Known(answer) else Answer.Unknown
      def venue(url: String): Answer[Option[PosterHash]] = gated(AgreementStage.PosterQuestion.Venue(url))(of.venuePosters.get(url))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = gated(AgreementStage.PosterQuestion.Film(tmdbId))(of.filmPosters.getOrElse(tmdbId, Nil))
    }

    /** Given up on: asked and failed for good -- the memory's give-up after its failures in a row -- never filed. One that
     *  failed a round before its filing is in its backoff then: pending, not absent (P1). */
    def givenUp(question: String): Boolean = filedAt(question) == Int.MaxValue && attempted(question)
    /** The question memory as the stage reads it: attempted -- answered, given up on, or failed and in its backoff -- by
     *  the id its outcome is kept under; the settle rule decides without an attempted one. */
    val tried: Question => Boolean = question => question.id.exists(attempted)
    /** ...and given up on: absent, where one in its backoff is pending (P1). */
    val gaveUp: Question => Boolean = question => question.id.exists(givenUp)
  }

  /** One random history over a case: the incremental path driven event by event, and the whole resolve. */
  final class History(of: Case, seed: Long, budget: Budget, order: Long = 0, canonical: String => String = identity) {
    private val world    = new World(seed, of, order, canonical)
    private val rnd      = new Random(seed * 7919)
    private val held     = mutable.LinkedHashMap.empty[ListingKey, Listing]
    /** The listings that left whose facts are kept ([[services.identity.ListingRetention]]). */
    private val retained = mutable.Set.empty[ListingKey]
    private val pending  = mutable.Queue.from(rnd.shuffle(of.listings))
    private val asked    = mutable.Set.empty[CandidateQuery]
    private val recorded = mutable.Set.empty[Int]
    private val modelStore = new InMemoryIdentityModelStore
    private val verdicts   = new InMemoryAgreementVerdicts
    private var keys: AgreementKeys = new InMemoryAgreementKeys

    /** The model's lookups, noting what it asked: what a round's filing announces to it. */
    private val noted: IdentityLookups = new services.identity.ForwardingIdentityLookups(world.lookups) {
      override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = { asked += query; super.candidates(query) }
      override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = { recorded += tmdbId; super.film(tmdbId) }
    }
    private def newModel() = new IncrementalResolver(noted, normalizer, calibration, decorations = TitleDecorations.None, store = modelStore, rules = "r")
    private def newStage(stored: AgreementVerdicts, keyStore: AgreementKeys, perApply: Int) =
      new AgreementStage(world.families, world.lookups, normalizer, IdentityCalibration.resolver, world.tmdbOf, stored, clock = world.clock,
        posters = world.posters, tmdb = Some(world.lookups), identities = Seq(VoterFamily.Wiki), rules = of.rules, correctionsPerApply = perApply,
        attempted = world.tried, givenUp = world.gaveUp, keyStore = keyStore)
    private var model = newModel()
    private var stage = newStage(verdicts, keys, budget.perApply)
    private def listingOf(key: ListingKey) = held.get(key)

    /** The incremental take of the inputs now: the stage applied to the model's resolution. */
    private def applied(): Resolution = stage.apply(model.resolution, listingOf, world.round)

    /** The whole resolve of the inputs now: the model's resolve of the listings held, a fresh stage applied to it — once,
     *  or (`settled`) until it waits on nothing not attempted yet. */
    private def whole(settled: Boolean): Resolution = {
      val resolution = IdentityResolver.resolveWith(held.values, world.lookups, normalizer, calibration, IdentityResolver.Mutation.None,
        decorations = TitleDecorations.None)
      val fresh = newStage(new InMemoryAgreementVerdicts, new InMemoryAgreementKeys, Int.MaxValue)
      if (settled) settle(fresh, resolution) else fresh.apply(resolution, listingOf, world.round)
    }

    /** `on` applied to `resolution` until it has no backlog and waits on nothing that is not attempted. */
    private def settle(on: AgreementStage, resolution: => Resolution): Resolution =
      Iterator.continually(on.apply(resolution, listingOf, world.round)).zipWithIndex
        .find { case (_, i) => i > 40 || (!on.backlog && on.wanted.forall(world.tried)) }.get._1

    /** Restart: a new model restored over its store, a new stage over the stored verdicts, with the key store kept, lost or
     *  numbered otherwise. */
    private def restart(perApply: Int = budget.perApply): String = {
      keys = rnd.nextInt(3) match {
        case 0 => keys
        case 1 => new InMemoryAgreementKeys
        case _ => renumbered(keys)
      }
      model = newModel()
      model.restore(held.values.toSeq)
      stage = newStage(verdicts, keys, perApply)
      "restart"
    }

    /** One event of the history: a batch arriving, some leaving (to come back), a round of filings, a restart. */
    private def event(): String = rnd.nextInt(14) match {
      case 0 | 1 | 2 | 3 if pending.nonEmpty =>
        val batch = Seq.fill(1 + rnd.nextInt(6))(()).flatMap(_ => Option.when(pending.nonEmpty)(pending.dequeue()))
        batch.foreach(listing => held(listing.key) = listing)
        retained --= batch.map(_.key)                                     // back: its current data replaces what was kept
        model.batch(seen = batch); s"seen ${batch.size}"
      case 4 if held.keys.exists(k => !retained(k)) =>
        // a listing leaving: its showings dropped, its facts kept while a present listing bills its title (ListingRetention),
        // as the worker's drain feeds the model
        val gone    = rnd.shuffle(held.keys.filterNot(retained).toSeq).take(1 + rnd.nextInt(3))
        val leaving = gone.flatMap(held.remove)
        pending ++= leaving
        val left = services.identity.ListingRetention.leave(leaving, held.values.filterNot(l => retained(l.key)), retained.toSeq.flatMap(held.get), normalizer)
        left.dropped.foreach(held.remove); retained --= left.dropped
        left.kept.foreach(l => held(l.key) = l); retained ++= left.kept.map(_.key)
        model.batch(seen = left.kept, gone = left.dropped); s"gone ${gone.size}"
      case 5 | 6 if world.round < Final => nextRound()
      case 7                            => restart()
      case _                            => "tick"
    }

    private def nextRound(): String = {
      world.round += 1
      model.batch(answered = world.filedNow(asked, recorded)); s"round ${world.round}"
    }

    /** The rest of the history: every listing held, every round filed. */
    private def finish(): String = {
      if (pending.nonEmpty) { val rest = pending.dequeueAll(_ => true); rest.foreach(l => held(l.key) = l); retained --= rest.map(_.key); model.batch(seen = rest) }
      while (world.round < Final) nextRound()
      "finish"
    }

    /** P2, monotone under filing (identity-flips-structural-2026-10-30, property 1): every listing held from the start,
     *  then only filings -- no listing leaving, no answer changing -- and each listing's outcome in the whole resolve itself
     *  only refines: Pending* then one decided value (a film, or no film). Never one decided value then another, nor a
     *  decided value then Pending: a decision made before an answer it reads was filed. */
    def monotoneUnderFiling(): Option[String] = {
      val outcomes = filingOutcomes()
      def refines(seq: Vector[String]): Boolean = { val decided = seq.dropWhile(isPending); decided.distinct.sizeIs <= 1 && !decided.exists(isPending) }
      outcomes.collectFirst { case (key, seq) if !refines(seq) => s"seed $seed, ${key.venue} | ${key.rawTitle}: ${seq.mkString(" -> ")}" }
    }

    /** Every listing held, then only filings, the year held: each listing's outcome in the whole resolve at each round. */
    def filingOutcomes(): collection.Map[ListingKey, Vector[String]] = {
      val all = pending.dequeueAll(_ => true)
      all.foreach(listing => held(listing.key) = listing)
      world.yearHeld = true                                              // only filings: the year is an input, kept
      val outcomes = mutable.LinkedHashMap.empty[ListingKey, Vector[String]]
      def record(): Unit = {
        val resolution = IdentityResolver.resolveWith(held.values, world.lookups, normalizer, calibration, IdentityResolver.Mutation.None,
          decorations = TitleDecorations.None)
        val fresh = newStage(new InMemoryAgreementVerdicts, new InMemoryAgreementKeys, Int.MaxValue)
        fresh.apply(resolution, listingOf, world.round).decisions.foreach { d =>
          val outcome = outcomeString(fresh.outcomeOf(d), d)
          d.members.foreach(key => outcomes(key) = outcomes.getOrElse(key, Vector.empty) :+ outcome)
        }
      }
      record()
      while (world.round < Final) { world.round += 1; record() }
      outcomes
    }

    /** One event of the history, the stage applied after it, and each listing's outcome then: a film, "none", or
     *  "pending:<the film served provisionally>" ([[AgreementStage.outcomeOf]], [[outcomeString]]). */
    def outcomesAfter(last: Boolean): (String, Map[ListingKey, String]) = {
      val label = if (last) finish() else event()
      val r     = applied()
      label -> r.decisions.iterator.flatMap { d =>
        val outcome = outcomeString(stage.outcomeOf(d), d)
        d.members.iterator.map(_ -> outcome)
      }.toMap
    }

    /** After every event, the incremental take of the inputs equals the whole resolve's of the same inputs -- a Pending
     *  one too, film and all: the decision on the inputs as they stand, never a previous take (step 6). `provisional` is
     *  told of each listing served a film while Pending. */
    def stepwise(steps: Int = 30, provisional: () => Unit = () => ()): Option[String] =
      (Iterator.range(0, steps).map(_ => event()) ++ Iterator(()).map(_ => finish())).zipWithIndex.flatMap { case (label, step) =>
        val now = applied()
        now.decisions.foreach(d => if (servedOf(d) != "none" && stage.outcomeOf(d).isInstanceOf[services.identity.TakeOutcome.Pending])
          d.members.foreach(_ => provisional()))
        // what the projection reads (AgreementStage.typed): each decision Pending exactly where its served outcome is,
        // and the same objects when typed again
        val typed  = stage.typed(now)
        val mistyped = now.decisions.zip(typed.decisions).collectFirst {
          case (d, t) if stage.outcomeOf(d).isInstanceOf[services.identity.TakeOutcome.Pending] != t.awaits.nonEmpty || t.film != d.film =>
            s"seed $seed, step $step ($label): ${d.members.head.rawTitle} typed ${t.awaits} for ${stage.outcomeOf(d)}" }
          .orElse(Option.when(!stage.typed(now).decisions.corresponds(typed.decisions)(_ eq _))(s"seed $seed, step $step ($label): typed anew"))
        mistyped.orElse(difference(now, whole(settled = false)).map(d => s"seed $seed, step $step ($label): $d"))
      }.nextOption()

    /** At the end of a history under the stage's budget, settled, the incremental take equals the whole resolve's, settled. */
    def settledEnd(steps: Int = 30): Option[String] = {
      Iterator.range(0, steps).foreach { _ => event(); applied() }
      finish()
      difference(settle(stage, model.resolution), whole(settled = true)).map(d => s"seed $seed, settled: $d")
    }

    /** Every restart alone — no input moved — hands back the takes the stage had before it, its first apply reading no
     *  model take afresh (a deploy's budget spent on what moved). */
    def restartsAlone(steps: Int = 30): Option[String] =
      Iterator.range(0, steps).flatMap { step =>
        if (rnd.nextInt(4) == 0) {
          val before = takes(settle(stage, model.resolution))
          restart(Budget.Spent.perApply)
          val after = takes(applied())
          stage = newStage(verdicts, keys, budget.perApply)
          Option.when(after != before)(s"seed $seed, step $step: a restart moved ${(after.toSet -- before).take(2).mkString("; ")}")
        } else { event(); applied(); None }
      }.nextOption()

    /** After every event, a twin stage over stores of its own groups, takes and waits as the stage does, though no name is
     *  the same: its counter starts elsewhere, and on every restart its keys are lost, numbered otherwise or permuted (the
     *  oldest made the youngest), its first apply then reading no model take afresh. Names only NAME
     *  (docs/design/identity-no-drift.md): a grouping, take or wait that read one would part the two; with `asking`, so
     *  would the questions it wants asked. `parted` hears of a history whose names did part. */
    def namesReadByNothing(asking: Boolean, parted: () => Unit, steps: Int = 30): Option[String] = {
      val shuffle      = new Random(seed * 31)
      val twinVerdicts = new InMemoryAgreementVerdicts
      var twinKeys: AgreementKeys = counted(new InMemoryAgreementKeys, from = 1000000L)
      var twin   = newStage(twinVerdicts, twinKeys, budget.perApply)
      var named  = false
      def waits(on: AgreementStage, resolution: Resolution) =
        resolution.decisions.filter(_.members.nonEmpty).map(d => d.members.sorted(using ListingKey.ordering) -> on.waitingOn(d))
          .sortBy(_._1.head)(using ListingKey.ordering)
      (Iterator.range(0, steps).map(_ => event()) ++ Iterator(()).map(_ => finish())).zipWithIndex.flatMap { case (label, step) =>
        val restarted = Option.when(label == "restart") {
          twinKeys = shuffle.nextInt(3) match {
            case 0 => new InMemoryAgreementKeys
            case 1 => renumbered(twinKeys)
            case _ => permuted(twinKeys)
          }
          // its first apply reading no model take afresh, as a deploy's spent budget: its takes stand on what it keeps alone
          val spent = newStage(twinVerdicts, twinKeys, Budget.Spent.perApply)
          twin = newStage(twinVerdicts, twinKeys, budget.perApply)
          spent.apply(model.resolution, listingOf, world.round)
        }
        val resolution = model.resolution
        val (own, other) = (stage.apply(resolution, listingOf, world.round), twin.apply(resolution, listingOf, world.round))
        named ||= resolution.decisions.exists(d => d.members.nonEmpty && stage.keyOf(d) != twin.keyOf(d))
        def at(what: String) = s"seed $seed, step $step ($label): $what"
        restarted.flatMap(difference(_, own)).map(d => at(s"takes on the restart's spent apply: twin $d"))
          .orElse(difference(other, own).map(d => at(s"takes: twin $d")))
          .orElse(Option.when(waits(twin, resolution) != waits(stage, resolution))(at("waits differ")))
          .orElse(Option.when(asking && twin.wanted != stage.wanted)(at(s"wanted: twin ${(twin.wanted -- stage.wanted).take(2)}, stage ${(stage.wanted -- twin.wanted).take(2)}")))
          .orElse(Option.when(twin.backlog != stage.backlog)(at("backlog differs")))
      }.nextOption().tap(_ => if (named) parted())
    }

    /** Every listing's served take, recorded after every event for the incremental path and the whole resolve alike
     *  ([[TakeFlips]], an event's inputs moved unless it was a restart or a tick): the first listing the incremental path
     *  flips that the whole resolve does not. */
    def flipsBeyondWhole(flipped: () => Unit, steps: Int = 30): Option[String] = {
      val (incremental, wholly) = (new TakeFlips, new TakeFlips)
      def served(resolution: Resolution) =
        resolution.decisions.filter(_.members.nonEmpty).flatMap { d =>
          val film = d.film.map(id => s"tmdb:$id").orElse(d.fallback.map(f => s"${f.source}:${f.id}")).getOrElse(TakeFlips.NoFilm)
          d.members.map(_ -> TakeFlips.Served(film, d.basis.toString))
        }
      (Iterator.range(0, steps).map(_ => event()) ++ Iterator(()).map(_ => finish())).zipWithIndex.foreach { case (label, step) =>
        val moved: ListingKey => Boolean = _ => label != "restart" && label != "tick"
        incremental.record(s"$step $label", served(applied()), moved)
        wholly.record(s"$step $label", served(whole(settled = false)), moved)
      }
      val alsoWhole = wholly.flips.map(_.key).toSet
      if (incremental.flips.nonEmpty) flipped()
      incremental.flips.find(f => !alsoWhole(f.key)).map(f => s"seed $seed: ${f.line}")
    }

    /** The whole resolve at the end of the history, settled: what the cases' reach is read off. */
    def wholeAtEnd(): Resolution = { finish(); whole(settled = true) }
  }

  /** An empty key store whose counter starts at `from`. */
  private def counted(keys: InMemoryAgreementKeys, from: Long): AgreementKeys = { keys.replace(Set.empty, Nil, from); keys }

  /** `keys` permuted: the oldest key made the youngest and so on, so every merge and split the stage meets next is decided
   *  between names ranked the other way round. */
  private def permuted(keys: AgreementKeys): AgreementKeys = {
    val held   = keys.all()
    val sorted = held.keys.map(_.key).sorted
    val swap   = sorted.zip(sorted.reverse).toMap
    val out    = new InMemoryAgreementKeys
    out.replace(Set.empty, held.keys.map(k => StoredKey(swap(k.key), k.first)), held.next)
    out
  }

  /** `keys` numbered otherwise: every key moved past the counter — a restart's keys none of the stored verdicts are under. */
  private def renumbered(keys: AgreementKeys): AgreementKeys = {
    val held = keys.all()
    val out  = new InMemoryAgreementKeys
    out.replace(Set.empty, held.keys.map(k => StoredKey(k.key + held.next + 7, k.first)), (held.next + 7) * 2 + 1)
    out
  }
}
