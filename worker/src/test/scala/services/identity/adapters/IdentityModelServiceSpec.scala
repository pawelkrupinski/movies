package services.identity
package adapters

import tools.SpecTimeouts

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.LoneElement
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.concurrent.duration._

/** The model kept by the pipeline's events: venues' archived scrapes diffed into listings seen and
 *  gone, new observations mapped back to the questions that read them, a restart taking up what
 *  the store kept — after every drain, what a whole resolve of the listings held decides. */
class IdentityModelServiceSpec extends AnyFlatSpec with Matchers with LoneElement {

  private val normalizer  = SingleCountryNormalizer.titleNormalizer
  private val calibration = IdentityCalibration.resolver

  /** A film database whose every answer is read under a store key (`q:`, `f:`), as the observation
   *  store's readers name them — and whose answers a test can change, as the fill does. */
  private final class Keyed(reads: ObservationReads) extends IdentityLookups {
    var titles: Map[String, Seq[Hit]] = Map(
      "lalka"   -> Seq(Hit(1, "Lalka", None, Some(1968), 10), Hit(2, "Lalka", None, Some(2025), 50)),
      "matilda" -> Seq(Hit(3, "Matilda", None, Some(1996), 20)))
    def hasDetail(listing: Listing): Boolean = false
    def detail(listing: Listing): Answer[Option[DetailFacts]] = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]] = {
      reads.read("q:" + query.sortKey)
      query match {
        case CandidateQuery.Title(text) => Answer.Known(titles.collect { case (word, hits) if normalizer.searchQuery(text).toLowerCase(java.util.Locale.ROOT).contains(word) => hits }.flatten.toSeq)
        case _                          => Answer.Known(Nil)
      }
    }
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = {
      reads.read(s"f:$id")
      Answer.Known(titles.values.flatten.find(_.tmdbId == id).map(hit => IdentityMeasures.Film(hit.title, None, Nil, hit.year)))
    }
  }

  private def movie(cinema: Cinema, title: String, year: Option[Int] = None): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, Some(s"${cinema.displayName}/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None)))

  private def listingsOf(scrapes: Map[Cinema, Seq[CinemaMovie]]) =
    ModelRead.corpus(scrapes.toSeq, normalizer)

  private def decided(decisions: Seq[ResolverDecision]) = decisions.map(d => (d.listings, d.film)).toSet

  /** The families' store at the input gate's boundary: which titles' questions are answered, and what was asked. */
  private final class Families extends ListingReadiness {
    var unanswered = Set.empty[String]
    /** The questions an answer led to, by title: a title's search names films whose records the listing then waits on. */
    var ledTo      = Map.empty[String, Set[Question]]
    val asked      = scala.collection.mutable.ArrayBuffer.empty[Question]
    private def question(listing: Listing) = Question.Family(agreement.VoterFamily.Imdb, s"title|${listing.title}")
    def awaiting(listing: Listing): Set[Question] =
      (if (unanswered(listing.title)) Set(question(listing)) else Set.empty) ++ ledTo.getOrElse(listing.title, Set.empty)
    def ask(listing: Listing, questions: Set[Question]): Unit = asked ++= questions
  }

  private final class GateReports extends IdentityModelMetrics {
    var heldReports = Seq.empty[(Int, Double)]
    var promotions  = Seq.empty[(Seq[Double], Boolean)]
    def batch(batch: ModelBatch): Unit = (); def rebuilt(): Unit = (); def takeUpFailed(): Unit = ()
    override def held(count: Int, oldestSeconds: Double): Unit = heldReports :+= (count -> oldestSeconds)
    override def promoted(heldSeconds: Seq[Double], atBoot: Boolean): Unit = promotions :+= (heldSeconds -> atBoot)
    var bounds = Seq.empty[Seq[Double]]
    override def bounded(heldSeconds: Seq[Double]): Unit = bounds :+= heldSeconds
  }

  /** `world`, its services' woken drains run nowhere: the spec drains by hand alone, so no drain on a thread of its own races
   *  the spec's (a raced drain promoted at a drain what a take-up was to promote at boot). No `peek`: it asks the model's
   *  thread, which runs nothing here. */
  private def handDrained(world: World): World = {
    val clock = new _root_.tools.MutableClock(java.time.Instant.parse("2026-11-06T10:00:00Z"))
    world.clock = clock
    world.scheduler = () => new _root_.tools.ManualScheduler(clock)
    world
  }

  private final class World {
    val reads   = new ObservationReads
    val lookups = new Keyed(reads)
    val store   = new InMemoryIdentityModelStore
    /** The kept listings' store, which outlives a service as `identity_retained` outlives a worker. */
    var retained: RetainedListingStore = new RetainedListingStore.InMemory()
    var scrapes = Map.empty[Cinema, Seq[CinemaMovie]]
    var pageWait: PageWait = PageWait.Never
    var clock: java.time.Clock = _root_.tools.SpecClock.Pinned
    /** The families' answers as the input gate reads them: a listing waits on one question per title not answered yet. */
    val families = new Families
    /** The held listings' store, which outlives a service as `identity_pending` outlives a worker. */
    val pendingStore = new PendingListingStore.InMemory
    var newWhilePending: IdentityModelService.NewWhilePending = IdentityModelService.NewWhilePending.Shown
    /** What the gate tells its metrics: the held count and oldest wait after each drain, and each promotion. */
    val gate = new GateReports
    /** Where a service's woken drains run: a thread of its own, or -- for a spec that drains by hand alone -- nowhere. */
    var scheduler: () => java.util.concurrent.ScheduledExecutorService = () => Executors.newSingleThreadScheduledExecutor()
    def service(): IdentityModelService = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(lookups, reads), normalizer, calibration, store = store),
      reads, () => listingsOf(scrapes), normalizer, 1.second, scheduler(), pageWait = pageWait,
      clock = clock, retainedStore = retained, readiness = families, pendingStore = pendingStore, newWhilePending = newWhilePending,
      metrics = gate)
    def scrape(service: IdentityModelService, cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
      scrapes += cinema -> films; service.venueScraped(cinema, films)
    }
    def expected: Set[(Set[ListingKey], Option[Int])] =
      decided(IdentityResolver.resolve(listingsOf(scrapes), lookups, normalizer, ResolveInputs.Unpinned.copy(calibration = calibration)).decisions)
  }

  "the model service" should "decide as a whole resolve after each drain of venues' scrapes, and sweep a film a venue stopped listing" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain().map(_.venues) shouldBe Some(2)
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected

    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))           // Matilda no longer listed
    service.drain()
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    service.peek(10.seconds).get.resolution.decisions.flatMap(_.film) should not contain 3
  }

  it should "re-read exactly the questions a new observation answers" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Matilda")))
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain()
    world.lookups.titles += "matilda" -> Seq(Hit(4, "Matilda", None, Some(2022), 90))       // the fill filed a new answer
    val matilda = world.reads.changedBy(Seq("q:" + CandidateQuery.Title("Matilda").sortKey))
    service.observed("q:" + CandidateQuery.Title("Matilda").sortKey)
    val batch = service.drain()
    batch.map(_.observations) shouldBe Some(1)
    batch.map(_.familiesResolved) shouldBe Some(1)                                        // Matilda's family, not Lalka's
    decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    matilda.queries should not be empty
  }

  it should "take up after a restart what its store kept, and decide as before" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(first, Helios, Seq(movie(Helios, "Lalka")))
    first.drain()
    val again = world.service()
    again.takeUp()
    decided(again.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    again.drain() shouldBe None                                                           // nothing queued, nothing to do
  }

  // The owner's lifetime of identity facts (2026-10-30, ListingRetention): "Drop showings, keep movie details from all
  // cinemas data until there are no showtimes in any cinema in the country", "and update the details when it comes back".
  it should "keep a leaving listing's facts while another venue bills its title, update them when it comes back, and drop them with the last" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    def held(cinema: Cinema) = service.peek(10.seconds).get.resolution.decisions.flatMap(_.listings).filter(_.venue == cinema.displayName)
    def filmOf(cinema: Cinema) = service.peek(10.seconds).get.resolution.decisions.find(_.listings.exists(_.venue == cinema.displayName)).flatMap(_.film)
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain()
    filmOf(Helios) shouldBe Some(2)                                              // the dated listing's 2025 film
    world.scrape(service, Multikino, Nil)                                        // Multikino's run ends; Helios bills it still
    service.drain()
    held(Multikino) should not be empty                                          // its facts kept, its showings dropped
    filmOf(Helios) shouldBe Some(2)
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))  // back, its details changed
    service.drain()
    filmOf(Multikino) shouldBe Some(1)                                           // the current data wins, never the kept copy
    world.scrape(service, Multikino, Nil)
    world.scrape(service, Helios, Nil)                                           // no venue bills it: every fact goes
    service.drain()
    held(Multikino) shouldBe empty
    held(Helios) shouldBe empty
  }

  // A row's model input is production's: the listings billed AND the ones kept. A worker's restart, or a replay of a
  // recording (whose capture of the kept set is this store, seeded: ArchiveReplayWiring.recordedRetained), takes up
  // the kept ones too -- until no venue bills their title.
  it should "take up, after a restart, a listing whose showtimes ended while another venue bills its title, and drop it once none does" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(first, Helios, Seq(movie(Helios, "Lalka")))
    first.drain()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Matilda")))                 // Lalka's showtimes at Multikino end
    first.drain()
    val again = world.service()                                                     // a deploy: the model taken up anew
    again.takeUp()
    def held(service: IdentityModelService) = service.peek(10.seconds).get.listings.map(_.key)
    val kept = ListingKey.of(Multikino, movie(Multikino, "Lalka", Some(2025)))
    held(again) should contain(kept)                                                // Helios bills "Lalka": its facts stay
    again.peek(10.seconds).get.resolution.decisions.find(_.listings.exists(_.venue == Helios.displayName)).flatMap(_.film) shouldBe Some(2)
    world.scrape(again, Helios, Nil)                                                // no venue bills it any more
    again.drain()
    held(again) should not contain kept
    world.retained.read().get.map(_.listing.key) should contain(kept)               // still an input: kept again if billed
  }

  // R1, no stickiness (flip-unification design §7, retention as a pure function): the listings kept are a function of
  // the inputs -- the listings billed, and those that left with their last-seen facts -- never of the order the leaves
  // and arrivals came in. Multikino's "Lalka (2025)" leaving before Kinoteka bills "Lalka" kept nothing and stayed so;
  // Kinoteka first kept it. Every order now holds what a fresh take-up of the same inputs (the whole resolve) holds.
  it should "keep the same listings and decide the same whatever order the leaves and arrivals come in, as a fresh take-up does" in {
    val start  = Map[Cinema, Seq[CinemaMovie]](Multikino -> Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")),
      Helios -> Seq(movie(Helios, "Matilda"), movie(Helios, "Lalka")), Kinoteka -> Nil)
    val finish = Map[Cinema, Seq[CinemaMovie]](Multikino -> Seq(movie(Multikino, "Matilda")), Helios -> Seq(movie(Helios, "Matilda")),
      Kinoteka -> Seq(movie(Kinoteka, "Lalka")))
    def outcome(service: IdentityModelService) = {
      val model = service.peek(10.seconds).get
      (model.listings.map(_.key).toSet, decided(model.resolution.decisions))
    }
    val outcomes = start.keys.toSeq.permutations.map { order =>
      val world   = new World
      val service = world.service()
      service.takeUp()
      start.foreach { case (cinema, films) => world.scrape(service, cinema, films) }
      service.drain()
      order.foreach { cinema => world.scrape(service, cinema, finish(cinema)); service.drain() }
      val whole = world.service()                                    // a fresh take-up of the same inputs
      whole.takeUp()
      outcome(whole) shouldBe outcome(service)
      order.map(_.displayName) -> outcome(service)
    }.toSeq
    val kept = ListingKey.of(Multikino, movie(Multikino, "Lalka", Some(2025)))
    all(outcomes.map(_._2._1)) should contain(kept)                   // Kinoteka bills "Lalka": Multikino's facts stay
    outcomes.map(_._2).distinct.size shouldBe 1
  }

  // A take-up asks every unread page of what it takes up, as `admit` asks a new listing's: a listing taken up -- billed
  // in the archive, or kept after its showtimes ended -- is decided Pending on its page, and nothing else asks for it.
  // Verify scrape recording #10 (US): kept Alamo Drafthouse listings no film row names held their families' takes
  // provisional at rest, their pages never asked, until the next day's re-scrape moved the families.
  it should "ask, at take-up, the unread venue page of every listing it takes up, billed or kept" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
    world.scrape(first, Helios, Seq(movie(Helios, "Lalka")))
    first.drain()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Matilda")))                 // Lalka's showtimes at Multikino end
    first.drain()
    val kept   = ListingKey.of(Multikino, movie(Multikino, "Lalka", Some(2025)))
    val billed = ListingKey.of(Helios, movie(Helios, "Lalka"))
    val pages  = new Pages
    pages.unread   = Set(kept, billed)
    world.pageWait = pages
    val again = world.service()                                                     // a deploy: the model taken up anew
    again.takeUp()
    again.peek(10.seconds).get.listings.map(_.key) should contain allOf (kept, billed)
    pages.requested.toSet shouldBe Set(kept, billed)
  }

  it should "resume no recorded kept listing whose title no venue bills" in {
    val world = new World
    world.scrapes = Map(Helios -> Seq(movie(Helios, "Matilda")))
    val stored  = listingsOf(Map(Multikino -> Seq(movie(Multikino, "Lalka", Some(2025)))))
    world.retained = new RetainedListingStore.InMemory(Some(RetainedListingStore.undated(stored)))   // written before 2026-11-03
    val service = world.service()
    service.takeUp()
    service.peek(10.seconds).get.listings.map(_.key.venue) shouldBe Seq(Helios.displayName)
    // migrated, not lost: dated to the take-up, an input a later "Lalka" keeps again
    world.retained.read().get.map(d => (d.listing.key, d.at)) shouldBe stored.map(_.key -> _root_.tools.SpecClock.Pinned.millis())
  }

  // A replay of a recording takes up its capture of the kept set on the harness's fixed day, months before the corpus's
  // next day, where the next-day row moves its clock. Dated to the take-up, every recorded listing aged past MaxAge at
  // that move and left its family: Verify scrape recording #11 (run 42034527957), US "Reservoir Dogs", "Cape Fear" and
  // "Play Misty for Me" lost the Pending a kept listing's unread page held, their own listings unchanged. Dated at the
  // recording (ProdRetained.departures), a day after it ages them by a day, as in production.
  it should "still hold a recorded kept listing a day after the recording, whatever day the replay took it up on" in {
    val world      = new World
    val recordedAt = java.time.Instant.parse("2026-11-05T03:38:04Z")
    val clock      = new _root_.tools.MutableClock(java.time.Instant.parse("2026-06-08T12:00:00Z"))
    world.clock    = clock
    world.scrapes  = Map(Helios -> Seq(movie(Helios, "Lalka")))
    val stored     = listingsOf(Map(Multikino -> Seq(movie(Multikino, "Lalka", Some(2025)))))
    world.retained = new RetainedListingStore.InMemory(Some(_root_.tools.ProdRetained.departures(stored, recordedAt)))
    val service    = world.service()
    service.takeUp()
    val kept = ListingKey.of(Multikino, movie(Multikino, "Lalka", Some(2025)))
    service.peek(10.seconds).get.listings.map(_.key) should contain(kept)
    clock.setTo(recordedAt.plus(java.time.Duration.ofDays(1)))                      // the next day
    world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
    service.drain()
    service.peek(10.seconds).get.listings.map(_.key) should contain(kept)           // Helios still bills "Lalka"
  }

  it should "forget, in the reads index, the questions of a film no venue lists any more" in {
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    service.drain()
    val before = world.reads.keys
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka")))                      // Matilda gone
    service.drain()
    world.reads.keys should be < before
    world.reads.changedBy(Seq("q:" + CandidateQuery.Title("Matilda").sortKey)).queries shouldBe empty
  }

  it should "report its families as soon as it has taken up its model, before any event" in {
    val world    = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    val reported = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service  = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = reported += batch; def rebuilt(): Unit = (); def takeUpFailed(): Unit = () }, clock = _root_.tools.SpecClock.Pinned)
    service.takeUp()
    reported.map(_.families) shouldBe Seq(2)
  }

  it should "tell whoever projects the model of each drain that moved something, and of no other" in {
    val world   = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka")))
    val batches = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
      batched = batches += _, clock = _root_.tools.SpecClock.Pinned)
    service.takeUp()
    batches.clear()
    service.drain()
    batches shouldBe empty                                         // nothing queued
    service.venueScraped(Multikino, Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    service.drain()
    batches.loneElement.moved shouldBe true
  }

  it should "report how large its families are: the largest, its busiest node, and none past a region" in {
    val world    = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")), Helios -> Seq(movie(Helios, "Lalka")))
    val reported = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service  = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = reported += batch; def rebuilt(): Unit = (); def takeUpFailed(): Unit = () }, clock = _root_.tools.SpecClock.Pinned)
    service.takeUp()
    val sizes = reported.loneElement.sizes
    sizes.largest.map(_.listings) shouldBe Seq(2, 1)
    sizes.largest.head.busiest.map(_._2) shouldBe Seq(2)                                   // both Lalkas one node
    (sizes.largestListings, sizes.largestNodes, sizes.large) shouldBe ((2, 1, 0))
  }

  it should "drain nothing before it has taken up its model" in {
    val world   = new World
    val service = world.service()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka")))
    service.drain() shouldBe None
    service.takeUp()
    service.drain().map(_.venues) shouldBe Some(1)
  }

  // A worker's readiness waits on the take-up (the next workers roll out only once it settles), so
  // a take-up that FAILED must count as settled too, or one bad boot would hold every rollout.
  it should "say its take-up has settled once it finished, even when it failed" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val service   = new IdentityModelService(
      () => throw new IllegalStateException("store unreachable"),
      world.reads, () => Nil, normalizer, 1.hour, scheduler, clock = _root_.tools.SpecClock.Pinned)
    try {
      service.takeUpSettled shouldBe false
      service.start()
      scheduler.submit((() => ()): Runnable).get(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)   // behind the take-up
      service.takeUpSettled shouldBe true
    } finally scheduler.shutdownNow()
  }

  // The boot's projection waits on this signal, not a timer: every projection on scrapes waits for that first one, so a
  // take-up that failed must announce itself when its retry succeeds, and a failed one never.
  it should "announce each take-up that succeeded — a retried one too — and none that failed" in {
    val world   = new World
    val clock   = new tools.MutableClock(java.time.Instant.parse("2026-10-04T00:00:00Z"))
    var failing = true
    var announced = 0
    val service = new IdentityModelService(
      () => { if (failing) throw new IllegalStateException("store unreachable")
              new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store) },
      world.reads, () => Nil, normalizer, 1.hour, Executors.newSingleThreadScheduledExecutor(), clock = clock,
      takenUp = () => announced += 1)
    service.tick(); service.takeUpSettled shouldBe false
    scala.util.Try(service.takeUp())
    announced shouldBe 0
    failing = false
    service.takeUp()
    announced shouldBe 1
  }

  // A shadow country's readers only peek, which never takes up: a failed take-up (a Mongo blip at boot)
  // left its model down until the next restart. The tick retries it, backing off 1, 2, 4 … 30 minutes.
  it should "retry a failed take-up on its tick, backing off to a cap, until one succeeds" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val clock     = new tools.MutableClock(java.time.Instant.parse("2026-10-03T00:00:00Z"))
    var failing   = true
    val attempts  = scala.collection.mutable.ArrayBuffer.empty[Long]
    val started   = clock.instant()
    var failures  = 0
    val service   = new IdentityModelService(
      () => {
        attempts += java.time.Duration.between(started, clock.instant()).toMinutes
        if (failing) throw new IllegalStateException("store unreachable")
        new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store)
      },
      world.reads, () => Nil, normalizer, 1.hour, scheduler, clock = clock,
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = (); def rebuilt(): Unit = (); def takeUpFailed(): Unit = failures += 1 })
    try {
      service.current(10.seconds) shouldBe None                   // the take-up, failed: tried once
      attempts.toSeq shouldBe Seq(0L)
      (1 to 125).foreach { _ => clock.advanceSeconds(60); service.tick() }
      attempts.toSeq.drop(1) shouldBe Seq(1L, 3L, 7L, 15L, 31L, 61L, 91L, 121L)
      service.peek(10.seconds) shouldBe None
      failures shouldBe attempts.size   // a model that is down shows on a dashboard, not only in its log

      failing = false
      (1 to 30).foreach { _ => clock.advanceSeconds(60); service.tick() }
      service.peek(10.seconds) shouldBe defined
      attempts.last shouldBe 151L
    } finally scheduler.shutdownNow()
  }

  // A reader that gave up (a projection's timeout behind a long take-up) must not leave its catch-up
  // queued on the model's thread: every refused projection would add another take-up and snapshot
  // there, run for nobody, each delaying the next reader further.
  it should "drop a reader's request that timed out before the model's thread reached it" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val built     = new java.util.concurrent.atomic.AtomicInteger()
    val service   = new IdentityModelService(
      () => { built.incrementAndGet(); new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration,
        store = world.store) },
      world.reads, () => Nil, normalizer, 1.hour, scheduler, clock = _root_.tools.SpecClock.Pinned)
    val release = new java.util.concurrent.CountDownLatch(1)
    try {
      scheduler.execute(() => release.await())                  // the model's thread, busy (a take-up)
      service.current(50.millis) shouldBe None
      release.countDown()
      scheduler.submit((() => ()): Runnable).get(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
      built.get shouldBe 0
    } finally scheduler.shutdownNow()
  }

  // A failed take-up is retried by the tick alone, on its backoff: a reader's take-up that failed used
  // to be taken up again at once as a "rebuild", and every later reader tried once more — each refused
  // projection held the model's thread for full take-ups (117–259 s each on US) with nothing to drain.
  it should "try a reader's take-up once when it fails, and leave its retry to the tick's backoff" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val built     = new java.util.concurrent.atomic.AtomicInteger()
    val service   = new IdentityModelService(
      () => { built.incrementAndGet(); throw new IllegalStateException("store unreachable") },
      world.reads, () => Nil, normalizer, 1.hour, scheduler, clock = _root_.tools.SpecClock.Pinned)
    try {
      service.current(10.seconds) shouldBe None
      built.get shouldBe 1
      service.current(10.seconds) shouldBe None
      built.get shouldBe 1
    } finally scheduler.shutdownNow()
  }

  // A settle of announced venue pages that fails (one page's read timing out) touches no engine state, and
  // leaves its pages noted for the next settle: rebuilding the whole model over it (minutes on US) was waste.
  it should "go on draining past a failed page settle, without rebuilding the model" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val built     = new java.util.concurrent.atomic.AtomicInteger()
    var settles   = 0
    val service   = new IdentityModelService(
      () => { built.incrementAndGet(); new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration,
        store = world.store) },
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.hour, scheduler,
      beforeDrain = () => { settles += 1; if (settles == 1) throw new IllegalStateException("venue_pages read timed out") }, clock = _root_.tools.SpecClock.Pinned)
    try {
      service.takeUp()
      world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
      service.tick()
      built.get shouldBe 1
      settles shouldBe 1
      decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected
    } finally scheduler.shutdownNow()
  }

  // A drain that throws part-way leaves the engine half-updated: the model is rebuilt from its store, over the archive's
  // listings — the scrape the failed drain had dequeued included — and counted; a rebuild that fails too leaves the model
  // down for the tick's backoff, never a half-updated engine serving.
  it should "rebuild the model from its store when a drain fails, and go down for the backoff when the rebuild fails too" in {
    val world     = new World
    val scheduler = Executors.newSingleThreadScheduledExecutor()
    val built     = new java.util.concurrent.atomic.AtomicInteger()
    var rebuilds  = 0
    var downs     = 0
    var brokenAt  = Set.empty[Int]   // which take-ups (1-based) throw
    var drainFail = false
    val failingPages = new PageWait {
      def awaiting(listing: Listing): Boolean = if (drainFail) { drainFail = false; throw new IllegalStateException("drain broke") } else false
      def request(listing: Listing): Unit = ()
    }
    val service = new IdentityModelService(
      () => { val n = built.incrementAndGet(); if (brokenAt(n)) throw new IllegalStateException("store unreachable")
              new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store) },
      world.reads, () => listingsOf(world.scrapes), normalizer, 1.hour, scheduler, pageWait = failingPages,
      metrics = new IdentityModelMetrics { def batch(batch: ModelBatch): Unit = (); def rebuilt(): Unit = rebuilds += 1; def takeUpFailed(): Unit = downs += 1 },
      clock = _root_.tools.SpecClock.Pinned)
    try {
      service.takeUp()
      drainFail = true
      world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025)), movie(Multikino, "Matilda")))
      service.tick()
      (built.get, rebuilds, downs) shouldBe ((2, 1, 0))
      decided(service.peek(10.seconds).get.resolution.decisions) shouldBe world.expected // the dequeued scrape is in

      brokenAt = Set(3); drainFail = true
      world.scrape(service, Helios, Seq(movie(Helios, "Lalka", Some(2025))))
      service.tick()
      (built.get, rebuilds, downs) shouldBe ((3, 2, 1))
      service.peek(10.seconds) shouldBe None
    } finally scheduler.shutdownNow()
  }

  // ── a new listing's venue page is asked for, and the listing decided meanwhile (a cut-over country) ────────

  /** Which listings' pages are unread, and the pages asked for: what a cut-over country's VenuePageWait
   *  answers from venue_pages and the task queue. */
  private final class Pages extends PageWait {
    var unread    = Set.empty[ListingKey]
    val requested = scala.collection.mutable.ArrayBuffer.empty[ListingKey]
    def awaiting(listing: Listing): Boolean = unread(listing.key)
    def request(listing: Listing): Unit = { requested += listing.key; () }
  }
  private def ticking = new tools.MutableClock(java.time.Instant.parse("2026-10-01T10:00:00Z"))
  private def paging(world: World, pages: Pages, clock: java.time.Clock) = new IdentityModelService(
    () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
    world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
    pageWait = pages, clock = clock)
  private def held(service: IdentityModelService) = service.peek(10.seconds).get.resolution.decisions.flatMap(_.listings).toSet

  // Step 6 (the owner, 2026-10-31): decide from the inputs as they stand. A new listing waited out of the model for its
  // page up to an hour, shown with no film meanwhile; it is taken in at once, its page asked for once.
  "a new listing whose venue page is unread" should "be taken in at once, its page asked for once" in {
    val (world, pages, clock) = (new World, new Pages, ticking)
    val service = paging(world, pages, clock)
    service.takeUp()
    val lalka = movie(Multikino, "Lalka", Some(2025))
    pages.unread = listingsOf(Map(Multikino -> Seq(lalka))).map(_.key).toSet
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    held(service) shouldBe pages.unread
    pages.requested.toSeq shouldBe pages.unread.toSeq
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    pages.requested.size shouldBe 1                                      // asked once, not every drain
  }

  it should "not have its page asked for again once the model holds it, and leave with its venue's listing" in {
    val (world, pages, clock) = (new World, new Pages, ticking)
    val service = paging(world, pages, clock)
    service.takeUp()
    val lalka   = movie(Multikino, "Lalka", Some(2025))
    val matilda = movie(Multikino, "Matilda")
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    val lalkaKey = listingsOf(Map(Multikino -> Seq(lalka))).map(_.key).toSet
    held(service) shouldBe lalkaKey
    // Its page is re-asked later (a refresh): the model keeps it, and asks only the newcomer's page.
    pages.unread = listingsOf(Map(Multikino -> Seq(lalka, matilda))).map(_.key).toSet
    world.scrape(service, Multikino, Seq(lalka, matilda))
    service.drain()
    held(service) shouldBe pages.unread
    pages.requested.toSet shouldBe pages.unread -- lalkaKey
    // Matilda leaves before its page is read: it goes as any listing its venue stops billing.
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    pages.unread = Set.empty
    service.drain()
    held(service) shouldBe lalkaKey
  }

  it should "drain a moment after an event, a burst together within its cap, and never between events" in {
    // No period: the model is woken by what it queues. A burst of scrapes is one batch, as the fixed period batched it; an
    // event alone is taken in seconds later; with nothing arriving, nothing runs.
    val world   = new World
    world.scrapes = Map(Multikino -> Seq(movie(Multikino, "Lalka")))
    val clock   = new _root_.tools.MutableClock(java.time.Instant.parse("2026-10-04T20:00:00Z"))
    val manual  = new _root_.tools.ManualScheduler(clock)
    val batches = scala.collection.mutable.ArrayBuffer.empty[ModelBatch]
    val service = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
      world.reads, () => listingsOf(world.scrapes), normalizer, 10.seconds, manual, batched = batches += _, clock = clock)
    service.start(); manual.runDue()                                // take-up
    batches.clear()
    manual.advance(java.time.Duration.ofHours(1))
    batches shouldBe empty                                          // nothing arrived: nothing ran
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka"), movie(Multikino, "Matilda")))
    manual.advance(java.time.Duration.ofMillis(1999))
    batches shouldBe empty
    manual.advance(java.time.Duration.ofMillis(1))
    batches.loneElement.venues shouldBe 1                           // 2 s after it
    batches.clear()
    (1 to 8).foreach { i =>                                         // a burst, an event every 1.5 s for 12 s
      world.scrape(service, Helios, Seq(movie(Helios, s"Film $i")))
      manual.advance(java.time.Duration.ofMillis(1500))
    }
    batches.size shouldBe 1                                         // the 10 s cap, then the rest after the burst
    manual.advance(java.time.Duration.ofSeconds(2))
    batches.size shouldBe 2
  }

  "the model's input gate" should "keep a re-scraped listing's last ready version until its own questions are answered, then take its new one in with no new scrape" in {
    // docs/design/identity-structural-fixes.md fix 1: a version is taken in once its questions are answered, never decided
    // on an answer still coming (the two-step write: a model "no film" written, the late answer restoring it)
    val world   = new World
    val service = world.service()
    service.takeUp()
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    service.drain()
    def multikino = service.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName)
    multikino.loneElement.year shouldBe Some(2025)
    world.families.unanswered = Set("Lalka")
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))    // its facts moved, its questions open
    service.drain()
    multikino.loneElement.year shouldBe Some(2025)                                    // the ready version stands
    world.families.asked.toSet shouldBe Set(Question.Family(agreement.VoterFamily.Imdb, "title|Lalka"))
    world.families.unanswered = Set.empty                                             // the answer filed
    service.drain()
    multikino.loneElement.year shouldBe Some(1968)
    world.pendingStore.read() shouldBe empty
  }

  it should "ask, ahead, the questions a held listing's answers led to, never leaving them to the agreement's queue" in {
    // Prod, image 5ec9cf7 (2026-11-06 13:16-13:30Z): DE and US held listings were taken in only by the 10-minute bound
    // while their first questions were answered -- a title's search answered names films whose records the listing then
    // waits on, and those were never asked ahead
    val world   = handDrained(new World)
    val service = world.service()
    service.takeUp()
    world.families.unanswered = Set("Lalka")
    world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    service.drain()
    val record = Question.Family(agreement.VoterFamily.Imdb, "record|tt0063215")
    world.families.unanswered = Set.empty; world.families.ledTo = Map("Lalka" -> Set(record))  // the search filed, a record named
    service.drain()
    world.families.asked.toSeq shouldBe Seq(Question.Family(agreement.VoterFamily.Imdb, "title|Lalka"), record)
    service.drain()
    world.families.asked.count(_ == record) shouldBe 1                                            // asked once, not each drain
  }

  it should "show a listing new to the model on its own, Pending on its questions, never joining another's decision -- or not at all" in {
    for (choice <- IdentityModelService.NewWhilePending.values) {
      val world   = new World
      world.newWhilePending = choice
      val service = world.service()
      service.takeUp()
      world.scrape(service, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
      service.drain()
      world.families.unanswered = Set("Lalka")
      world.scrape(service, Helios, Seq(movie(Helios, "Lalka")))
      service.drain()
      val snapshot = service.peek(10.seconds).get
      val helios   = snapshot.resolution.decisions.filter(_.members.exists(_.venue == Helios.displayName))
      if (choice == IdentityModelService.NewWhilePending.Shown) {
        helios.loneElement.members.loneElement.venue shouldBe Helios.displayName
        helios.loneElement.outcome shouldBe a[TakeOutcome.Pending]
      } else helios shouldBe empty
      snapshot.resolution.decisions.filter(_.members.exists(_.venue == Multikino.displayName)).loneElement.members should have size 1
    }
  }

  it should "survive a restart: a version held when the worker stops is held again at boot, its questions asked again, and taken in once answered, with no new scrape" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    first.drain()
    world.families.unanswered = Set("Lalka")
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))
    first.drain()
    world.pendingStore.read().loneElement.ready.flatMap(_.year) shouldBe Some(2025)
    // the worker stops mid-wait: a new service over the same archive and stores
    world.families.asked.clear()
    val again = world.service()
    again.takeUp()
    def multikino = again.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName)
    multikino.loneElement.year shouldBe Some(2025)
    world.families.asked.toSet shouldBe Set(Question.Family(agreement.VoterFamily.Imdb, "title|Lalka"))   // asked again
    world.families.unanswered = Set.empty
    again.drain()
    multikino.loneElement.year shouldBe Some(1968)
    world.pendingStore.read() shouldBe empty
    // a take-up after the promotion reads the same: nothing held, nothing taken twice
    val third = world.service()
    third.takeUp()
    third.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName).loneElement.year shouldBe Some(1968)
  }

  it should "promote at boot a version whose answers were filed while the worker was down" in {
    val world = new World
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    first.drain()
    world.families.unanswered = Set("Lalka")
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))
    first.drain()
    world.families.unanswered = Set.empty                                             // answered while down
    val again = world.service()
    again.takeUp()
    again.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName).loneElement.year shouldBe Some(1968)
    world.pendingStore.read() shouldBe empty
  }

  it should "tell its metrics how many it holds, how long the oldest waited, and how long each promoted one waited, at boot too" in {
    // nothing replays the gate, so these are all that see it stuck (IdentityInputGateStuck)
    val world = handDrained(new World)
    val clock = world.clock.asInstanceOf[_root_.tools.MutableClock]
    val first = world.service()
    first.takeUp()
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
    first.drain()
    world.families.unanswered = Set("Lalka")
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))
    first.drain()
    world.gate.heldReports.last shouldBe (1 -> 0.0)
    clock.advance(java.time.Duration.ofSeconds(90))
    first.drain()
    world.gate.heldReports.last shouldBe (1 -> 90.0)
    clock.advance(java.time.Duration.ofSeconds(30))
    world.families.unanswered = Set.empty
    first.drain()
    world.gate.promotions.last shouldBe (Seq(120.0) -> false)
    world.gate.heldReports.last shouldBe (0 -> 0.0)
    // held again, then answered while the worker is down: promoted at boot
    world.families.unanswered = Set("Lalka")
    world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2026))))
    first.drain()
    clock.advance(java.time.Duration.ofSeconds(300))
    world.families.unanswered = Set.empty
    world.service().takeUp()
    world.gate.promotions.last shouldBe (Seq(300.0) -> true)
    world.gate.heldReports.last shouldBe (0 -> 0.0)
  }

  it should "hide a listing new to the model until it is taken in, by default: shown, it took a film the site showed off it" in {
    IdentityModelService.NewWhilePending.Default shouldBe IdentityModelService.NewWhilePending.Hidden
  }

  it should "hold a listing its venue re-keyed as a changed one, its old version served meanwhile, across a restart too" in {
    // DE Mal seh'n Kino, 2026-11-06: a scrape that dropped the film's page turned its Native key Published; the gate took
    // it for new, and its film went off the site until it was taken in. Its old version is its stand-in now.
    def unpaged(cinema: Cinema, title: String, year: Int) = CinemaMovie(Movie(title, releaseYear = Some(year)), cinema, None, None, None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 10, 1, 18, 0), None)))
    for (restart <- Seq(false, true)) {
      val world   = new World
      val first   = world.service()
      first.takeUp()
      world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
      first.drain()
      val paged = first.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName).loneElement.key
      def film(service: IdentityModelService) = service.peek(10.seconds).get.resolution.decisions
        .filter(_.members.exists(_.venue == Multikino.displayName)).flatMap(_.film).distinct.loneElement
      val took = film(first)
      world.families.unanswered = Set("Lalka")
      world.scrape(first, Multikino, Seq(unpaged(Multikino, "Lalka", 2025)))        // re-keyed: no page now
      first.drain()
      val service = if (restart) { val again = world.service(); again.takeUp(); again } else first
      withClue(s"restart $restart") {
        service.peek(10.seconds).get.listings.filter(_.venue == Multikino.displayName).map(_.key) shouldBe Seq(paged)
        film(service) shouldBe took
        world.families.unanswered = Set.empty
        service.drain()
        // the old key is let go -- kept for its facts alone, as every listing that left while its title is billed is
        // (ListingRetention), never billed
        val now = service.peek(10.seconds).get.listings.filter(l => l.venue == Multikino.displayName && !l.kept).loneElement.key
        now should not be paged
        film(service) shouldBe took
        world.pendingStore.read() shouldBe empty
      }
    }
  }

  it should "take a listing held past its bound in at once, whatever it still waits on, and count it -- at boot too" in {
    // the backstop: a question the queue does not reach never holds a listing longer than MaxHold
    for (atBoot <- Seq(false, true)) {
      val world = handDrained(new World)
      val clock = world.clock.asInstanceOf[_root_.tools.MutableClock]
      val first = world.service()
      first.takeUp()
      world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(2025))))
      first.drain()
      world.families.unanswered = Set("Lalka")                                      // and never answered
      world.scrape(first, Multikino, Seq(movie(Multikino, "Lalka", Some(1968))))
      first.drain()
      clock.advance(ModelInputGate.MaxHold.plusSeconds(1))
      if (atBoot) world.service().takeUp() else first.drain()
      withClue(s"at boot $atBoot") {
        world.gate.bounds.lastOption.map(_.size) shouldBe Some(1)
        world.gate.heldReports.last shouldBe (0 -> 0.0)
        world.pendingStore.read() shouldBe empty
      }
    }
  }
}
