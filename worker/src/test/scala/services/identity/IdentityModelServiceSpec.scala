package services.identity

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
        case CandidateQuery.Title(text) => Answer.Known(titles.collect { case (word, hits) if normalizer.searchQuery(text).toLowerCase.contains(word) => hits }.flatten.toSeq)
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
    Listing.distinct(Listing.all(scrapes.toSeq, normalizer))

  private def decided(decisions: Seq[ResolverDecision]) = decisions.map(d => (d.listings, d.film)).toSet

  private final class World {
    val reads   = new ObservationReads
    val lookups = new Keyed(reads)
    val store   = new InMemoryIdentityModelStore
    var scrapes = Map.empty[Cinema, Seq[CinemaMovie]]
    def service(): IdentityModelService = new IdentityModelService(
      () => new IncrementalResolver(new TrackedLookups(lookups, reads), normalizer, calibration, store = store),
      reads, () => listingsOf(scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(), clock = _root_.tools.SpecClock.Pinned)
    def scrape(service: IdentityModelService, cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
      scrapes += cinema -> films; service.venueScraped(cinema, films)
    }
    def expected: Set[(Set[ListingKey], Option[Int])] =
      decided(IdentityResolver.resolve(listingsOf(scrapes), lookups, normalizer, calibration).decisions)
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
      val limit: FiniteDuration = 1.hour
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

  // ── a new listing waits for its venue page (a cut-over country) ─────────────────────────────

  /** Which listings' pages are unread, and the pages asked for: what a cut-over country's VenuePageWait
   *  answers from venue_pages and the task queue. */
  private final class Pages extends PageWait {
    var unread    = Set.empty[ListingKey]
    val requested = scala.collection.mutable.ArrayBuffer.empty[ListingKey]
    def awaiting(listing: Listing): Boolean = unread(listing.key)
    def request(listing: Listing): Unit = { requested += listing.key; () }
    val limit: FiniteDuration = 1.hour
  }
  private def ticking = new tools.MutableClock(java.time.Instant.parse("2026-10-01T10:00:00Z"))
  private def waiting(world: World, pages: Pages, clock: java.time.Clock) = new IdentityModelService(
    () => new IncrementalResolver(new TrackedLookups(world.lookups, world.reads), normalizer, calibration, store = world.store),
    world.reads, () => listingsOf(world.scrapes), normalizer, 1.second, Executors.newSingleThreadScheduledExecutor(),
    pageWait = pages, clock = clock)
  private def held(service: IdentityModelService) = service.peek(10.seconds).get.resolution.decisions.flatMap(_.listings).toSet

  "a new listing whose venue page is unread" should "wait for the page, asked for once, and be taken in once it is read" in {
    val (world, pages, clock) = (new World, new Pages, ticking)
    val service = waiting(world, pages, clock)
    service.takeUp()
    val lalka = movie(Multikino, "Lalka", Some(2025))
    pages.unread = listingsOf(Map(Multikino -> Seq(lalka))).map(_.key).toSet
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    held(service) shouldBe empty
    pages.requested.toSeq shouldBe pages.unread.toSeq
    service.drain()
    pages.requested.size shouldBe 1                                      // asked once, not every drain

    pages.unread = Set.empty                                             // the page was read
    service.drain()
    held(service) shouldBe pages.requested.toSet
  }

  it should "be taken in after the limit even if its page is never read" in {
    val (world, pages, clock) = (new World, new Pages, ticking)
    val service = waiting(world, pages, clock)
    service.takeUp()
    val lalka = movie(Multikino, "Lalka", Some(2025))
    pages.unread = listingsOf(Map(Multikino -> Seq(lalka))).map(_.key).toSet
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    held(service) shouldBe empty
    clock.advanceSeconds(61 * 60)
    service.drain()
    held(service) shouldBe pages.unread
  }

  it should "never hold back a listing the model already holds, and be forgotten when its venue stops listing it" in {
    val (world, pages, clock) = (new World, new Pages, ticking)
    val service = waiting(world, pages, clock)
    service.takeUp()
    val lalka   = movie(Multikino, "Lalka", Some(2025))
    val matilda = movie(Multikino, "Matilda")
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    val lalkaKey = listingsOf(Map(Multikino -> Seq(lalka))).map(_.key).toSet
    held(service) shouldBe lalkaKey
    // Its page is re-asked later (a refresh): the model keeps it, and holds back only the newcomer.
    pages.unread = listingsOf(Map(Multikino -> Seq(lalka, matilda))).map(_.key).toSet
    world.scrape(service, Multikino, Seq(lalka, matilda))
    service.drain()
    held(service) shouldBe lalkaKey
    // Matilda leaves before its page is read: nothing of it is ever taken in.
    world.scrape(service, Multikino, Seq(lalka))
    service.drain()
    pages.unread = Set.empty
    service.drain()
    held(service) shouldBe lalkaKey
  }
}
