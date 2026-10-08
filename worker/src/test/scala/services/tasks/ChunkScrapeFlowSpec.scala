package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Multikino, Showtime}
import services.events.TaskFinished
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper}
import FakeChunkedScraper.CircuitBlockMs

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * End-to-end of the chunked-scrape mechanism over the real `InMemoryTaskQueue`,
 * store, planner, handlers, coordinator and reaper — no client converted yet, a
 * fake `ChunkedCinemaScraper` stands in. Covers the happy path, per-chunk retry,
 * the supersession/conflict guard, and partial reduce on an abandoned run.
 */
class ChunkScrapeFlowSpec extends AnyFlatSpec with Matchers with org.scalatest.OptionValues {
  import HandlerOutcome._

  private val cinema = Multikino
  private val cinemaName    = cinema.displayName
  private val now    = Instant.parse("2026-06-25T00:00:00Z")
  private val stale  = 15.minutes

  private def film(title: String, day: Int): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, Some(s"https://f/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 6, day, 18, 0), None)), Map.empty, None)

  /** The chunked-scrape stack publishing into `published`. As production's recording
   *  wrapper does, a failed scrape re-raises — unless the venue's fallback served it
   *  instead (`fallbackServes`), which returns normally. */
  private class Harness(scraper: FakeChunkedScraper, clock: Clock = Clock.fixed(now, ZoneOffset.UTC),
                         venueCadenceDefault: FiniteDuration = 14.hours,
                         chunkStore: InMemoryChunkScrapeStore = new InMemoryChunkScrapeStore,
                         fallbackServes: Boolean = false) {
    val venueCadence = new VenueCadenceStore(settings.ScrapeFreshness(venueCadenceDefault))
    val published    = mutable.ListBuffer.empty[Seq[CinemaMovie]]
    val noSchedule   = mutable.ListBuffer.empty[Boolean]
    private val stack = new ChunkScrapeHarness(scraper, s => {
      noSchedule += s.noScheduleListed
      val scraped = scala.util.Try(s.fetch())
      published += scraped.getOrElse(Seq.empty)
      if (!fallbackServes) scraped.get
      ()
    }, clock, chunkStore, venueCadence = Some(venueCadence), staleAfter = stale)
    export stack.{queue, store, freshness, planner, chunkH, reduceH, coord, reaper}
    def drain(at: Instant = now): Int = stack.drain(at)
  }

  "a chunked scrape" should "fan out, gather, and publish the merged listing once every chunk lands" in {
    val h = new Harness(new FakeChunkedScraper(Map(
      "2026-06-25" -> Seq(film("Dune", 25)),
      "2026-06-26" -> Seq(film("Dune", 26), film("Wicked", 26)))))
    h.planner.plan(cinemaName) shouldBe 2
    h.queue.waitingCount(TaskType.ScrapeChunk) shouldBe 2

    h.drain() // chunks → coordinator enqueues reduce → reduce publishes

    h.published should have size 1
    val byTitle = h.published.head.map(m => m.movie.title -> m.showtimes.size).toMap
    byTitle shouldBe Map("Dune" -> 2, "Wicked" -> 1) // Dune merged across both days
    h.freshness.isFresh(ScrapeCinemaHandler.dedupKey(cinema), FreshnessKind.CinemaScrape, now) shouldBe true
    h.store.activeRun(cinemaName) shouldBe None // run cleaned up
  }

  it should "not publish or complete a run whose chunks it could not read — the reduce retries" in {
    val store = new FailingReadChunkScrapeStore
    val h = new Harness(new FakeChunkedScraper(Map("2026-06-25" -> Seq(film("Dune", 25)))), chunkStore = store)
    h.planner.plan(cinemaName) shouldBe 1
    // Run the chunk with the store readable, which enqueues the reduce…
    val chunk = h.queue.claim("w", 30.seconds, now).value
    h.chunkH.handle(chunk) shouldBe Done
    h.queue.complete(chunk.id, "w")
    h.coord.onTaskFinished(TaskFinished(chunk.taskType, chunk.dedupKey, chunk.payload))
    val reduce = h.queue.claim("w", 30.seconds, now).value
    reduce.taskType shouldBe TaskType.ScrapeChunkReduce

    // …then blind the reduce's reads: it must fail, not publish "nothing" and clean up.
    store.failingReads = true
    scala.util.Try(h.reduceH.handle(reduce)).toOption should not contain Done
    h.published shouldBe empty

    store.failingReads = false
    h.store.activeRun(cinemaName) should not be empty // the chunks are still there to retry
    h.reduceH.handle(reduce) shouldBe Done
    h.published.map(_.map(_.movie.title)) shouldBe Seq(Seq("Dune"))
  }

  // The chunked reduce path is `ScrapeChunkReduceHandler`'s own terminal-success
  // branch, a DIFFERENT call site from the plain scrape's — see
  // `ScrapeTasksSpec`'s equivalent for that one. Both must feed the same
  // `VenueScrapeCadence` mechanism (durant/moab/zamora's US/ES venues chunk on
  // Flicks/Webedia), so it's proven here too rather than assumed from the other
  // path's test.
  it should "shorten a thin venue's cadence once its chunks reduce to a listing that runs out early" in {
    // `now` is 2026-06-25T00:00Z = Warsaw 02:00 (CEST); a single showtime at
    // 06:00 the same Warsaw day leaves 4h of runway — well under the 14h
    // default, so periodFor halves it to 2h.
    val thinDay = LocalDateTime.of(2026, 6, 25, 6, 0)
    val thin    = CinemaMovie(Movie("Coyote vs. Acme"), cinema, None, Some("https://f/thin"), None, Nil, Nil,
      Seq(Showtime(thinDay, None)), Map.empty, None)
    val h = new Harness(new FakeChunkedScraper(Map("2026-06-25" -> Seq(thin))))
    h.planner.plan(cinemaName) shouldBe 1
    h.drain()

    h.published should have size 1
    h.venueCadence.periodFor(ScrapeCinemaHandler.dedupKey(cinema)) shouldBe 2.hours
  }

  it should "NOT reduce until every expected chunk has landed" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25)))))
    val runId = { h.planner.plan(cinemaName); h.store.activeRun(cinemaName).get.runId }
    h.chunkH.handle(Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName,runId, "a"), 1)) shouldBe Done
    h.coord.maybeReduce(cinemaName, runId) shouldBe false // only 1 of 2 chunks
    h.queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 0
  }

  it should "retry a failing chunk and still complete" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))), failOnce = Set("b")))
    h.planner.plan(cinemaName) shouldBe 2
    h.drain()                       // chunk b fails once (rescheduled, held back), a stores
    h.published shouldBe empty      // run not complete yet
    h.drain(now.plusSeconds(120))   // b retried → stores → coordinator → reduce
    h.published should have size 1
    h.published.head.map(_.movie.title).toSet shouldBe Set("X", "Y")
  }

  // UK 2026-09-21/22: Odeon advertised business dates whose showtimes call then answered
  // 404 on every retry. Rescheduling a definitive "not found" only replays it — the run
  // sat waiting on those chunks, retrying them to exhaustion (20+ min, paid egress each
  // time), and published only through the backstop's partial reduce.
  it should "land a chunk the upstream says does not exist as an empty slice, so the run completes" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))), gone = Set("b")))
    h.planner.plan(cinemaName) shouldBe 2
    h.drain()
    h.published should have size 1
    h.published.head.map(_.movie.title).toSet shouldBe Set("X")
  }

  it should "DEFER a chunk the host's circuit breaker refused, waiting out the block it named" in {
    // The fetch never reached the wire, so this is not a chunk failure and must not
    // be charged as one: it waits exactly as long as the breaker has left to run.
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25))), circuitOpen = Set("a")))
    val runId = { h.planner.plan(cinemaName); h.store.activeRun(cinemaName).get.runId }
    val payload = ChunkScrapeKeys.chunkPayload(cinemaName, runId, "a")

    h.chunkH.handle(Task("t", TaskType.ScrapeChunk, "d", payload, 1)) match {
      case Deferred(err, notBefore) =>
        err.getOrElse("") should include ("circuit open for fake.pl")
        notBefore shouldBe Some(now.plusMillis(CircuitBlockMs))  // the breaker's own half-open instant
      case other => fail(s"expected a Deferred, got $other")
    }
  }

  it should "not burn a chunk's retry budget while its host is circuit-broken" in {
    // The regression the whole change exists for: on 2026-07-28 fast-fails walked
    // the UK Odeon chunks to attempts 5-6 inside nine minutes — pushing them up a
    // doubling backoff curve toward the 30-minute cap — without one wire call.
    // Needs a MOVING clock: the breaker's block is always measured from "now", so a
    // frozen one would re-defer to an instant already reached and spin the drain.
    val clock = new tools.MutableClock(now)
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25))), circuitOpen = Set("a")), clock)
    h.planner.plan(cinemaName) shouldBe 1

    // Five blocks' worth of passes: each claims the chunk, is refused before the
    // wire, and hands the attempt straight back.
    (1 to 5).foreach { _ =>
      h.drain(clock.instant())
      clock.advance(java.time.Duration.ofMillis(CircuitBlockMs + 1000))
    }

    h.published shouldBe empty
    h.queue.claim("probe", 30.seconds, clock.instant()).map(_.attempts) shouldBe Some(1)
  }

  it should "refuse a second concurrent run for the same cinema (the conflict guard)" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName) shouldBe 1
    val runId = h.store.activeRun(cinemaName).get.runId
    h.planner.plan(cinemaName) shouldBe 0 // a run is already active → no second run
    h.store.activeRun(cinemaName).get.runId shouldBe runId
    h.queue.waitingCount(TaskType.ScrapeChunk) shouldBe 1 // no extra chunk tasks
  }

  it should "drop a stale run's chunk once a superseding run is active" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName)
    val stale1 = h.store.activeRun(cinemaName).get.runId
    // Supersede: a fresh plan after the run goes stale starts a new run.
    val later  = now.plusSeconds(16 * 60)
    val h2runId = h.store.startRun(cinemaName, Seq("a"), later, stale).get
    h2runId should not be stale1
    // The leftover stale-run chunk task is dropped, not stored.
    h.chunkH.handle(Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName,stale1, "a"), 1)) shouldBe Skipped
    h.store.storedKeys(cinemaName, stale1) shouldBe empty
  }

  it should "partial-reduce an abandoned run via the backstop reaper" in {
    // Chunk 'b' is permanently dead, so the run never completes on its own.
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))), failAlways = Set("b")))
    h.planner.plan(cinemaName)
    h.drain()                 // 'a' stores; 'b' fails (rescheduled, held back); not complete
    h.published shouldBe empty
    val past = now.plusSeconds(16 * 60)
    h.reaper(Clock.fixed(past, ZoneOffset.UTC)).sweep() shouldBe 1 // abandoned → enqueue partial reduce
    h.drain(past)
    h.published should have size 1
    h.published.head.map(_.movie.title) shouldBe Seq("X") // partial: only the chunk that landed
  }

  it should "complete a run whose chunks are processed by DIFFERENT worker instances (shared store)" in {
    // Two instances share ONE queue + ONE store (prod = shared Mongo); each has
    // its own coordinator (its own in-process EventBus). No data is duplicated and
    // exactly one reduce/publish happens regardless of which instance did what.
    val published = mutable.ListBuffer.empty[Seq[CinemaMovie]]
    val publish: CinemaScraper => Unit = s => { published += scala.util.Try(s.fetch()).getOrElse(Seq.empty); () }
    val h = new ChunkScrapeHarness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25)))),
      publish, Clock.fixed(now, ZoneOffset.UTC), staleAfter = stale)
    val (queue, store, planner, chunkH, reduceH) = (h.queue, h.store, h.planner, h.chunkH, h.reduceH)
    val coordA  = h.coord                                   // instance A
    val coordB  = new ChunkScrapeCoordinator(store, queue, _root_.tools.SpecClock.Pinned) // instance B

    planner.plan(cinemaName) // one instance plans; the run + chunk tasks are shared

    // Instance A handles one chunk and fires ITS coordinator (not complete yet).
    val ta = queue.claim("A", 30.seconds, now).get
    chunkH.handle(ta) shouldBe Done; queue.complete(ta.id, "A")
    coordA.onTaskFinished(TaskFinished(ta.taskType, ta.dedupKey, ta.payload))
    queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 0

    // Instance B handles the other chunk; its coordinator reads the shared store,
    // sees the run complete, and enqueues the single reduce.
    val tb = queue.claim("B", 30.seconds, now).get
    chunkH.handle(tb) shouldBe Done; queue.complete(tb.id, "B")
    coordB.onTaskFinished(TaskFinished(tb.taskType, tb.dedupKey, tb.payload))
    queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 1

    // Either instance reduces — exactly once.
    val tr = queue.claim("A", 30.seconds, now).get
    reduceH.handle(tr) shouldBe Done; queue.complete(tr.id, "A")
    published should have size 1
    published.head.map(_.movie.title).toSet shouldBe Set("X", "Y")
    store.activeRun(cinemaName) shouldBe None
  }

  // The German outage (prod 2026-07-24 → 07-27). A Filmstarts venue that advertises
  // no showtime days at all — a small/seasonal house with nothing on — plans zero
  // chunks, so it never reaches the reduce that stamps freshness. Un-stamped means
  // `lastFetchedAt = None`, which sorts AHEAD of every timestamp in the ScrapeReaper's
  // oldest-first order, so ~40 such venues held the entire per-tick cap (40) every
  // minute for days and Germany's ~1000 working cinemas were never enqueued once.
  // An empty repertoire is a SUCCESSFUL scrape and must advance the due schedule.
  it should "mark a chunked cinema fresh when its plan is legitimately empty, so it waits its normal window" in {
    val h   = new Harness(new FakeChunkedScraper(Map.empty))
    val key = ScrapeCinemaHandler.dedupKey(cinema)

    h.planner.plan(cinemaName) shouldBe 0
    h.published should have size 1     // still published (and recorded white on /uptime)
    h.published.head shouldBe empty
    h.freshness.isFresh(key, FreshnessKind.CinemaScrape, now) shouldBe true
  }

  // The page saying the venue has nothing on (a drive-in closed for the season) must reach the
  // archive as such, so the content census does not count it as a silent parser break.
  it should "publish an empty plan the source vouched for as no schedule listed, and an unvouched one as not" in {
    val vouched = new Harness(new FakeChunkedScraper(Map.empty, planSaysNoSchedule = true))
    vouched.planner.plan(cinemaName) shouldBe 0
    vouched.noSchedule.toList shouldBe List(true)

    val unvouched = new Harness(new FakeChunkedScraper(Map.empty))
    unvouched.planner.plan(cinemaName) shouldBe 0
    unvouched.noSchedule.toList shouldBe List(false)
  }

  // A plan that THROWS keeps the fast-retry behaviour a transient 5xx needs — but
  // only for the retry budget, after which it is parked on the normal window. This
  // is the UK half of the same outage: ~40 venues 404ing on every tick forever.
  it should "retry a failed plan at tick cadence for the budget, then park it on the normal window" in {
    val h   = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25))), planThrows = true))
    val key = ScrapeCinemaHandler.dedupKey(cinema)

    h.planner.plan(cinemaName) shouldBe 0
    h.freshness.lastFetchedAt(key) shouldBe None   // still due next tick — maybe transient
    h.planner.plan(cinemaName) shouldBe 0
    h.freshness.lastFetchedAt(key) shouldBe None
    h.planner.plan(cinemaName) shouldBe 0          // past the 2-retry budget
    h.freshness.isFresh(key, FreshnessKind.CinemaScrape, now) shouldBe true
  }

  // The fallback serving a venue whose plan failed IS the scrape succeeding, as on the
  // plain path: kept due, the venue was re-run twice a minute apart, and each re-run
  // re-walked the fallback's whole horizon.
  it should "mark a chunked cinema fresh when its fallback served the plan that failed" in {
    val h   = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25))), planThrows = true), fallbackServes = true)
    val key = ScrapeCinemaHandler.dedupKey(cinema)

    h.planner.plan(cinemaName) shouldBe 0
    h.freshness.isFresh(key, FreshnessKind.CinemaScrape, now) shouldBe true
  }

  it should "re-process a chunk whose worker instance crashed mid-run (lease expiry)" in {
    val h = new Harness(new FakeChunkedScraper(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName)
    // Instance A claims the chunk on a 1s lease, then "crashes" (never completes).
    val ta = h.queue.claim("A", 1.second, now).get
    ta.taskType shouldBe TaskType.ScrapeChunk
    // Lease expires → the task returns to waiting and another instance drains it.
    h.queue.reapExpiredLeases(now.plusSeconds(2)) should be >= 1
    h.drain(now.plusSeconds(2))
    h.published should have size 1
    h.published.head.map(_.movie.title) shouldBe Seq("X")
  }

  it should "stagger a big fan-out's chunks across the spread window instead of all-claimable-at-once" in {
    // Six chunks, a 6-minute spread → chunk k eligible at +1min·k. All six are
    // enqueued up front, but only the first slice is claimable at T0; the rest are
    // held back by `nextEligibleAt`, so a free worker falls through to other work
    // (e.g. rating refreshes) between them instead of the queue handing out all six.
    val queue   = new InMemoryTaskQueue
    val store   = new InMemoryChunkScrapeStore
    val slices  = (0 until 6).map(i => f"2026-06-${25 + i}%02d" -> Seq(film("F", 25 + i))).toMap
    val map     = Map(cinemaName -> (new FakeChunkedScraper(slices): ChunkedCinemaScraper))
    val planner = new ChunkScrapePlanner(map, store, queue, _ => (),
      new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = _root_.tools.SpecClock.Pinned), services.tasks.ChunkScrapePlanner.RunTimeout(30.minutes),
      Clock.fixed(now, ZoneOffset.UTC), chunkSpread = settings.ScrapeChunkSpread(6.minutes))

    planner.plan(cinemaName) shouldBe 6
    queue.waitingCount(TaskType.ScrapeChunk) shouldBe 6 // all six enqueued...

    // ...but claimable in staggered slices, not one burst. Claiming (without
    // completing) leaves each claimed task worked_on, so re-claims can't double-count.
    claimAllWaiting(queue, now) shouldBe 1                       // only the +0 slice
    claimAllWaiting(queue, now.plusSeconds(6 * 60)) shouldBe 5   // the rest, once their window opens
  }

  // What the cost-spaced scrape schedule spaces venues by: the planner task, one per
  // chunk, and the reduce.
  it should "record the run's fan-out as the venue's scrape cost" in {
    val queue   = new InMemoryTaskQueue
    val costs   = new InMemoryScrapeCostStore
    val slices  = (0 until 6).map(i => f"2026-06-${25 + i}%02d" -> Seq(film("F", 25 + i))).toMap
    val scraper = new FakeChunkedScraper(slices)
    val planner = new ChunkScrapePlanner(Map(cinemaName -> (scraper: ChunkedCinemaScraper)), new InMemoryChunkScrapeStore, queue, _ => (),
      new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = _root_.tools.SpecClock.Pinned), services.tasks.ChunkScrapePlanner.RunTimeout(30.minutes),
      Clock.fixed(now, ZoneOffset.UTC), costs = costs)

    planner.plan(cinemaName) shouldBe 6
    costs.recent() shouldBe Map(ScrapeCinemaHandler.dedupKey(scraper.cinema) -> Seq(ScrapeCost(8)))
  }

  // The queue claims the eligible task with the oldest `submittedAt`. When every chunk of
  // a run carried the planning instant, an OLDER run's stragglers — still ripening for the
  // whole spread — beat a NEWER run's chunks that had been claimable for minutes: prod
  // 2026-09-21..28 showed the head-of-line age pinned just under the 300s spread in every
  // country while chunks were being claimed throughout. A chunk queues from when it
  // becomes claimable, so claim order is eligibility order.
  it should "claim an earlier-eligible chunk of a newer run before an older run's later-eligible one" in {
    val queue   = new InMemoryTaskQueue
    val store   = new InMemoryChunkScrapeStore
    def slices(day: Int) = (0 until 2).map(i => f"2026-06-${day + i}%02d" -> Seq(film("F", day + i))).toMap
    val older   = new FakeChunkedScraper(slices(10))
    val newer   = new FakeChunkedScraper(slices(20), cinema = KinoApollo)
    def planAt(at: Instant, scraper: FakeChunkedScraper) =
      new ChunkScrapePlanner(Map(scraper.cinema.displayName -> (scraper: ChunkedCinemaScraper)), store, queue, _ => (),
        new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = _root_.tools.SpecClock.Pinned), services.tasks.ChunkScrapePlanner.RunTimeout(30.minutes),
        Clock.fixed(at, ZoneOffset.UTC), chunkSpread = settings.ScrapeChunkSpread(5.minutes)).plan(scraper.cinema.displayName)

    planAt(now, older) shouldBe 2                   // chunks eligible at +0s and +150s
    planAt(now.plusSeconds(60), newer) shouldBe 2   // chunks eligible at +60s and +210s
    queue.claim("w", 30.seconds, now).map(_.payload) should not be empty // the older run's first chunk

    // At +160s the older run's second chunk has been claimable 10s, the newer run's first 100s.
    queue.claim("w", 30.seconds, now.plusSeconds(160)).map(_.payload("cinema")) shouldBe Some(KinoApollo.displayName)
  }

  /** Claim every currently-eligible waiting task at `at` (leaving them worked_on,
   *  not completed), returning how many were claimable. */
  private def claimAllWaiting(queue: InMemoryTaskQueue, at: Instant): Int = {
    var n = 0
    while (queue.claim("probe", 30.seconds, at).isDefined) n += 1
    n
  }

  it should "record the scrape's failure when chunk-plan enumeration throws" in {
    val h = new Harness(new FakeChunkedScraper(Map.empty, planThrows = true))
    h.planner.plan(cinemaName) shouldBe 0
    h.store.activeRun(cinemaName) shouldBe None  // no run started
    h.published should have size 1        // the failure was published through the recorder path
  }

  // A chunk whose first read lost a page fails to store it (a store that throws once) and the task
  // retries. The retry reads every page: nothing of the failed attempt may outlive it, or the run
  // reduces INCOMPLETE and skips the prune it owes.
  it should "publish a run complete once a retry reads every page the failed attempt lost" in {
    val blipOnce = failingWrites("a", 1)
    val completeness = mutable.ListBuffer.empty[Boolean]
    val stack = new ChunkScrapeHarness(new FakeChunkedScraper(Map("a" -> Seq(film("Dune", 25))), pageFailsOnceIn = Set("a")),
      s => { completeness += s.listingIsComplete; () }, Clock.fixed(now, ZoneOffset.UTC), blipOnce, staleAfter = stale)
    stack.planner.plan(cinemaName) shouldBe 1
    val runId = stack.store.activeRun(cinemaName).value.runId
    val chunk = Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName, runId, "a"), 1)
    stack.chunkH.handle(chunk) shouldBe a[Reschedule]
    stack.chunkH.handle(chunk.copy(attempts = 2)) shouldBe Done
    stack.reduceH.handle(Task("r", TaskType.ScrapeChunkReduce, "r", ChunkScrapeKeys.reducePayload(cinemaName, runId), 1)) shouldBe Done
    completeness shouldBe Seq(true)
  }

  /** A store whose writes of `key` throw `times` times, then land. */
  private def failingWrites(key: String, times: Int) = new InMemoryChunkScrapeStore {
    private var left = times
    override def storeChunk(cinema: String, runId: String, k: String, chunk: StoredChunk, now: Instant): Unit =
      if (k == key && left > 0) { left -= 1; throw new RuntimeException("mongo blip") }
      else super.storeChunk(cinema, runId, k, chunk, now)
  }

  // Without its marker the run would reduce as the whole listing and prune what the plan's
  // failed day probe missed: the run is abandoned instead, so the venue's next scrape plans afresh.
  it should "abandon a run whose plan marker failed to store" in {
    val stack = new ChunkScrapeHarness(new FakeChunkedScraper(Map("a" -> Seq(film("Dune", 25))), planPageFails = true),
      _ => (), Clock.fixed(now, ZoneOffset.UTC), failingWrites(ChunkScrapeKeys.PlanIncomplete, 1), staleAfter = stale)
    a[RuntimeException] should be thrownBy stack.planner.plan(cinemaName)
    stack.store.activeRun(cinemaName) shouldBe None
    stack.queue.claim("w", 30.seconds, now) shouldBe None
  }

  // A chunk gone upstream lands empty — and when that store fails, the chunk retries like any
  // other failure rather than escaping the handler.
  it should "reschedule a gone chunk whose empty slice failed to store" in {
    val stack = new ChunkScrapeHarness(new FakeChunkedScraper(Map("a" -> Nil), gone = Set("a")),
      _ => (), Clock.fixed(now, ZoneOffset.UTC), failingWrites("a", 1), staleAfter = stale)
    stack.planner.plan(cinemaName) shouldBe 1
    val runId = stack.store.activeRun(cinemaName).value.runId
    val chunk = Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName, runId, "a"), 1)
    stack.chunkH.handle(chunk) shouldBe a[Reschedule]
    stack.chunkH.handle(chunk.copy(attempts = 2)) shouldBe Done
    stack.store.storedKeys(cinemaName, runId) shouldBe Set("a")
  }

  // Attempt 1 stalls mid-read past its lease; the queue hands the chunk to attempt 2, which reads every
  // page and stores the whole slice. Attempt 1 then lands its PARTIAL slice: it must not replace attempt 2's,
  // or the run reduces as complete over a slice that lacks a film — and prunes it.
  it should "keep a retry's whole slice when the stalled attempt it replaced lands late" in {
    val published = mutable.ListBuffer.empty[(Seq[String], Boolean)]
    val whole = new ChunkScrapeHarness(new FakeChunkedScraper(Map("a" -> Seq(film("Dune", 25), film("Odyseja", 25)))),
      s => { published += ((s.fetch().map(_.movie.title).sorted, s.listingIsComplete)); () }, Clock.fixed(now, ZoneOffset.UTC), staleAfter = stale)
    val stalled = whole.rescraping(new FakeChunkedScraper(Map("a" -> Seq(film("Dune", 25))), pageFailsIn = Set("a")))
    whole.planner.plan(cinemaName) shouldBe 1
    val runId = whole.store.activeRun(cinemaName).value.runId
    val chunk = Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName, runId, "a"), 1)
    whole.chunkH.handle(chunk.copy(attempts = 2)) shouldBe Done
    stalled.chunkH.handle(chunk) shouldBe Done
    whole.reduceH.handle(Task("r", TaskType.ScrapeChunkReduce, "r", ChunkScrapeKeys.reducePayload(cinemaName, runId), 1)) shouldBe Done
    published shouldBe Seq((Seq("Dune", "Odyseja"), true))
  }
}
