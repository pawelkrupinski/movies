package services.tasks

import models.{Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaScrapeRunner, CinemaScraper, FakeDetailEnricher, FilmwebShowtimesClient}
import services.events.InProcessEventBus
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.duration._
import scala.util.hashing.MurmurHash3

class ScrapeTasksSpec extends AnyFlatSpec with Matchers {

  private class FakeScraper(val cinema: Cinema, result: => Seq[CinemaMovie]) extends CinemaScraper {
    var fetchCount = 0
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = { fetchCount += 1; result }
  }

  private def movieAt(c: Cinema, title: String = "Dune") = Seq(
    CinemaMovie(Movie(title), c, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.now(), Some("https://book"))))
  )

  private def freshRunner() = new CinemaScrapeRunner(
    new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus()),
    new InProcessEventBus(),
    deferredCinemas = Set.empty
  )

  private def task(c: Cinema) =
    Task("id", TaskType.ScrapeCinema, ScrapeCinemaHandler.dedupKey(c),
      Map(ScrapeCinemaHandler.CinemaKey -> c.displayName), attempts = 1)

  // ── ScrapeCinemaHandler ───────────────────────────────────────────────────

  "ScrapeCinemaHandler" should "skip (no scrape) when the cinema is already fresh" in {
    val scraper = new FakeScraper(Multikino, movieAt(Multikino))
    val fresh   = new InMemoryFreshnessStore
    fresh.markFresh(ScrapeCinemaHandler.dedupKey(Multikino), FreshnessKind.CinemaScrape)
    val h = new ScrapeCinemaHandler(Map(ScrapeCinemaHandler.scraperKey(Multikino) -> scraper), freshRunner(), fresh)
    h.handle(task(Multikino)) shouldBe HandlerOutcome.Skipped
    scraper.fetchCount shouldBe 0
  }

  it should "scrape and mark fresh when stale" in {
    val scraper = new FakeScraper(Multikino, movieAt(Multikino))
    val fresh   = new InMemoryFreshnessStore
    val key     = ScrapeCinemaHandler.dedupKey(Multikino)
    val h = new ScrapeCinemaHandler(Map(ScrapeCinemaHandler.scraperKey(Multikino) -> scraper), freshRunner(), fresh)
    h.handle(task(Multikino)) shouldBe HandlerOutcome.Done
    scraper.fetchCount shouldBe 1
    fresh.isFresh(key, FreshnessKind.CinemaScrape) shouldBe true
  }

  it should "drop a task whose cinema is no longer in the catalogue" in {
    val h = new ScrapeCinemaHandler(Map.empty, freshRunner(), new InMemoryFreshnessStore)
    h.handle(task(Multikino)) shouldBe HandlerOutcome.Done
  }

  it should "swallow a scrape failure as Done and leave the cinema stale so the reaper retries" in {
    val scraper = new FakeScraper(Multikino, throw new RuntimeException("HTTP 503 for GET https://x"))
    val fresh   = new InMemoryFreshnessStore
    val key     = ScrapeCinemaHandler.dedupKey(Multikino)
    val h = new ScrapeCinemaHandler(Map(ScrapeCinemaHandler.scraperKey(Multikino) -> scraper), freshRunner(), fresh)
    h.handle(task(Multikino)) shouldBe HandlerOutcome.Done
    fresh.isFresh(key, FreshnessKind.CinemaScrape) shouldBe false
  }

  it should "scrape (not skip) a cinema the reaper deems due even when its last scrape was inside the rolling TTL" in {
    // Regression: the reaper enqueues on a phase-window boundary; the handler must
    // re-gate on the SAME DueWindow, not a rolling freshness TTL. A cinema scraped
    // just before its boundary is due again just after it, yet still within the
    // window's TTL — the old isFresh re-gate would skip a task the reaper keeps
    // enqueuing, churning the queue without ever scraping. Both share DueWindow now.
    val period    = 15.minutes
    val due        = new DueWindow(period)
    val key        = ScrapeCinemaHandler.dedupKey(Multikino)
    val phase      = Math.floorMod(MurmurHash3.stringHash(key).toLong, period.toMillis)
    val w          = 100L
    val stampedAt  = Instant.ofEpochMilli(phase + w * period.toMillis + period.toMillis - 60000) // 1 min before boundary
    val now        = Instant.ofEpochMilli(phase + (w + 1) * period.toMillis + 60000)             // 1 min after  boundary
    val scraper    = new FakeScraper(Multikino, movieAt(Multikino))
    val fresh      = new InMemoryFreshnessStore
    fresh.markFresh(key, FreshnessKind.CinemaScrape, stampedAt)
    val h = new ScrapeCinemaHandler(Map(ScrapeCinemaHandler.scraperKey(Multikino) -> scraper),
      freshRunner(), fresh, due, Clock.fixed(now, ZoneOffset.UTC))
    h.handle(task(Multikino)) shouldBe HandlerOutcome.Done
    scraper.fetchCount shouldBe 1
  }

  // ── ScrapeReaper ──────────────────────────────────────────────────────────

  "ScrapeReaper" should "enqueue a stale cinema and not double-enqueue while the task is still active" in {
    val scraper = new FakeScraper(Multikino, movieAt(Multikino))
    val queue   = new InMemoryTaskQueue
    val reaper  = new ScrapeReaper(Seq(scraper), queue, new InMemoryFreshnessStore)
    reaper.tick() shouldBe 1
    reaper.tick() shouldBe 0 // dedup — task still waiting
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "not enqueue a cinema that is still fresh" in {
    val scraper = new FakeScraper(KinoApollo, movieAt(KinoApollo))
    val fresh   = new InMemoryFreshnessStore
    fresh.markFresh(ScrapeCinemaHandler.dedupKey(KinoApollo), FreshnessKind.CinemaScrape)
    val reaper  = new ScrapeReaper(Seq(scraper), new InMemoryTaskQueue, fresh)
    reaper.tick() shouldBe 0
  }

  it should "enqueue each of several stale cinemas once" in {
    val scrapers = Seq(new FakeScraper(Multikino, movieAt(Multikino)), new FakeScraper(KinoApollo, movieAt(KinoApollo)))
    val queue    = new InMemoryTaskQueue
    val reaper   = new ScrapeReaper(scrapers, queue, new InMemoryFreshnessStore)
    reaper.tick() shouldBe 2
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 2L
  }

  // Boot-storm guard: a cold worker sees every cinema stale at once. Enqueuing all
  // of them lets the TaskWorker pool drain flat-out for minutes and exhaust the
  // shared-CPU credit balance. The reaper caps the per-tick batch so it drains
  // inside the tick interval (idle gaps let credit recover); the backlog clears
  // over later ticks. The queue dedups, so in-flight cinemas don't re-count.
  it should "enqueue at most maxEnqueuePerTick stale cinemas per tick and drain the rest on later ticks" in {
    val scrapers = Seq(Multikino, KinoApollo, KinoMuza, Rialto, Helios)
      .map(c => new FakeScraper(c, movieAt(c)))
    val queue  = new InMemoryTaskQueue
    val reaper = new ScrapeReaper(scrapers, queue, new InMemoryFreshnessStore, maxEnqueuePerTick = 2)
    reaper.tick() shouldBe 2            // first batch capped at 2
    reaper.tick() shouldBe 2            // first 2 still in-flight (deduped) → next 2 enqueue
    reaper.tick() shouldBe 1            // last stale cinema
    reaper.tick() shouldBe 0            // nothing left to enqueue
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 5L
  }

  // Fairness under the cap: when more cinemas are due than the per-tick cap allows,
  // the reaper must enqueue the MOST-OVERDUE first, not the head of its fixed
  // city→catalogue list. Otherwise a credit-throttled worker that drains slowly
  // keeps re-enqueuing the same front cinemas every tick and STARVES the tail (the
  // last cities) — they'd never get scraped while the backlog persists. Here the
  // tail-of-list cinemas carry the OLDEST stamps; under a cap of 2 they (not the
  // two head-of-list cinemas) must win the tick.
  it should "enqueue the most-overdue cinemas first when the per-tick cap bites, not the head of the list" in {
    val now = Instant.parse("2026-06-20T08:00:00Z")
    // List order: Multikino, KinoApollo, KinoMuza, Rialto, Helios.
    // Stamp them so the LAST two in the list are the most overdue (oldest stamps),
    // and all are past their DueWindow boundary so every one is due.
    val ordered = Seq(Multikino, KinoApollo, KinoMuza, Rialto, Helios)
    val ageMinutes = Map(    // larger age = more overdue
      Multikino  -> 31L, KinoApollo -> 32L, KinoMuza -> 33L, Rialto -> 90L, Helios -> 120L)
    val fresh = new InMemoryFreshnessStore
    ordered.foreach(c =>
      fresh.markFresh(ScrapeCinemaHandler.dedupKey(c), FreshnessKind.CinemaScrape,
        now.minusSeconds(ageMinutes(c) * 60)))
    val scrapers = ordered.map(c => new FakeScraper(c, movieAt(c)))
    val queue    = new InMemoryTaskQueue
    val reaper   = new ScrapeReaper(scrapers, queue, fresh, maxEnqueuePerTick = 2,
      clock = Clock.fixed(now, ZoneOffset.UTC))

    reaper.tick() shouldBe 2
    val enqueuedKeys = queue.monitor(100).active.map(_.dedupKey).toSet
    // The two OLDEST (Helios=120m, Rialto=90m) — at the TAIL of the list — must win,
    // NOT the head-of-list Multikino/KinoApollo the old fixed-order takeWhile chose.
    enqueuedKeys shouldBe Set(ScrapeCinemaHandler.dedupKey(Helios), ScrapeCinemaHandler.dedupKey(Rialto))
  }

  // Credit-throttle backoff: while the ScrapeThrottleMonitor reports the pool is
  // running throttled-slow, the reaper enqueues only a TRICKLE (throttledMaxEnqueue
  // PerTick) instead of the healthy cap — so the backlog drains and the pool earns
  // idle to rebuild credit (breaking the metastable deadlock), then resumes.
  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    val scrapers = Seq(Multikino, KinoApollo, KinoMuza, Rialto, Helios).map(c => new FakeScraper(c, movieAt(c)))
    val queue    = new InMemoryTaskQueue
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def slowScrapeMillis = 30000L }
    val reaper = new ScrapeReaper(scrapers, queue, new InMemoryFreshnessStore,
      maxEnqueuePerTick = Int.MaxValue, throttledMaxEnqueuePerTick = 2, throttle = throttled)
    // All 5 cinemas are due, but throttled → only 2 enqueue (vs the full 5 healthy).
    reaper.tick() shouldBe 2
  }

  it should "resume the full enqueue cap once the throttle clears" in {
    val scrapers = Seq(Multikino, KinoApollo, KinoMuza, Rialto, Helios).map(c => new FakeScraper(c, movieAt(c)))
    val queue    = new InMemoryTaskQueue
    var throttledFlag = true
    val signal = new ScrapeThrottleSignal { def isThrottled = throttledFlag; def slowScrapeMillis = 0L }
    val reaper = new ScrapeReaper(scrapers, queue, new InMemoryFreshnessStore,
      maxEnqueuePerTick = Int.MaxValue, throttledMaxEnqueuePerTick = 2, throttle = signal)
    reaper.tick() shouldBe 2 // throttled trickle
    throttledFlag = false
    reaper.tick() shouldBe 3 // recovered → the remaining 3 (first 2 still waiting, deduped)
  }

  it should "not enqueue when another machine has claimed this minute's occurrence" in {
    val scraper = new FakeScraper(Multikino, movieAt(Multikino))
    val queue   = new InMemoryTaskQueue
    val reaper  = new ScrapeReaper(Seq(scraper), queue, new InMemoryFreshnessStore,
      runStore = NeverClaimScheduledRunStore)
    reaper.tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "enqueue stale cinemas when it wins this minute's occurrence claim" in {
    val scraper = new FakeScraper(Multikino, movieAt(Multikino))
    val queue   = new InMemoryTaskQueue
    val reaper  = new ScrapeReaper(Seq(scraper), queue, new InMemoryFreshnessStore,
      runStore = new InMemoryScheduledRunStore)
    reaper.tickIfClaimed() shouldBe 1
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  // Boot-cost guard: on a cold worker every cinema is stale, so the first tick
  // enqueues all of them at once for the TaskWorker to drain — the queue-mode
  // boot scrape burst. `start()` must hold that first tick back by `initialDelay`
  // so it doesn't pile onto the cold-JVM cache hydrate. (Before this change the
  // initial delay was a hardcoded `0L`, so the tick fired immediately and the
  // `shouldBe 0L` assertion below would fail.)
  it should "hold the first tick until the initial delay elapses" in {
    val queue  = new InMemoryTaskQueue
    val reaper = new ScrapeReaper(Seq(new FakeScraper(Multikino, movieAt(Multikino))),
                                  queue, new InMemoryFreshnessStore, initialDelay = 300.millis)
    reaper.start()
    Thread.sleep(100)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L // still within the delay
    Thread.sleep(600)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L // delay elapsed → first tick ran
    reaper.stop()
  }

  // Boot-storm guard: at a cold boot the freshness mirror hydrates asynchronously,
  // so before it lands every cinema reads as stale. If the reaper ticked then, it
  // would enqueue ALL of them — the 300-wide storm that drained the CPU credit
  // balance. `start()` must hold the first tick until `whenReady(CinemaScrape)`
  // signals the scrape stamps are loaded; by then they're fresh, so nothing
  // enqueues. (Before the gate, the first tick fired at initialDelay=0 against the
  // empty mirror and the first `shouldBe 0L` below would see both cinemas queued.)
  it should "wait for the freshness mirror to hydrate before its first tick, so a slow boot doesn't re-scrape every cinema" in {
    import scala.concurrent.{Future, Promise}
    val gate  = Promise[Unit]()
    val fresh = new InMemoryFreshnessStore {
      override def whenReady(kind: FreshnessKind): Future[Unit] =
        if (kind == FreshnessKind.CinemaScrape) gate.future else super.whenReady(kind)
    }
    val scrapers = Seq(new FakeScraper(Multikino, movieAt(Multikino)),
                       new FakeScraper(KinoApollo, movieAt(KinoApollo)))
    val queue  = new InMemoryTaskQueue
    val reaper = new ScrapeReaper(scrapers, queue, fresh, initialDelay = 0.seconds, readyTimeout = 5.seconds)
    reaper.start()
    Thread.sleep(150)
    // Gate still closed → reaper is blocked, not yet ticking.
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
    // Boot hydrate lands: stamps populate the mirror (fresh), then readiness fires.
    scrapers.foreach(s => fresh.markFresh(ScrapeCinemaHandler.dedupKey(s.cinema), FreshnessKind.CinemaScrape))
    gate.success(())
    Thread.sleep(150)
    // First tick ran post-hydrate and found every cinema fresh → no storm.
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
    reaper.stop()
  }

  // Boot-storm guard, the stacked-restart case: if the freshness mirror never
  // hydrates within a single ready timeout (a slow/throttled Mongo — exactly what
  // an in-progress storm causes), the reaper must keep HOLDING its ticks, not fail
  // open and re-scrape every cinema. Before this fix it waited only `readyTimeout`
  // then ticked against the empty mirror, enqueuing all ~300 — which threw more
  // load at the already-slow Mongo so the next restart hydrated slower still and
  // stormed again. Now it keeps waiting for as long as the mirror is unready (and
  // readiness itself is withheld until a hydrate succeeds), so a cold mirror yields
  // zero enqueues no matter how many ready timeouts elapse; once readiness fires,
  // normal ticking resumes.
  it should "keep holding ticks instead of failing open and storming while the freshness mirror stays unhydrated" in {
    import scala.concurrent.{Future, Promise}
    val gate  = Promise[Unit]()
    val fresh = new InMemoryFreshnessStore {
      override def whenReady(kind: FreshnessKind): Future[Unit] =
        if (kind == FreshnessKind.CinemaScrape) gate.future else super.whenReady(kind)
    }
    val scrapers = Seq(new FakeScraper(Multikino, movieAt(Multikino)),
                       new FakeScraper(KinoApollo, movieAt(KinoApollo)))
    val queue  = new InMemoryTaskQueue
    val reaper = new ScrapeReaper(scrapers, queue, fresh,
      initialDelay = 0.seconds, readyTimeout = 50.millis, interval = 50.millis)
    reaper.start()
    Thread.sleep(400) // several readyTimeout + interval periods pass with the mirror still cold
    // Before the fix: fail-open after the first 50ms → both stale cinemas enqueued.
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
    gate.success(()) // mirror reports ready → the legitimate cold-start scrape may now proceed
    Thread.sleep(200)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) should be > 0L
    reaper.stop()
  }

  // ── WorkerHeartbeat: queue-depth diagnostic ─────────────────────────────────

  "WorkerHeartbeat.statusLine" should "report the queue backlog depth" in {
    val queue = new InMemoryTaskQueue
    queue.enqueue(TaskType.ScrapeCinema, "a")
    queue.enqueue(TaskType.ScrapeCinema, "b")
    val line = new WorkerHeartbeat(queue).statusLine()
    line should include ("waiting=2")
    line should include ("backlog=2")
  }

  // ── Detail enqueue is event-driven, not done by the runner ──────────────────

  private def movieWithRef(c: Cinema, title: String = "Dune") = Seq(
    CinemaMovie(Movie(title), c, posterUrl = None, filmUrl = Some(s"http://detail/$title"),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.now(), Some("https://book"))))
  )

  "CinemaScrapeRunner" should "enqueue detail via the bus (CinemaMovieAdded → DetailTaskEnqueuer), not inline" in {
    // The runner only records + publishes; the cache fires CinemaMovieAdded for
    // the new film, the cinema's enqueuer (subscribed to the same bus) turns that
    // into one EnrichDetails task. End-to-end proof the event path replaces the
    // old inline enqueue.
    val bus     = new InProcessEventBus()
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), bus)
    val queue   = new InMemoryTaskQueue
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo")
    bus.subscribe(new DetailTaskEnqueuer(enricher, cache, queue, new InMemoryFreshnessStore).onCinemaMovieAdded)

    new CinemaScrapeRunner(cache, bus, Set.empty).run(new FakeScraper(KinoApollo, movieWithRef(KinoApollo)))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "leave the queue empty when no enqueuer is subscribed for the cinema" in {
    val bus   = new InProcessEventBus()
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), bus)
    val queue = new InMemoryTaskQueue
    new CinemaScrapeRunner(cache, bus, Set.empty).run(new FakeScraper(KinoApollo, movieWithRef(KinoApollo)))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  // ── classify: when to enrich now vs wait for deferred detail ────────────────

  "CinemaScrapeRunner.classify" should
    "hold a deferred cinema's new film (mark detailPending, emit no event) until its detail lands" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val runner  = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set(KinoApollo))
    val touched = cache.recordCinemaScrape(KinoApollo, movieWithRef(KinoApollo))

    runner.classify(KinoApollo, touched) shouldBe empty // not resolved at scrape
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(true)
  }

  it should "enrich a film with no deferred detail immediately (emit MovieDetailsComplete, no detailPending)" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val runner  = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty)
    val touched = cache.recordCinemaScrape(Multikino, movieAt(Multikino))

    runner.classify(Multikino, touched).map(_.title) shouldBe Seq("Dune")
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
  }

  it should "enrich a deferred cinema's film immediately when it carries no detail filmUrl (nothing to wait for)" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val runner  = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set(KinoApollo))
    val touched = cache.recordCinemaScrape(KinoApollo, movieAt(KinoApollo)) // filmUrl = None

    runner.classify(KinoApollo, touched).map(_.title) shouldBe Seq("Dune")
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
  }

  it should "enrich a deferred cinema's film immediately when its filmUrl is a Filmweb-fallback page (native enricher can't fetch it)" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val runner  = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set(KinoApollo))
    val fallback = Seq(CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None,
      filmUrl = Some(FilmwebShowtimesClient.filmPageUrl(1089)), synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.now(), Some("https://book")))))
    val touched = cache.recordCinemaScrape(KinoApollo, fallback)

    runner.classify(KinoApollo, touched).map(_.title) shouldBe Seq("Dune")  // not held — enriches now
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
  }
}
