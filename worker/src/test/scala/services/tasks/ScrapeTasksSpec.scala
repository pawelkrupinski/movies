package services.tasks

import models.{Cinema, CinemaMovie, KinoApollo, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaScrapeRunner, CinemaScraper, FakeDetailEnricher}
import services.events.InProcessEventBus
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import java.time.LocalDateTime

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
    new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus()),
    new InProcessEventBus()
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

  // Boot-cost guard: on a cold worker every cinema is stale, so the first tick
  // enqueues all of them at once for the TaskWorker to drain — the queue-mode
  // boot scrape burst. `start()` must hold that first tick back by `initialDelay`
  // so it doesn't pile onto the cold-JVM cache hydrate. (Before this change the
  // initial delay was a hardcoded `0L`, so the tick fired immediately and the
  // `shouldBe 0L` assertion below would fail.)
  it should "hold the first tick until the initial delay elapses" in {
    import scala.concurrent.duration._
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
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepo(), bus)
    val queue   = new InMemoryTaskQueue
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo")
    bus.subscribe(new DetailTaskEnqueuer(enricher, cache, queue, new InMemoryFreshnessStore).onCinemaMovieAdded)

    new CinemaScrapeRunner(cache, bus).run(new FakeScraper(KinoApollo, movieWithRef(KinoApollo)))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "leave the queue empty when no enqueuer is subscribed for the cinema" in {
    val bus   = new InProcessEventBus()
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(), bus)
    val queue = new InMemoryTaskQueue
    new CinemaScrapeRunner(cache, bus).run(new FakeScraper(KinoApollo, movieWithRef(KinoApollo)))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }
}
