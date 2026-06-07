package services.tasks

import models.{Cinema, CinemaMovie, KinoApollo, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaScrapeRunner, CinemaScraper, DetailEnricher, FilmDetail}
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

  // ── CinemaScrapeRunner enqueues detail tasks for detail-deferring cinemas ───

  private def movieWithRef(c: Cinema, title: String = "Dune") = Seq(
    CinemaMovie(Movie(title), c, posterUrl = None, filmUrl = Some(s"http://detail/$title"),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.now(), Some("https://book"))))
  )

  private def runnerWith(queue: TaskQueue, enrichers: Map[String, DetailEnricher]) =
    new CinemaScrapeRunner(
      new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus()),
      new InProcessEventBus(), queue, new InMemoryFreshnessStore, enrichers)

  "CinemaScrapeRunner" should "enqueue an EnrichDetails task per scraped film for a detail-deferring cinema" in {
    val scraper  = new FakeScraper(KinoApollo, movieWithRef(KinoApollo))
    val queue    = new InMemoryTaskQueue
    val enricher = new DetailEnricher {
      val cinema = KinoApollo; val detailGroup = "kino-apollo"
      def fetchFilmDetail(ref: String) = Some(FilmDetail())
    }
    runnerWith(queue, Map(KinoApollo.displayName -> enricher)).run(scraper)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L // one EnrichDetails task
  }

  it should "not enqueue detail tasks for a cinema without a detail enricher" in {
    val scraper = new FakeScraper(KinoApollo, movieWithRef(KinoApollo))
    val queue   = new InMemoryTaskQueue
    runnerWith(queue, Map.empty).run(scraper)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }
}
