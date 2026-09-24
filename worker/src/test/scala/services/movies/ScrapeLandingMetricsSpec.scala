package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * `ScrapeLandingMetrics` actually gets called — the guard-verdict and
 * write-skipped counters `WorkerTaskMetrics` publishes to Prometheus are only as
 * good as `ScrapeLanding` calling them at the right moments, which the metrics
 * trait's own unit tests (there are none — it is one line per method) cannot
 * check. Pinned here rather than by reading `WorkerTaskMetrics.Series`'s
 * Prometheus counters directly, the same way `MovieCacheSpec`'s
 * `RecordingCacheMetrics` pins `recordRehydrate`.
 */
class ScrapeLandingMetricsSpec extends AnyFlatSpec with Matchers {

  private def deepScrape(films: Int, showtimesEach: Int): Seq[CinemaMovie] =
    (1 to films).map { i =>
      val times = DepthGuardTime.showtimes(showtimesEach)
      CinemaMovie(movie = Movie(s"Film $i", releaseYear = Some(2026)), cinema = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = times)
    }

  private def splitRepository() = new InMemoryMovieRepository(
    screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository))


  "the depth guard" should "record a reject then an accept through ScrapeLandingMetrics" in {
    val repository = splitRepository()
    val metrics    = new RecordingScrapeLandingMetrics
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock,
      maxConsecutiveGuardRejections = 1, scrapeLandingMetrics = metrics)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    metrics.verdicts shouldBe empty // the healthy first scrape records nothing

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1)) // 1st reject: held
    metrics.verdicts shouldBe Vector(ScrapeLandingMetrics.Guard.Depth -> ScrapeLandingMetrics.Verdict.Reject)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1)) // 2nd reject: accepted (cap=1)
    metrics.verdicts shouldBe Vector(
      ScrapeLandingMetrics.Guard.Depth -> ScrapeLandingMetrics.Verdict.Reject,
      ScrapeLandingMetrics.Guard.Depth -> ScrapeLandingMetrics.Verdict.Accept)
  }

  "the breadth guard" should "record a reject then an accept through ScrapeLandingMetrics" in {
    val repository = splitRepository()
    val metrics    = new RecordingScrapeLandingMetrics
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock,
      maxConsecutiveGuardRejections = 1, scrapeLandingMetrics = metrics)

    // 20 films, then a stable 9-of-20 with MORE showtimes each so the depth axis never
    // engages — isolating the breadth guard, exactly as DepthGuardUnderSplitSpec does.
    cache.recordCinemaScrape(Multikino, deepScrape(films = 20, showtimesEach = 12))
    metrics.verdicts shouldBe empty

    cache.recordCinemaScrape(Multikino, deepScrape(films = 9, showtimesEach = 30)) // 1st reject: held
    metrics.verdicts shouldBe Vector(ScrapeLandingMetrics.Guard.Breadth -> ScrapeLandingMetrics.Verdict.Reject)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 9, showtimesEach = 30)) // 2nd reject: accepted (cap=1)
    metrics.verdicts shouldBe Vector(
      ScrapeLandingMetrics.Guard.Breadth -> ScrapeLandingMetrics.Verdict.Reject,
      ScrapeLandingMetrics.Guard.Breadth -> ScrapeLandingMetrics.Verdict.Accept)
  }

  // The write-skip counters (`cache-miss-race` / `unreadable-row`) are pinned against
  // the two scenarios that already reproduce them, in `UnreadableRowScrapeSpec`.
}
