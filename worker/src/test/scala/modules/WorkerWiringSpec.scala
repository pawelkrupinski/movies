package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.ShowtimeCache
import services.movies.MovieService
import tools.{ExecutorProbes, TestWiring}

/** The worker composition root must boot BOTH halves of the write pipeline:
 *  the scrape side (`showtimeCache`) and the enrich/TMDB side (`movieService`).
 *  `start()` is what production calls; this asserts it reaches both cascade
 *  entry points.
 *
 *  Deterministic spy approach (no network): a `TestWiring` (disabled Mongo,
 *  stub TMDB key) with `showtimeCache` and `movieService` overridden by spy
 *  subclasses whose `start()` only records the call — the real `start()` (which
 *  schedules background pools) is never invoked, so nothing touches Mongo or the
 *  network. We then assert both flags flipped. The spies reuse the EXACT
 *  constructor arguments the wiring passes, so the seam stays faithful. */
class WorkerWiringSpec extends AnyFlatSpec with Matchers {

  class SpyWiring extends TestWiring {
    @volatile var showtimeStarted = false
    @volatile var movieServiceStarted = false

    override lazy val showtimeCache: ShowtimeCache = new ShowtimeCache(
      cinemaScrapers, eventBus, movieCache, showtimeFetchEc
    ) {
      override def start(): Unit = showtimeStarted = true
    }

    override lazy val movieService: MovieService = new MovieService(
      movieCache, eventBus, tmdbClient, backgroundBudget.ec("enrichment-worker")
    ) {
      override def start(): Unit = movieServiceStarted = true
    }
  }

  "WorkerWiring.start()" should "boot both the scrape and the enrichment cascade" in {
    val wiring = new SpyWiring
    wiring.start()
    wiring.showtimeStarted shouldBe true
    wiring.movieServiceStarted shouldBe true
    wiring.stop()
  }

  // The number of cinemas that scrape concurrently. Probe the EXACT EC the
  // wiring hands to `ShowtimeCache`: launch more tasks than the cap and assert
  // the peak in-flight tops out at `scrapeConcurrency`. Guards the default
  // against accidental drift (fails at the old value of 2).
  "the showtime-fetch pool" should "let scrapeConcurrency (default 4) cinemas scrape at once" in {
    val wiring = new SpyWiring
    wiring.scrapeConcurrency shouldBe 4
    val ec = wiring.showtimeFetchEc
    ExecutorProbes.peakConcurrency(10, IndexedSeq(ec)) shouldBe wiring.scrapeConcurrency
    wiring.stop()
  }
}
