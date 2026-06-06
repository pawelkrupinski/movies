package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.ShowtimeCache
import services.movies.MovieService
import tools.TestWiring

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
      cinemaScrapers, eventBus, movieCache,
      backgroundBudget.ec("showtime-fetch", tools.Env.positiveInt("KINOWO_SCRAPE_CONCURRENCY", 2))
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
}
