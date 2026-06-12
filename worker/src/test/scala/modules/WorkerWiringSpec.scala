package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.freshness.{FreshnessStore, InMemoryFreshnessStore}
import services.movies.MovieService
import services.tasks.{InMemoryTaskQueue, ScrapeReaper, TaskQueue}
import tools.TestWiring

/** The worker composition root must boot BOTH halves of the write pipeline:
 *  the scrape side (the queue-driven `scrapeReaper`) and the enrich/TMDB side
 *  (`movieService`). `start()` is what production calls; this asserts it reaches
 *  both cascade entry points.
 *
 *  Deterministic spy approach (no network): a `TestWiring` (disabled Mongo, stub
 *  TMDB key) with the Mongo-backed task queue + freshness store swapped for
 *  in-memory fakes (so the unconditional queue path boots without a cluster),
 *  and `scrapeReaper` + `movieService` overridden by spy subclasses whose
 *  `start()` only records the call — the real `start()` (which schedules
 *  background pools) is never invoked for those two, so nothing touches the
 *  network. We then assert both flags flipped. */
class WorkerWiringSpec extends AnyFlatSpec with Matchers {

  class SpyWiring extends TestWiring {
    @volatile var scrapeStarted = false
    @volatile var movieServiceStarted = false

    // Swap the Mongo-backed queue + freshness store for in-memory fakes so the
    // (now unconditional) queue path boots without a cluster.
    override lazy val taskQueue: TaskQueue = new InMemoryTaskQueue
    override lazy val freshnessStore: FreshnessStore = new InMemoryFreshnessStore

    override lazy val scrapeReaper: ScrapeReaper =
      new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore) {
        override def start(): Unit = scrapeStarted = true
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
    wiring.scrapeStarted shouldBe true
    wiring.movieServiceStarted shouldBe true
    wiring.stop()
  }
}
