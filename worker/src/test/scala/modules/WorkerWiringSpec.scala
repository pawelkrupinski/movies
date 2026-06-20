package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.events.TmdbResolved
import services.tasks.{ScrapeReaper, UnresolvedTmdbReaper}
import tools.TestWiring

/** The worker composition root must boot BOTH halves of the write pipeline:
 *  the scrape side (the queue-driven `scrapeReaper`) and the enrich/TMDB side
 *  (the `unresolvedTmdbReaper`, which drives the phase-spread TMDB re-resolve —
 *  the role MovieService's old daily scheduler used to own). `start()` is what
 *  production calls; this asserts it reaches both cascade entry points.
 *
 *  Deterministic spy approach (no network): a `TestWiring` (disabled Mongo, stub
 *  TMDB key, in-memory task queue + freshness store so the unconditional queue
 *  path boots without a cluster) with `scrapeReaper` + `unresolvedTmdbReaper`
 *  overridden by spy subclasses whose `start()` only records the call — the real
 *  `start()` (which schedules background pools) is never invoked for those two,
 *  so nothing touches the network. We then assert both flags flipped. */
class WorkerWiringSpec extends AnyFlatSpec with Matchers {

  class SpyWiring extends TestWiring {
    @volatile var scrapeStarted = false
    @volatile var tmdbRetryStarted = false

    override lazy val scrapeReaper: ScrapeReaper =
      new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore) {
        override def start(): Unit = scrapeStarted = true
      }

    override lazy val unresolvedTmdbReaper: UnresolvedTmdbReaper =
      new UnresolvedTmdbReaper(movieCache, movieService.retryResolve) {
        override def start(): Unit = tmdbRetryStarted = true
      }
  }

  "WorkerWiring.start()" should "boot both the scrape and the enrichment cascade" in {
    val wiring = new SpyWiring
    wiring.start()
    wiring.scrapeStarted shouldBe true
    wiring.tmdbRetryStarted shouldBe true
    wiring.stop()
  }

  // Smoothing guard: a TMDB resolution must NOT directly enqueue rating tasks.
  // The old `RatingEnqueuer` subscribed to `TmdbResolved` and fanned out four
  // rating tasks per event instantly (the unspread amplifier behind the midday
  // `kinowo_worker_tasks` rating spikes). With it removed, the EnrichmentReaper
  // is the sole, capped + phase-spread rating-enqueue path — so publishing the
  // event leaves the queue untouched. (Fails before the cascade removal: the
  // event would have enqueued four tasks.)
  it should "not enqueue rating tasks on a resolution event (ratings flow only via the EnrichmentReaper)" in {
    val wiring = new SpyWiring
    val before = wiring.taskQueue.countByState().values.sum
    wiring.eventBus.publish(TmdbResolved("Dune", Some(2024), "tt1"))
    wiring.taskQueue.countByState().values.sum shouldBe before
    wiring.stop()
  }
}
