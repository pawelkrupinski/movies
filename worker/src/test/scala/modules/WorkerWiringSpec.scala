package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.events.ImdbIdMissing
import services.tasks.{ScrapeReaper, UnresolvedTmdbReaper}
import tools.{SharedExecutionBudget, TestWiring}

import scala.concurrent.duration._

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
    // Records restart requests instead of exiting the JVM, so a wedge response is observable.
    @volatile var restartRequests = Vector.empty[String]
    override protected[modules] def restartMachine(reason: String): Unit = restartRequests :+= reason
    // In-memory wedge-restart marker (no /data volume in tests) so the cooldown is drivable.
    @volatile var wedgeMarker: Option[java.time.Instant] = None
    override protected[modules] def lastWedgeRestartAt: Option[java.time.Instant] = wedgeMarker
    override protected[modules] def recordWedgeRestart(at: java.time.Instant): Unit = wedgeMarker = Some(at)

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

  // Smoothing guard: a resolution BUS EVENT must NOT fan out rating tasks. The old
  // cascade subscribed the rating fetchers to the resolution events and dumped four
  // rating tasks per event instantly (the unspread amplifier behind the midday
  // `kinowo_worker_tasks` rating spikes). Rating enqueues now come from only two
  // bounded paths: the EnrichmentReaper's capped + phase-spread corpus sweep, and the
  // immediate newcomer-fold kick (a trickle — a few promotions a day). `ImdbIdMissing`
  // (the one surviving resolution event) drives id recovery alone, never a rating
  // enqueue — so publishing it on its own leaves the queue untouched.
  it should "not fan out rating tasks when a resolution event fires (only the reaper + newcomer fold enqueue ratings)" in {
    val wiring = new SpyWiring
    val before = wiring.taskQueue.countByState().values.sum
    wiring.eventBus.publish(ImdbIdMissing("Dune", Some(2024), "Dune"))
    wiring.taskQueue.countByState().values.sum shouldBe before
    wiring.stop()
  }

  // Smoothing lever: the reapers are wired with the finer (≤1min) tick interval,
  // so the rating/detail sweeps enqueue a flat per-minute trickle instead of
  // dumping a 5-min-wide backlog in one tick (the residual `kinowo_worker_tasks`
  // spikes). The reapers now read the interval by-name (a live, mid-flight-
  // flippable knob), so this guards the wiring value the composition root supplies.
  it should "wire both reapers with a sub-5-minute tick interval so enqueues stay flat" in {
    val wiring = new SpyWiring
    wiring.enrichmentTickInterval should be <= (1.minute: FiniteDuration)
    wiring.detailTickInterval     should be <= (1.minute: FiniteDuration)
    wiring.stop()
  }

  // Smoothing default: scrape + enrichment + the rating refreshers share ONE
  // background concurrency budget; capping it at 4 (was 8) is what flattens the
  // per-tick CPU burst that drives the shared-cpu credit downslope — a 2026-06-27
  // live A/B showed 4 ~halved the burst (busy p95 156→58 centi-cores) at unchanged
  // scrape throughput. Guards the composition-root default against a silent bump.
  it should "cap the shared background concurrency budget at 4 by default" in {
    val wiring = new SpyWiring
    wiring.backgroundBudget match {
      case budget: SharedExecutionBudget => budget.maxConcurrent shouldBe 4
      case other                         => fail(s"expected a SharedExecutionBudget, got $other")
    }
    wiring.stop()
  }

  // The CpuCreditPoller's fast-drain DOWNSLOPE projection (credit still above the floor)
  // is NOT a wedge — the reaper backoff the throttle engages is the response, and a
  // restart would only burn the boot re-grant. So the projection path must ALARM only.
  it should "alarm, not restart, on the CpuCreditPoller fast-drain downslope" in {
    val wiring = new SpyWiring
    wiring.onThrottleWedged("projection downslope")
    wiring.restartRequests shouldBe empty
    wiring.stop()
  }

  // A genuine STUCK wedge (ThrottleStuckWatchdog: throttled continuously past the
  // threshold AND credit not recovering) is usually a reboot-clearable epoll spin on a
  // wedged Mongo socket — restarting drops the wedged FD. With no prior restart marker,
  // the first wedge restarts (and stamps the marker so the cooldown survives the reboot).
  it should "restart the machine on the first genuine stuck credit wedge" in {
    val wiring = new SpyWiring
    wiring.onCreditWedgeStuck("stuck watchdog")
    wiring.restartRequests shouldBe Vector("stuck watchdog")
    wiring.wedgeMarker      should not be empty  // stamped so a re-wedge hits the cooldown
    wiring.stop()
  }

  // The loop guard: a second stuck wedge WITHIN the cooldown must NOT restart again — a
  // re-wedge (or a structural floor a reboot can't clear) can never spiral the machine,
  // the ~45min loop the 2026-07-03 removal fought. The cooldown persists across reboots.
  it should "NOT restart again on a stuck wedge within the cooldown (rides it out)" in {
    val wiring = new SpyWiring
    wiring.wedgeMarker = Some(java.time.Instant.now().minusSeconds(60))  // just restarted
    wiring.onCreditWedgeStuck("stuck watchdog")
    wiring.restartRequests shouldBe empty
    wiring.stop()
  }
}
