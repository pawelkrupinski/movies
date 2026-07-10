package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import models.Country
import services.MongoConnection
import services.events.ImdbIdMissing
import services.tasks.{ScrapeReaper, TaskType, UnresolvedTmdbReaper}
import tools.{ExecutionBudget, SharedExecutionBudget, TestWiring}

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

    override lazy val scrapeReaper: ScrapeReaper =
      new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore) {
        override def start(): Unit = scrapeStarted = true
      }

    override lazy val unresolvedTmdbReaper: UnresolvedTmdbReaper =
      new UnresolvedTmdbReaper(movieCache, movieService.retryResolve) {
        override def start(): Unit = tmdbRetryStarted = true
      }
  }

  // A Filmweb-disabled country: same test seams as SpyWiring, but the per-country
  // Filmweb gate pinned off (Country is sealed with only Poland — filmwebEnabled
  // is the wiring's gate, so overriding it is how we simulate a non-Filmweb country).
  class NoFilmwebWiring extends SpyWiring {
    override protected def filmwebEnabled: Boolean = false
  }

  // A minimal wiring that varies the WorkerWiring CONSTRUCTOR (country + injected
  // budget) — which `TestWiring` can't, since it fixes the no-arg super-constructor.
  // Mongo is pinned disabled so nothing connects; we only read the derivation seams.
  class Probe(c: Country, b: ExecutionBudget) extends WorkerWiring(c, b) {
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = "unused", required = false)
    def dbNameForTest: String              = mongoDbName
    def defaultScrapeCitiesForTest: Set[String] = scrapeCitiesDefault
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

  // Dropping the throttle restart (2026-07-03): a sustained credit-floor throttle is a
  // STRUCTURAL deficit (steady CPU ~0.30 cores just over the shared-cpu-4x earn rate
  // ~0.26), NOT a wedge a reboot can clear — restarting only burned the ~16k boot
  // re-grant and looped every ~45min while the box was near-idle. Both throttle paths
  // (the CpuCreditPoller downslope projection and the ThrottleStuckWatchdog 45-min
  // wedge) are wired to `onThrottleWedged`, which must ALARM, never restart the machine.
  it should "alarm, not restart the machine, when the CPU-credit throttle wedges" in {
    val wiring = new SpyWiring
    wiring.onThrottleWedged("stuck watchdog")
    wiring.onThrottleWedged("projection downslope")
    wiring.restartRequests shouldBe empty
    wiring.stop()
  }

  // Per-country Filmweb gate: a Filmweb-enabled country (Poland) wires the Filmweb
  // rating handler + bulk-refresh handler; a disabled country wires neither, so its
  // TaskWorker can't run any Filmweb task and no Filmweb source is constructed.
  "The Filmweb path" should "be wired for a Filmweb-enabled country and dropped for a disabled one" in {
    val enabled  = new SpyWiring
    enabled.ratingHandlers.map(_.taskType)   should contain (TaskType.FilmwebRating: TaskType)
    enabled.operatorHandlers.map(_.taskType) should contain (TaskType.RefreshAllFilmweb: TaskType)
    enabled.stop()

    val disabled = new NoFilmwebWiring
    disabled.ratingHandlers.map(_.taskType)   should not contain (TaskType.FilmwebRating: TaskType)
    disabled.operatorHandlers.map(_.taskType) should not contain (TaskType.RefreshAllFilmweb: TaskType)
    // The other three rating sources are untouched by the gate.
    disabled.ratingHandlers.map(_.taskType) should contain allOf
      (TaskType.ImdbRating: TaskType, TaskType.RtRating: TaskType, TaskType.McRating: TaskType)
    disabled.stop()
  }

  // The whole point of hoisting the budget into WorkerMain: N country wirings draw
  // permits from ONE shared SharedExecutionBudget (one Semaphore/cap), and each
  // wiring scopes to its own country's cities + database.
  "Two country wirings" should "share one injected background budget and scope to their own country" in {
    val budget = new SharedExecutionBudget(4)
    val w1 = new Probe(Country.Poland, budget)
    val w2 = new Probe(Country.Poland, budget)

    (w1.backgroundBudget eq budget)              shouldBe true
    (w2.backgroundBudget eq budget)              shouldBe true
    (w1.backgroundBudget eq w2.backgroundBudget) shouldBe true

    w1.country shouldBe Country.Poland
    w1.defaultScrapeCitiesForTest shouldBe Country.Poland.cities.map(_.slug).toSet
    w1.dbNameForTest              shouldBe Country.dbNameFor(Country.Poland)
  }
}
