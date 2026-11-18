package services.tasks

import settings.{ScrapeBootRamp, ScrapeChunkSpread, ScrapeEnqueueSpreadSlices, ScrapeInitialDelay, ScrapeMaxEnqueuePerTick, ScrapeMaxOutstandingTasks, ScrapeTasksPerVenue}
import services.cinemas.common.CinemaScraper
import services.freshness.{Freshness, FreshnessStore}
import services.schedule.{AlwaysClaimScheduledRunStore, ScheduledRunStore}
import tools.TickRecorder

import java.time.Clock
import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.duration._

/** A [[ScrapeReaper]] assembled from its parts the way the worker's wiring assembles it, every setting
 *  defaulted as the parts default it: what the specs build a reaper with. `scheduleSlice`, when given,
 *  replaces the tick's deferral of its spread slices (observed by hand, no wall-clock wait). */
object ScrapeReapers {

  def apply(
    scrapers:  Seq[CinemaScraper],
    queue:     TaskProducer,
    freshness: FreshnessStore,
    dueWindow: DueWindow = new DueWindow(Freshness.DefaultScrapeTtl),
    interval:  ScrapeReaper.TickInterval = ScrapeReaper.TickInterval(1.minute),
    initialDelay: ScrapeInitialDelay = ScrapeInitialDelay(0.seconds),
    readyTimeout: ScrapeReaper.ReadyTimeout = ScrapeReaper.ReadyTimeout(30.seconds),
    maxEnqueuePerTick: ScrapeMaxEnqueuePerTick = ScrapeMaxEnqueuePerTick(Int.MaxValue),
    bootRamp: ScrapeBootRamp = ScrapeBootRamp(0.seconds),
    maxOutstandingScrapeTasks: ScrapeMaxOutstandingTasks = ScrapeMaxOutstandingTasks(Int.MaxValue),
    costs: ScrapeCostEstimates = new ScrapeCostEstimates(ScrapeTasksPerVenue(1)),
    inFlight: ScrapeInFlight = ScrapeInFlight.Never,
    chunkSpread: ScrapeChunkSpread = ScrapeChunkSpread(Duration.Zero),
    enqueueSpread: ScrapeEnqueueSpreadSlices = ScrapeEnqueueSpreadSlices(1),
    runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
    clock:    Clock,
    scheduler: ScheduledExecutorService,
    ticks: TickRecorder = TickRecorder.Unrecorded,
    scheduleSlice: Option[(FiniteDuration, Runnable) => Unit] = None
  ): ScrapeReaper = {
    val due       = new DueScrapes(scrapers, freshness, dueWindow, inFlight)
    val admission = new ScrapeAdmission(scrapers.size, dueWindow.period, interval.value, maxEnqueuePerTick, bootRamp,
      maxOutstandingScrapeTasks, costs, chunkSpread)
    val enqueuer  = new ScrapeEnqueuer(queue, admission, clock)
    val tick = scheduleSlice match {
      case None => new ScrapeTick(due, admission, enqueuer, interval.value, enqueueSpread, ticks, scheduler)
      case Some(deferred) => new ScrapeTick(due, admission, enqueuer, interval.value, enqueueSpread, ticks, scheduler) {
        override protected def scheduleSlice(delay: FiniteDuration, task: Runnable): Unit = deferred(delay, task)
      }
    }
    new ScrapeReaper(due, tick, interval, initialDelay, readyTimeout, runStore, clock, scheduler, ticks)
  }
}
