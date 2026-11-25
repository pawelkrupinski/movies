package services.tasks

import settings.{ScrapeBootRamp, ScrapeChunkSpread, ScrapeEnqueueSpreadSlices, ScrapeInitialDelay, ScrapeMaxEnqueuePerTick, ScrapeMaxOutstandingTasks, ScrapeTasksPerVenue}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScrapeRunner, CinemaScraper}
import services.freshness.{Freshness, FreshnessStore}
import services.scrapes.{ScrapeArchiveReader, ScrapeArchiveRepository}
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

/** A [[ScrapeCinemaHandler]] assembled from its parts as the worker's wiring assembles it: what the specs
 *  handle `ScrapeCinema` tasks with. Without `scrapeFreshness`, a policy of its own over `freshness`. */
object ScrapeCinemaHandlers {

  def apply(
    scrapersByKey: Map[String, CinemaScraper],
    runner:        CinemaScrapeRunner,
    freshness:     FreshnessStore,
    dueWindow:     DueWindow = new DueWindow(Freshness.DefaultScrapeTtl),
    clock:         Clock,
    chunkPlanner:  Option[ChunkScrapePlanner] = None,
    scrapeFreshness: Option[ScrapeFreshnessPolicy] = None,
    scrapeArchive: ScrapeArchiveReader = ScrapeArchiveRepository.empty,
    costs:         ScrapeCostStore = ScrapeCostStore.Discarding
  ): ScrapeCinemaHandler = {
    val outcome = scrapeFreshness.getOrElse(new ScrapeFreshnessPolicy(freshness, clock = clock))
    val pages   = new VenuePageHistory(scrapeArchive, clock)
    new ScrapeCinemaHandler(scrapersByKey, new DueScrapes(scrapersByKey.values.toSeq, freshness, dueWindow),
      new PlainCinemaScrape(runner, outcome, pages, costs, clock, tools.SpecStopwatch.Frozen), chunkPlanner, pages, outcome, clock)
  }
}

/** A [[ChunkScrapePlanner]] assembled from its parts as the worker's wiring assembles it. */
object ChunkScrapePlanners {

  def apply(
    chunkScrapers:   Map[String, ChunkedCinemaScraper],
    store:           ChunkScrapeStore,
    queue:           TaskProducer,
    publishScrape:   CinemaScraper => Unit,
    scrapeFreshness: ScrapeFreshnessPolicy,
    staleAfter:      ChunkScrapePlanner.RunTimeout = ChunkScrapePlanner.RunTimeout(ChunkScrapePlanner.DefaultRunTimeout),
    clock:           Clock,
    chunkSpread:     ScrapeChunkSpread = ScrapeChunkSpread(Duration.Zero),
    costs:           ScrapeCostStore = ScrapeCostStore.Discarding,
    runStarted:      ChunkRun => Unit = _ => (),
    scrapeArchive:   ScrapeArchiveReader = ScrapeArchiveRepository.empty
  ): ChunkScrapePlanner =
    new ChunkScrapePlanner(chunkScrapers, store, new ChunkFanOut(queue, staleAfter.value, chunkSpread, costs),
      new ChunkedListingPublisher(publishScrape, scrapeFreshness),
      new FailedChunkPlan(publishScrape, scrapeFreshness, new VenuePageHistory(scrapeArchive, clock)), staleAfter, clock, runStarted)
}

/** The chunked scrape's handlers and reaper assembled from their parts as the worker's wiring assembles them. */
object ChunkScrapeParts {

  def chunkHandler(chunkScrapers: Map[String, ChunkedCinemaScraper], store: ChunkScrapeStore, clock: Clock,
                   pageMemo: ChunkPageMemo = ChunkPageMemo.none,
                   memoMetrics: ChunkPageMemoMetrics = ChunkPageMemoMetrics.noop): ScrapeChunkHandler =
    new ScrapeChunkHandler(chunkScrapers, store, new ChunkSlices(pageMemo, memoMetrics), new HostDownDeferrals(clock), clock)

  def reduceHandler(chunkScrapers: Map[String, ChunkedCinemaScraper], store: ChunkScrapeStore,
                    publishScrape: CinemaScraper => Unit, scrapeFreshness: ScrapeFreshnessPolicy): ScrapeChunkReduceHandler =
    new ScrapeChunkReduceHandler(chunkScrapers, store, new ChunkedListingPublisher(publishScrape, scrapeFreshness))

  def reaper(store: ChunkScrapeStore, coordinator: ChunkScrapeCoordinator,
             staleAfter: ChunkScrapePlanner.RunTimeout = ChunkScrapePlanner.RunTimeout(ChunkScrapePlanner.DefaultRunTimeout),
             runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore, clock: Clock, scheduler: ScheduledExecutorService,
             ticks: TickRecorder = TickRecorder.Unrecorded): ChunkScrapeReaper =
    new ChunkScrapeReaper(new ChunkRunDeadlines(store, coordinator, staleAfter, clock, scheduler),
      runStore = runStore, clock = clock, scheduler = scheduler, ticks = ticks)
}
