package modules.wiring

import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, FallbackEligibility}
import services.tasks.{ChunkFanOut, ChunkRun, ChunkRunDeadlines, ChunkSlices, ChunkedListingPublisher, FailedChunkPlan, HostDownDeferrals, ChunkScrapeCoordinator, ChunkScrapePlanner, ChunkScrapeReaper, ChunkScrapeStore, MongoChunkScrapeStore, ScrapeCadence, ScrapeChunkHandler, ScrapeChunkReduceHandler, ScrapeCinemaHandler, ScrapeInFlight}
import settings.ScrapeChunkSpread


/** ── Chunked (map-reduce) scrape machinery ──────────────────────────────────
 *  A chunked cinema (ChunkedCinemaScraper) is scraped as one ScrapeChunk task
 *  per chunk, gathered by ChunkScrapeCoordinator + the ChunkScrapeReaper's per-run deadline
 *  and aggregated by one ScrapeChunkReduce task — namespaced by a per-run id so a
 *  re-scrape can't conflict with an in-flight one. See ChunkScrapeStore. */
trait ChunkScrapeWiring { self: WorkerWiring =>

  lazy val chunkScrapeStore: ChunkScrapeStore = new MongoChunkScrapeStore(mongoConnection.database)

  /** The chunked clients keyed by displayName — the plan/fetchChunk/reduce
   *  functions the chunk tasks call: each catalogued scraper's `chunkedScraper`, the
   *  same answer `cinemaScrapers` skips the per-scrape timeout by. */
  lazy val chunkScrapers: Map[String, ChunkedCinemaScraper] =
    country.cities
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c, Nil))
      .flatMap(_.chunkedScraper.map(cs => ScrapeCinemaHandler.scraperKey(cs.cinema) -> cs))
      .toMap

  /** Publish a (pre-scraped) listing through the SAME recorder + runner a live
   *  scrape uses — so the chunked reduce records uptime and falls back to Filmweb
   *  identically. Also the sink for plan-step (nav-fetch) failures. */
  private val publishScrape: CinemaScraper => Unit =
    inner => { cinemaScrapeRunner.run(recordingScraper(inner, FallbackEligibility.eligible(inner))); () }

  // Stagger a chunked venue's ScrapeChunk fan-out across this window instead of making
  // all ~200 (a full-horizon UK Flicks venue) claimable at once — otherwise the burst
  // pins the pool under strict oldest-first claim and starves the evenly-enqueued rating
  // refreshes behind it. Sized in ScrapeCadence; the planner clamps it under the run
  // stale timeout. See ChunkFanOut.chunkSpread.
  def scrapeChunkSpread: ScrapeChunkSpread = configuration.scrapeChunkSpread(ScrapeChunkSpread(ScrapeCadence.ChunkEnqueueSpread))
  lazy val chunkScrapePlanner       = new ChunkScrapePlanner(chunkScrapers, chunkScrapeStore,
    new ChunkFanOut(taskQueue, ChunkScrapePlanner.DefaultRunTimeout, scrapeChunkSpread, scrapeCostStore),
    chunkedListingPublisher, new FailedChunkPlan(publishScrape, scrapeFreshnessPolicy, venuePageHistory), clock = clock,
    runStarted = chunkRunDeadlines.arm)
  lazy val chunkPageMemo: services.tasks.ChunkPageMemo = new services.tasks.MongoChunkPageMemo(mongoConnection.database, clock = clock)
  /** Publishes a chunked venue's listing — its reduce, or an empty plan's — and stamps it. */
  lazy val chunkedListingPublisher  = new ChunkedListingPublisher(publishScrape, scrapeFreshnessPolicy)
  lazy val scrapeChunkHandler       = new ScrapeChunkHandler(chunkScrapers, chunkScrapeStore,
    new ChunkSlices(chunkPageMemo, taskPipelineMetrics), new HostDownDeferrals(clock), clock)
  lazy val scrapeChunkReduceHandler = new ScrapeChunkReduceHandler(chunkScrapers, chunkScrapeStore, chunkedListingPublisher)
  lazy val chunkScrapeCoordinator   = new ChunkScrapeCoordinator(chunkScrapeStore, taskQueue, clock)
  private lazy val chunkScrapeScheduler = threadPools.scheduler("chunk-scrape-reaper")
  /** Each chunked run's abandonment deadline, armed by the planner as it starts the run. */
  lazy val chunkRunDeadlines        = new ChunkRunDeadlines(chunkScrapeStore, chunkScrapeCoordinator, clock = clock, scheduler = chunkScrapeScheduler)
  lazy val chunkScrapeReaper        = managedResources.stopping(new ChunkScrapeReaper(chunkRunDeadlines,
    runStore = scheduledRunStore, clock = clock, ticks = ticks, scheduler = chunkScrapeScheduler))

  /** A chunked venue is mid-scrape while its run doc is live and not yet abandoned.
   *  Keeps the reaper from re-admitting it into a no-op — see [[ScrapeInFlight]]. */
  lazy val chunkRunInFlight: ScrapeInFlight = new ScrapeInFlight {
    private def live(run: ChunkRun) = !run.isStale(clock.instant(), ChunkScrapePlanner.DefaultRunTimeout)
    def isRunning(cinemaName: String): Boolean = chunkScrapeStore.activeRun(cinemaName).exists(live)
    // One read of every run for the reaper's tick: asked one venue at a time it read `scrape_runs`
    // once per due venue each minute (~24 reads/s on US, 2026-10-01).
    override def snapshot(): String => Boolean = {
      val running = chunkScrapeStore.activeRuns().filter(live).map(_.cinema).toSet
      running.contains
    }
  }
}
