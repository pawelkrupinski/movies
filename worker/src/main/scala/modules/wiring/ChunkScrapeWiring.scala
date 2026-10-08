package modules.wiring

import settings.ScrapeChunkSpread

import modules.WorkerWiring
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, FallbackEligibility}
import services.tasks.{ChunkRun, ChunkScrapeCoordinator, ChunkScrapePlanner, ChunkScrapeReaper, ChunkScrapeStore, MongoChunkScrapeStore, ScrapeCadence, ScrapeChunkHandler, ScrapeChunkReduceHandler, ScrapeCinemaHandler, ScrapeInFlight}


/** ── Chunked (map-reduce) scrape machinery ──────────────────────────────────
 *  A chunked cinema (ChunkedCinemaScraper) is scraped as one ScrapeChunk task
 *  per chunk, gathered by ChunkScrapeCoordinator + the ChunkScrapeReaper's per-run deadline
 *  and aggregated by one ScrapeChunkReduce task — namespaced by a per-run id so a
 *  re-scrape can't conflict with an in-flight one. See ChunkScrapeStore. */
trait ChunkScrapeWiring { self: WorkerWiring =>

  lazy val chunkScrapeStore: ChunkScrapeStore = new MongoChunkScrapeStore(mongoConnection.database)

  /** Raw chunked clients keyed by displayName — the plan/fetchChunk/reduce
   *  functions the chunk tasks call. Empty until a client opts in. */
  lazy val chunkScrapers: Map[String, ChunkedCinemaScraper] =
    country.cities
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .collect { case cs: ChunkedCinemaScraper => ScrapeCinemaHandler.scraperKey(cs.cinema) -> cs }
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
  // stale timeout. See ChunkScrapePlanner.chunkSpread.
  def scrapeChunkSpread: ScrapeChunkSpread = configuration.scrapeChunkSpread(ScrapeChunkSpread(ScrapeCadence.ChunkEnqueueSpread))
  lazy val chunkScrapePlanner       = new ChunkScrapePlanner(chunkScrapers, chunkScrapeStore, taskQueue, publishScrape,
    scrapeFreshnessPolicy, chunkSpread = scrapeChunkSpread, costs = scrapeCostStore, clock = clock,
    runStarted = chunkScrapeReaper.armDeadline)
  lazy val chunkPageMemo: services.tasks.ChunkPageMemo = new services.tasks.MongoChunkPageMemo(mongoConnection.database, clock = clock)
  lazy val scrapeChunkHandler       = new ScrapeChunkHandler(chunkScrapers, chunkScrapeStore,
    pageMemo = chunkPageMemo, memoMetrics = taskMetrics, clock = clock)
  lazy val scrapeChunkReduceHandler = new ScrapeChunkReduceHandler(chunkScrapers, chunkScrapeStore, publishScrape,
    scrapeFreshnessPolicy, clock = clock)
  lazy val chunkScrapeCoordinator   = new ChunkScrapeCoordinator(chunkScrapeStore, taskQueue, clock)
  lazy val chunkScrapeReaper        = managedResources.stopping(new ChunkScrapeReaper(chunkScrapeStore, taskQueue, chunkScrapeCoordinator,
    runStore = scheduledRunStore, clock = clock))

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
