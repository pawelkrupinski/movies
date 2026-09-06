package modules.wiring

import modules.WorkerWiring
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, FallbackEligibility}
import services.tasks.{ChunkScrapeCoordinator, ChunkScrapePlanner, ChunkScrapeReaper, ChunkScrapeStore, MongoChunkScrapeStore, ScrapeCadence, ScrapeChunkHandler, ScrapeChunkReduceHandler, ScrapeCinemaHandler, ScrapeInFlight}
import tools.Env

import scala.concurrent.duration.DurationLong

/** ── Chunked (map-reduce) scrape machinery ──────────────────────────────────
 *  A chunked cinema (ChunkedCinemaScraper) is scraped as one ScrapeChunk task
 *  per chunk, gathered by ChunkScrapeCoordinator + the ChunkScrapeReaper backstop
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
  def scrapeChunkSpreadMinutes: Long =
    Env.positiveLong("KINOWO_SCRAPE_CHUNK_SPREAD_MINUTES", ScrapeCadence.ChunkEnqueueSpread.toMinutes)
  lazy val chunkScrapePlanner       = new ChunkScrapePlanner(chunkScrapers, chunkScrapeStore, taskQueue, publishScrape,
    scrapeFreshnessPolicy, chunkSpread = scrapeChunkSpreadMinutes.minutes)
  lazy val scrapeChunkHandler       = new ScrapeChunkHandler(chunkScrapers, chunkScrapeStore)
  lazy val scrapeChunkReduceHandler = new ScrapeChunkReduceHandler(chunkScrapers, chunkScrapeStore, publishScrape,
    scrapeFreshnessPolicy)
  lazy val chunkScrapeCoordinator   = new ChunkScrapeCoordinator(chunkScrapeStore, taskQueue)
  lazy val chunkScrapeReaper        = new ChunkScrapeReaper(chunkScrapeStore, taskQueue, chunkScrapeCoordinator,
    runStore = scheduledRunStore)

  /** A chunked venue is mid-scrape while its run doc is live and not yet abandoned.
   *  Keeps the reaper from re-admitting it into a no-op — see [[ScrapeInFlight]]. */
  lazy val chunkRunInFlight: ScrapeInFlight = (cinemaName: String) =>
    chunkScrapeStore.activeRun(cinemaName)
      .exists(!_.isStale(java.time.Instant.now(), ChunkScrapePlanner.DefaultRunTimeout))
}
