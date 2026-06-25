package services.tasks

import play.api.Logging
import services.cinemas.{ChunkedCinemaScraper, CinemaScraper, PreScrapedCinemaScraper}

import java.time.Clock
import scala.concurrent.duration._

/**
 * The PLAN ("split") step of a chunked scrape, called from `ScrapeCinemaHandler`
 * for a chunked cinema: enumerate the chunk keys (a nav/index fetch), start a
 * run (the per-cinema mutex in [[ChunkScrapeStore]]), and enqueue one
 * `ScrapeChunk` task per key.
 *
 * Failure handling matches a normal scrape: a nav-fetch throw — or a genuinely
 * empty repertoire — is published immediately as the scrape's outcome through
 * `publishScrape` (uptime red/white + Filmweb fallback for eligible venues),
 * rather than starting an empty run. If a run is already active for the cinema,
 * planning is a no-op (one run at a time) — duplicate `ScrapeCinema` tasks are
 * harmless.
 */
class ChunkScrapePlanner(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  queue:         TaskQueue,
  publishScrape: CinemaScraper => Unit,
  staleAfter:    FiniteDuration = ChunkScrapePlanner.DefaultRunTimeout,
  clock:         Clock          = Clock.systemUTC()
) extends Logging {

  def isChunked(cinema: String): Boolean = chunkScrapers.contains(cinema)

  /** Returns the number of `ScrapeChunk` tasks enqueued (0 when a run was already
   *  active, or when the plan was published directly as empty/failed). */
  def plan(cinema: String): Int = chunkScrapers.get(cinema) match {
    case None => 0
    case Some(scraper) =>
      val keys =
        try scraper.planChunks()
        catch { case e: Exception => publishFailure(scraper, e); return 0 }

      if (keys.isEmpty) { publishEmpty(scraper); return 0 }

      store.startRun(cinema, keys, clock.instant(), staleAfter) match {
        case None => 0 // a run is already active for this cinema
        case Some(runId) =>
          val n = keys.count(k => queue.enqueue(TaskType.ScrapeChunk,
            ChunkScrapeKeys.chunkDedup(cinema, runId, k),
            ChunkScrapeKeys.chunkPayload(cinema, runId, k)) == EnqueueResult.Added)
          logger.info(s"$cinema chunked scrape run $runId: enqueued $n/${keys.size} chunk task(s)")
          n
      }
  }

  private def publishEmpty(scraper: ChunkedCinemaScraper): Unit =
    try publishScrape(new PreScrapedCinemaScraper(scraper.cinema, scraper.scrapeHosts, scraper.chain, () => Seq.empty))
    catch { case _: Exception => () }

  private def publishFailure(scraper: ChunkedCinemaScraper, e: Exception): Unit = {
    logger.warn(s"chunked plan for ${scraper.cinema.displayName} failed: ${e.getMessage}")
    try publishScrape(new PreScrapedCinemaScraper(scraper.cinema, scraper.scrapeHosts, scraper.chain, () => throw e))
    catch { case _: Exception => () }
  }
}

object ChunkScrapePlanner {
  /** A run with no fresh completion this long is considered abandoned: a new
   *  `ScrapeCinema` supersedes it, and the backstop reduces its partial data. */
  val DefaultRunTimeout: FiniteDuration = 15.minutes
}
