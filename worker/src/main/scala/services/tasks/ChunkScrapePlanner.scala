package services.tasks

import play.api.Logging
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, PreScrapedCinemaScraper}

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
  clock:         Clock          = Clock.systemUTC(),
  // Stagger this run's `ScrapeChunk` fan-out evenly across this window (chunk k of
  // n becomes claimable at `+ chunkSpread·k/n`) instead of making all n claimable
  // at once — so a big full-horizon venue can't monopolise the pool and starve the
  // evenly-enqueued ratings (see [[services.tasks.ScrapeCadence.ChunkEnqueueSpread]]).
  // Clamped to a third of `staleAfter` so every chunk still becomes eligible AND
  // drains before the run is abandoned to a partial reduce. Default `Zero` disables
  // the spread, leaving the flow tests that drive `plan()` + `drain(now)` directly
  // (and the deterministic fixture harness) unaffected.
  chunkSpread:   FiniteDuration = Duration.Zero
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

      val now = clock.instant()
      store.startRun(cinema, keys, now, staleAfter) match {
        case None => 0 // a run is already active for this cinema
        case Some(runId) =>
          // Stagger the fan-out's eligibility evenly across the (clamped) spread
          // window so this venue's chunks don't all become claimable at once and
          // monopolise the pool — see `chunkSpread`. `Zero` window → no offset.
          val windowMillis = math.min(chunkSpread.toMillis, staleAfter.toMillis / 3)
          val total        = keys.size
          val n = keys.zipWithIndex.count { case (k, index) =>
            val notBefore =
              if (windowMillis > 0 && total > 1) Some(now.plusMillis(windowMillis * index / total))
              else None
            queue.enqueue(TaskType.ScrapeChunk,
              ChunkScrapeKeys.chunkDedup(cinema, runId, k),
              ChunkScrapeKeys.chunkPayload(cinema, runId, k),
              submittedAt = now, notBefore = notBefore) == EnqueueResult.Added
          }
          logger.info(s"$cinema chunked scrape run $runId: enqueued $n/${keys.size} chunk task(s)" +
            (if (windowMillis > 0 && total > 1) s", spread over ${windowMillis / 1000}s" else ""))
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
