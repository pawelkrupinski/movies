package services.tasks

import java.time.Instant

import scala.concurrent.duration.*

import play.api.Logging
import services.cinemas.common.ChunkedCinemaScraper
import services.tasktypes.TaskType
import settings.ScrapeChunkSpread

/**
 * The fan-out of a started chunked run: one `ScrapeChunk` task per key, staggered across the spread
 * window, and what the run costs the queue recorded for the cost-spaced schedule.
 *
 * @param runTimeout  how long a run may go before it is abandoned ([[ChunkScrapePlanner.RunTimeout]])
 * @param chunkSpread stagger the run's fan-out evenly across this window (chunk k of n becomes claimable
 *                    at `+ chunkSpread·k/n`) instead of making all n claimable at once — so a big
 *                    full-horizon venue can't monopolise the pool and starve the evenly-enqueued ratings
 *                    (see [[services.tasks.ScrapeCadence.ChunkEnqueueSpread]]). Clamped to a third of
 *                    `runTimeout` so every chunk still becomes eligible AND drains before the run is
 *                    abandoned to a partial reduce. Default `Zero` disables the spread, leaving the flow
 *                    tests that drive `plan()` + `drain(now)` directly (and the deterministic fixture
 *                    harness) unaffected.
 * @param costs       where each planned run's fan-out is recorded, so the scrape schedule can space venues
 *                    by what they cost ([[CostSpacedPhaseOffset]]).
 */
final class ChunkFanOut(
  queue:       TaskProducer,
  runTimeout:  FiniteDuration,
  chunkSpread: ScrapeChunkSpread = ScrapeChunkSpread(Duration.Zero),
  costs:       ScrapeCostStore = ScrapeCostStore.Discarding
) extends Logging {

  /** Enqueue run `runId`'s chunks of `scraper`'s venue, keyed `cinema`; returns how many were added. */
  def enqueue(scraper: ChunkedCinemaScraper, cinema: String, runId: String, keys: Seq[String], now: Instant): Int = {
    // Stagger the fan-out's eligibility evenly across the (clamped) spread
    // window so this venue's chunks don't all become claimable at once and
    // monopolise the pool — see `chunkSpread`. `Zero` window → no offset.
    val windowMillis = math.min(chunkSpread.value.toMillis, runTimeout.toMillis / 3)
    val total        = keys.size
    val n = keys.zipWithIndex.count { case (k, index) =>
      val notBefore =
        if (windowMillis > 0 && total > 1) Some(now.plusMillis(windowMillis * index / total))
        else None
      queue.enqueue(TaskType.ScrapeChunk,
        ChunkScrapeKeys.chunkDedup(cinema, runId, k),
        ChunkScrapeKeys.chunkPayload(cinema, runId, k),
        // Submitted as of when it becomes claimable, not the planning instant: the
        // queue claims oldest `submittedAt` first, so a shared stamp let an older
        // run's stragglers — ripening for the whole spread — keep beating a newer
        // run's chunks claimable for minutes (head-of-line age pinned just under
        // the spread in every country, 2026-09-21..28).
        submittedAt = notBefore.getOrElse(now), notBefore = notBefore) == EnqueueResult.Added
    }
    logger.info(s"$cinema chunked scrape run $runId: enqueued $n/${keys.size} chunk task(s)" +
      (if (windowMillis > 0 && total > 1) s", spread over ${windowMillis / 1000}s" else ""))
    // The planner task + one per chunk + the reduce.
    costs.recordScrape(scraper.cinema, tasks = total + 2)
    n
  }
}
