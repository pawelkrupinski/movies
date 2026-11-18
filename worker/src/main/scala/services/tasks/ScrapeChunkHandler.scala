package services.tasks

import services.tasktypes.TaskType

import play.api.Logging
import services.cinemas.common.ChunkedCinemaScraper

import java.time.Clock

/**
 * Handles a `ScrapeChunk` task (the MAP): fetch + parse one chunk of a chunked
 * cinema's listing and store its slice under `(cinema, runId, key)`.
 *
 *  - The task is dropped (`Skipped`) when its `runId` is no longer the cinema's
 *    active run — i.e. a superseding re-scrape started — so stale chunks never
 *    feed a published listing.
 *  - An upstream "not found" (404/410) stores the chunk EMPTY: it is the answer,
 *    and retrying it only holds the run open until the chunk exhausts.
 *  - Any other fetch failure — or a store that failed to land the chunk — `Reschedule`s just this chunk (the queue's exponential
 *    backoff = the per-chunk retry); the run completes via the coordinator only
 *    once every chunk has stored, or via the backstop's partial reduce on timeout.
 *  - A fetch the host's circuit breaker refused outright never happened, so it
 *    `Deferred`s instead: same return-to-waiting, but the attempt is refunded and
 *    the wait is the breaker's own remaining block rather than a doubling backoff.
 */
class ScrapeChunkHandler(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  slices:        ChunkSlices,
  deferrals:     HostDownDeferrals,
  clock:         Clock
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeChunk

  override def handle(task: Task): HandlerOutcome = {
    val cinema = task.payload.getOrElse(ChunkScrapeKeys.CinemaKey, "")
    val runId  = task.payload.getOrElse(ChunkScrapeKeys.RunIdKey, "")
    val key    = task.payload.getOrElse(ChunkScrapeKeys.ChunkKey, "")
    if (!store.activeRun(cinema).exists(_.runId == runId)) return Skipped // superseded/stale run

    chunkScrapers.get(cinema) match {
      case None => Done // cinema dropped from the catalogue
      case Some(scraper) =>
        val label = s"chunk '$key' for $cinema run $runId"
        try {
          val (slice, complete) = slices.read(cinema, key, scraper, label)
          if (!complete) logger.info(s"$label stored INCOMPLETE: a page failed")
          store.storeChunk(cinema, runId, key, StoredChunk(slice, complete, task.attempts), clock.instant())
          Done
        } catch {
          // The host's breaker is open, so this chunk never reached the wire ([[HostDownDeferrals]]);
          // otherwise a failed fetch, or a store that failed to land the slice: retry the chunk.
          case e: Exception => deferrals.deferral(e, label).getOrElse {
            logger.warn(s"$label failed: ${tools.FailureText.of(e)}")
            Reschedule(Some(tools.FailureText.of(e)))
          }
        }
    }
  }
}

/** How each page-at-a-time chunk went: its last parse reused (`hit`), or parsed because the page
 *  changed, the memo had none for it, or the parser changed since. */
trait ChunkPageMemoMetrics { def recordPage(outcome: String): Unit }

object ChunkPageMemoMetrics {
  val Hit = "hit"; val Changed = "changed"; val New = "new"; val Parser = "parser"
  val Outcomes: Seq[String] = Seq(Hit, Changed, New, Parser)
  val noop: ChunkPageMemoMetrics = (_: String) => ()
}
