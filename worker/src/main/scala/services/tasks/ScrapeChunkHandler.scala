package services.tasks

import play.api.Logging
import services.cinemas.{ChunkedCinemaScraper, CinemaMovieJson}

import java.time.Clock

/**
 * Handles a `ScrapeChunk` task (the MAP): fetch + parse one chunk of a chunked
 * cinema's listing and store its slice under `(cinema, runId, key)`.
 *
 *  - The task is dropped (`Skipped`) when its `runId` is no longer the cinema's
 *    active run — i.e. a superseding re-scrape started — so stale chunks never
 *    feed a published listing.
 *  - A fetch failure `Reschedule`s just this chunk (the queue's exponential
 *    backoff = the per-chunk retry); the run completes via the coordinator only
 *    once every chunk has stored, or via the backstop's partial reduce on timeout.
 */
class ScrapeChunkHandler(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  clock:         Clock = Clock.systemUTC()
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
        try {
          val slice = scraper.fetchChunk(key)
          store.storeChunk(cinema, runId, key, CinemaMovieJson.encode(slice), clock.instant())
          Done
        } catch {
          case e: Exception =>
            logger.warn(s"chunk '$key' for $cinema run $runId failed: ${e.getMessage}")
            Reschedule(Some(e.getMessage))
        }
    }
  }
}
