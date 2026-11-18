package services.tasks

import services.tasktypes.TaskType

import play.api.Logging
import services.cinemas.common.ChunkedCinemaScraper

/**
 * Handles a `ScrapeChunkReduce` task (the REDUCE): load every stored chunk slice
 * for the run, aggregate via the scraper's `reduceChunks` ([[ReducedRun]]), and
 * PUBLISH the listing ([[ChunkedListingPublisher]]) through the same recording/fallback
 * path a normal scrape uses. Freshness is marked there for the terminal SUCCESS; a plan
 * that never gets this far (empty or failed) is stamped by [[ChunkScrapePlanner]] against
 * the same [[ScrapeFreshnessPolicy]], so every chunked outcome advances the due schedule
 * by one shared rule.
 *
 *  - `Skipped` when the run is no longer active (superseded), so an overlapping
 *    run can't double-publish.
 *  - A publish failure `Reschedule`s; the stored chunks are left in place so the
 *    retry re-reads them. Only on success are the chunks dropped (`completeRun`).
 *  - Reduces whatever chunks have landed — the backstop enqueues this for an
 *    abandoned run too, so one dead chunk degrades to a partial listing rather
 *    than losing the whole cinema.
 */
class ScrapeChunkReduceHandler(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  // Shares the plain/planner path's rule (and failure streak) — see the class doc.
  publisher:     ChunkedListingPublisher
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeChunkReduce

  override def handle(task: Task): HandlerOutcome = {
    val cinema = task.payload.getOrElse(ChunkScrapeKeys.CinemaKey, "")
    val runId  = task.payload.getOrElse(ChunkScrapeKeys.RunIdKey, "")
    store.activeRun(cinema).filter(_.runId == runId) match {
      case None => Skipped // superseded/already reduced
      case Some(run) => chunkScrapers.get(cinema).fold[HandlerOutcome] { store.completeRun(cinema, runId); Done }(reduce(run, _))
    }
  }

  private def reduce(run: ChunkRun, scraper: ChunkedCinemaScraper): HandlerOutcome = {
    // Work out whether this is the WHOLE listing BEFORE publishing, and tell the
    // cache. A partial reduce omits every film that only screens on a missing date,
    // and the cache's prune would read that omission as "stopped screening" — which
    // is how UK venues lost their advance-booking titles on 2026-07-27. This used to
    // be computed after the publish and only logged.
    val reduced = ReducedRun.of(store, run, scraper)
    try {
      publisher.publishReduced(scraper, reduced.movies, reduced.whole)
      store.completeRun(run.cinema, run.runId)
      if (!reduced.whole)
        logger.warn(s"${run.cinema} run ${run.runId} reduced PARTIAL: ${reduced.storedChunks}/${reduced.expectedChunks} chunks " +
          s"(${reduced.missingChunks} missing), ${reduced.incompleteReads} read(s) incomplete")
      Done
    } catch {
      case e: Exception =>
        logger.warn(s"reduce/publish for ${run.cinema} run ${run.runId} failed, will retry: ${e.getMessage}")
        Reschedule(Some(tools.FailureText.of(e)))
    }
  }
}
