package services.tasks

import play.api.Logging
import services.cinemas.common.{ChunkedCinemaScraper, CinemaMovieJson, CinemaScraper, PreScrapedCinemaScraper}

import java.time.Clock

/**
 * Handles a `ScrapeChunkReduce` task (the REDUCE): load every stored chunk slice
 * for the run, aggregate via the scraper's `reduceChunks`, and PUBLISH the full
 * listing through the same recording/fallback path a normal scrape uses
 * (`publishScrape` = `CinemaScrapeRunner.run` ∘ the uptime/Filmweb-fallback
 * decorator) — so uptime classification, the fallback, cache write-through and
 * `MovieDetailsComplete` events are all reused unchanged. Freshness is marked here
 * for the terminal SUCCESS; a plan that never gets this far (empty or failed) is
 * stamped by [[ChunkScrapePlanner]] against the same [[ScrapeFreshnessPolicy]], so
 * every chunked outcome advances the due schedule by one shared rule.
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
  publishScrape: CinemaScraper => Unit,
  // Shares the plain/planner path's rule (and failure streak) — see the class doc.
  scrapeFreshness: ScrapeFreshnessPolicy,
  clock:           Clock = Clock.systemUTC()
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ScrapeChunkReduce

  override def handle(task: Task): HandlerOutcome = {
    val cinema = task.payload.getOrElse(ChunkScrapeKeys.CinemaKey, "")
    val runId  = task.payload.getOrElse(ChunkScrapeKeys.RunIdKey, "")
    val run    = store.activeRun(cinema)
    if (!run.exists(_.runId == runId)) return Skipped // superseded/already reduced

    chunkScrapers.get(cinema) match {
      case None => store.completeRun(cinema, runId); Done
      case Some(scraper) =>
        val stored = store.loadChunks(cinema, runId)
          .map { case (k, json) => k -> CinemaMovieJson.decode(json, scraper.cinema) }
        val movies = scraper.reduceChunks(stored)
        // Work out whether this is the WHOLE listing BEFORE publishing, and tell the
        // cache. A partial reduce omits every film that only screens on a missing date,
        // and the cache's prune would read that omission as "stopped screening" — which
        // is how UK venues lost their advance-booking titles on 2026-07-27. This used to
        // be computed after the publish and only logged.
        val expected = run.get.expectedKeys.toSet
        val missing  = expected.diff(stored.keySet)
        try {
          publishScrape(new PreScrapedCinemaScraper(scraper.cinema, scraper.scrapeHosts, scraper.chain,
            () => movies, listingComplete = missing.isEmpty))
          scrapeFreshness.succeeded(ScrapeCinemaHandler.dedupKey(scraper.cinema))
          store.completeRun(cinema, runId)
          if (missing.nonEmpty)
            logger.warn(s"$cinema run $runId reduced PARTIAL: ${stored.size}/${expected.size} chunks (${missing.size} missing)")
          Done
        } catch {
          case e: Exception =>
            logger.warn(s"reduce/publish for $cinema run $runId failed, will retry: ${e.getMessage}")
            Reschedule(Some(e.getMessage))
        }
    }
  }
}
