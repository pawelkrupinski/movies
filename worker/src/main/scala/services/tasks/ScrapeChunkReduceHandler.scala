package services.tasks

import play.api.Logging
import services.cinemas.{ChunkedCinemaScraper, CinemaMovieJson, CinemaScraper, PreScrapedCinemaScraper}
import services.freshness.{FreshnessKind, FreshnessStore}

import java.time.Clock

/**
 * Handles a `ScrapeChunkReduce` task (the REDUCE): load every stored chunk slice
 * for the run, aggregate via the scraper's `reduceChunks`, and PUBLISH the full
 * listing through the same recording/fallback path a normal scrape uses
 * (`publishScrape` = `CinemaScrapeRunner.run` ∘ the uptime/Filmweb-fallback
 * decorator) — so uptime classification, the fallback, cache write-through and
 * `MovieDetailsComplete` events are all reused unchanged. Freshness is marked
 * only here, on the terminal success.
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
  freshness:     FreshnessStore,
  clock:         Clock = Clock.systemUTC()
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
        try {
          publishScrape(new PreScrapedCinemaScraper(scraper.cinema, scraper.scrapeHosts, scraper.chain, () => movies))
          freshness.markFresh(ScrapeCinemaHandler.dedupKey(scraper.cinema), FreshnessKind.CinemaScrape)
          store.completeRun(cinema, runId)
          val expected = run.get.expectedKeys.toSet
          val missing  = expected.diff(stored.keySet)
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
