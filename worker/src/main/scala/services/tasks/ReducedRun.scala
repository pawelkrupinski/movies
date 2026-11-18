package services.tasks

import models.CinemaMovie
import services.cinemas.common.{ChunkedCinemaScraper, CinemaMovieJson}

/** A chunked run's stored chunks reduced to the venue's listing, and whether that is the WHOLE listing:
 *  every expected chunk stored, and no read inside the plan (`markers`) or a chunk incomplete. */
final case class ReducedRun(movies: Seq[CinemaMovie], storedChunks: Int, expectedChunks: Int, missingChunks: Int, incompleteReads: Int) {
  def whole: Boolean = missingChunks == 0 && incompleteReads == 0
}

object ReducedRun {

  /** Load `run`'s stored chunks and reduce them through `scraper`: whatever chunks have landed — the
   *  backstop reduces an abandoned run too, so one dead chunk degrades to a partial listing rather than
   *  losing the whole cinema. */
  def of(store: ChunkScrapeStore, run: ChunkRun, scraper: ChunkedCinemaScraper): ReducedRun = {
    val (markers, slices) = store.loadChunks(run.cinema, run.runId).partition { case (k, _) => ChunkScrapeKeys.isIncompleteMarker(k) }
    val partialSlices = slices.count { case (_, chunk) => !chunk.complete }
    val stored = slices.map { case (k, chunk) => k -> CinemaMovieJson.decode(chunk.valueJson, scraper.cinema) }
    val expected = run.expectedKeys.toSet
    ReducedRun(scraper.reduceChunks(stored), stored.size, expected.size, expected.diff(stored.keySet).size, markers.size + partialSlices)
  }
}
