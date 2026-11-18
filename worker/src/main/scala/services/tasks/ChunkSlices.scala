package services.tasks

import play.api.Logging
import services.cinemas.common.{ChunkedCinemaScraper, CinemaMovieJson, ListingReads}
import tools.ReadOutcome

/**
 * One chunk of a chunked cinema's listing, fetched and parsed into its encoded slice. A page-at-a-time
 * scraper's page is parsed only when it differs from the page this chunk last parsed, or when the parser
 * has changed since ([[ChunkPageMemo]]): most day pages are byte-identical from one scrape to the next (75
 * of 80 UK and US Flicks pages two hours apart, 2026-10-01), and parsing them was 7% of the US worker's CPU
 * (JFR, same day).
 */
final class ChunkSlices(
  pageMemo:    ChunkPageMemo = ChunkPageMemo.none,
  memoMetrics: ChunkPageMemoMetrics = ChunkPageMemoMetrics.noop
) extends Logging {

  /** The chunk `key` of `cinema`, read through `scraper`: its encoded slice, and whether every read in it
   *  was complete. `label` names the chunk in what is logged. */
  def read(cinema: String, key: String, scraper: ChunkedCinemaScraper, label: String): (String, Boolean) =
    try {
      val (slice, reads) = ListingReads.during(sliceOf(cinema, key, scraper))
      (slice, reads.complete)
    } catch {
      // The upstream says this chunk does not exist (a 404/410 on a key its own
      // plan advertised — Odeon's empty business dates, UK 2026-09-21/22). That is
      // an answer, not a failure: a retry only replays it, holding the run open
      // until the chunk exhausts. Land it empty so the run can complete.
      case e: Exception if ReadOutcome.isAbsent(e) =>
        logger.info(s"$label is gone upstream; storing it empty: ${e.getMessage}")
        (CinemaMovieJson.encode(Nil), true)
    }

  private def sliceOf(cinema: String, key: String, scraper: ChunkedCinemaScraper): String = scraper.pagedChunks match {
    case Some(paged) =>
      val page   = paged.fetchChunkPage(key)
      val digest = ChunkPageMemo.digest(page)
      val known  = pageMemo.recall(cinema, key)
      known match {
        case Some(entry) if entry.page == digest && entry.parser == paged.pageParser =>
          memoMetrics.recordPage(ChunkPageMemoMetrics.Hit)
          entry.slice
        case _ =>
          memoMetrics.recordPage(known.fold(ChunkPageMemoMetrics.New)(e =>
            if (e.page != digest) ChunkPageMemoMetrics.Changed else ChunkPageMemoMetrics.Parser))
          val slice = CinemaMovieJson.encode(paged.parseChunkPage(key, page))
          pageMemo.remember(cinema, key, ChunkPageMemo.Entry(digest, paged.pageParser, slice))
          slice
      }
    case None => CinemaMovieJson.encode(scraper.fetchChunk(key))
  }
}
