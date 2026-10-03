package services.tasks

import play.api.Logging
import services.cinemas.common.{ChunkedCinemaScraper, CinemaMovieJson, PagedChunkScraper}
import tools.{CircuitOpenException, EnrichmentRead}

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
 *  - Any other fetch failure `Reschedule`s just this chunk (the queue's exponential
 *    backoff = the per-chunk retry); the run completes via the coordinator only
 *    once every chunk has stored, or via the backstop's partial reduce on timeout.
 *  - A fetch the host's circuit breaker refused outright never happened, so it
 *    `Deferred`s instead: same return-to-waiting, but the attempt is refunded and
 *    the wait is the breaker's own remaining block rather than a doubling backoff.
 */
class ScrapeChunkHandler(
  chunkScrapers: Map[String, ChunkedCinemaScraper],
  store:         ChunkScrapeStore,
  clock:         Clock = Clock.systemUTC(),
  // A chunk page's last parse, so an identical page is not parsed again — see `sliceOf`.
  pageMemo:      ChunkPageMemo = ChunkPageMemo.none,
  memoMetrics:   ChunkPageMemoMetrics = ChunkPageMemoMetrics.noop
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
          store.storeChunk(cinema, runId, key, sliceOf(cinema, key, scraper), clock.instant())
          Done
        } catch {
          // The host's breaker is open, so this chunk never reached the wire. Give
          // the attempt back and wait out the block instead of charging a doubling
          // backoff for work that was refused locally — otherwise a host-wide block
          // retries the whole estate out of existence while the host is still down
          // (see HandlerOutcome.Deferred for the 2026-07-28 Odeon numbers).
          case e: CircuitOpenException =>
            logger.info(s"chunk '$key' for $cinema run $runId deferred: ${e.getMessage}")
            Deferred(Some(e.getMessage), Some(clock.instant().plusMillis(e.openForMs)))
          // The upstream says this chunk does not exist (a 404/410 on a key its own
          // plan advertised — Odeon's empty business dates, UK 2026-09-21/22). That is
          // an answer, not a failure: a retry only replays it, holding the run open
          // until the chunk exhausts. Land it empty so the run can complete.
          case e: Exception if EnrichmentRead.isAbsent(e) =>
            logger.info(s"chunk '$key' for $cinema run $runId is gone upstream; storing it empty: ${e.getMessage}")
            store.storeChunk(cinema, runId, key, CinemaMovieJson.encode(Nil), clock.instant())
            Done
          case e: Exception =>
            logger.warn(s"chunk '$key' for $cinema run $runId failed: ${e.getMessage}")
            Reschedule(Some(e.getMessage))
        }
    }
  }

  /** The chunk's slice, encoded. A page-at-a-time scraper's page is parsed only when it differs
   *  from the page this chunk last parsed, or when the parser has changed since: most day pages are
   *  byte-identical from one scrape to the next (75 of 80 UK and US Flicks pages two hours apart,
   *  2026-10-01), and parsing them was 7% of the US worker's CPU (JFR, same day). */
  private def sliceOf(cinema: String, key: String, scraper: ChunkedCinemaScraper): String = scraper match {
    case paged: PagedChunkScraper =>
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
    case other => CinemaMovieJson.encode(other.fetchChunk(key))
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
