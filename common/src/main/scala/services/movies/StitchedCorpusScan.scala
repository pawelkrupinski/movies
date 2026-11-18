package services.movies

import models.{Showtime, SourceData}
import org.bson.conversions.Bson
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** THE STITCHED CORPUS SCAN of [[MongoMovieRepository]]: `movies` read a keyset page at a time ([[MoviesKeysetPages]]),
 *  each page's side rows read for exactly the films it holds, and the page handed on whole ([[MovieSideStitch]]) —
 *  or not at all, when a side read failed. Every corpus read of the repository (`findAll`, `foreachPage`, the
 *  `foreachRecord` family) goes through here, so no two of them can disagree about a film's cinemas or showtimes.
 *
 *  Built by the repository over its own paging, stitch, title rules and stopwatch, and logging under the
 *  repository's `logger`: a scan's timing and refusals read as the repository's, which they are. */
private[movies] final class StitchedCorpusScan(pages: MoviesKeysetPages, stitch: MovieSideStitch, normalizer: services.titles.TitleNormalizer,
                                               stopwatch: tools.Stopwatch, logger: play.api.Logger)(using LatestTitleYear) {

  /** The ONE stitched corpus scan — keyset-paged movies + showtimes re-injected from
   *  `screenings` — shared by [[findAll]] and [[foreachRecord]] so the two can never
   *  disagree on a film's showtimes (the divergence that dropped 129 films: a reader
   *  that forgot to stitch made the reconcile prune live `web_screenings`). Reads each
   *  page's side rows, then hands the page, stitched, to `onBatch`.
   *
   *  Projection-safety: a page whose side read FAILED is never handed on — its rows,
   *  stitched from the empty answer, would read as films with no cinemas or no showtimes,
   *  and a projecting caller writes exactly that. The page is skipped and the scan reports
   *  "incomplete", exactly like a failed movies batch, so a pruning caller skips
   *  its destructive step too.
   *
   *  Side rows are fetched PER PAGE, for exactly the films that page holds, rather than preloaded
   *  whole. Both are one indexed `filmId $in [...]` query. `foreachRecord` promises its callers it
   *  never holds more than a page, and that stopped being true when the bulk of a row moved into the
   *  side collections: this preloaded both entire collections first (7.5 MB each on prod PL, more on
   *  UK) and held them for the whole scan, on a worker with a 320 MB heap and an OOM history. Now the
   *  peak is one page's films, whatever the corpus grows to. Completeness still gates the whole scan,
   *  not the page: a caller that PRUNES on a row's absence must not act on a partial view, so one
   *  failed side read fails the scan exactly as a failed `movies` batch does. */
  def scanStitched(collection: Option[MongoCollection[StoredMovieDto]], onBatch: Seq[StoredMovieRecord] => Unit,
                   withShowtimes: Boolean = true, filter: Bson = Filters.empty()): tools.ScanOutcome = {
    var sideReadsComplete = true
    val timing = new ScanTiming
    var films  = 0
    // One pool for the scan: equal instants, rooms and formats across its pages are one object ([[ShowtimePool]]).
    val shared = new ShowtimePool
    val moviesComplete = scanByKeyset(collection, filter) { batch =>
      val ids = batch.map(_._id).toSet
      val (pageScr, pageSlots) = readPageSides(ids, withShowtimes, timing)
      // A page whose side read failed is NOT handed on, stitched from the empty answer: its
      // films would arrive with no cinemas or no showtimes, and "incomplete" only stops a
      // prune — `ReadModelSweeper.reconcile` and the sweep's catch-up PROJECT every row they
      // are given, so each film on the page lost every served showtime (the per-row
      // `MovieSideStitch.decode` declines for the same reason). The page is skipped and the scan
      // reported incomplete; the rows it could stitch are still delivered.
      (pageScr, pageSlots) match {
        case (Some(showtimes), Some(slotRows)) =>
          films += batch.size
          onBatch(timing.stitching(batch.map(dto => stitch.row(StoredMovieDto.toDomain(dto, normalizer),
            showtimes.getOrElse(dto._id, Map.empty).map { case (slot, read) => slot -> shared.showtimes(read) },
            slotRows.getOrElse(dto._id, Map.empty)))))
        case _ =>
          sideReadsComplete = false
          logger.warn(s"MovieRepository.scanStitched: a side-collection read failed for a page of " +
            s"${ids.size} film(s) (screenings ok=${pageScr.isDefined}, slots ok=${pageSlots.isDefined}) — skipping the page and treating the " +
            "scan as incomplete, so no caller acts on films whose cinemas or showtimes it could not see.")
      }
    }
    timing.report(films, wholeCorpus = filter.toBsonDocument.isEmpty)
    moviesComplete.andThen(tools.ScanOutcome.of(sideReadsComplete, "a side-collection read failed for a page of films"))
  }

  /** One page's side rows — its showtimes (empty when `withShowtimes` is off) and its slots — each
   *  None when its read failed. `withShowtimes = false` skips the showtimes read entirely rather
   *  than discarding its result: the caller has said it never looks at a showtime, and this is the
   *  expensive half. The two reads are independent (each is `filmId $in` the page's ids), so they
   *  run at once: in sequence they were most of a US boot's 16–21 s cache hydrate, one round-trip
   *  after the other for every page. */
  private def readPageSides(ids: Set[String], withShowtimes: Boolean, timing: ScanTiming)
      : (Option[Map[String, Map[String, Seq[Showtime]]]], Option[Map[String, Map[String, SourceData]]]) = {
    val slotsRead = scala.concurrent.Future(scala.concurrent.blocking(timing.slotReads(stitch.slotRowsOf(ids))))(using scala.concurrent.ExecutionContext.global)
    val pageScr   = if (!withShowtimes) Some(Map.empty[String, Map[String, Seq[Showtime]]]) else timing.screeningReads(stitch.screeningRowsOf(ids))
    (pageScr, Await.result(slotsRead, Duration.Inf))
  }

  /** Where a scan's time goes, logged once per scan (the boot hydrate, the rare backstop): the
   *  restart's critical path runs through here, and a guess at its bottleneck already shipped once
   *  without moving it. */
  private final class ScanTiming {
    private val wall   = stopwatch.start()
    val slotReads      = stopwatch.total()
    val screeningReads = stopwatch.total()
    val stitching      = stopwatch.total()

    /** A whole-corpus scan (the hydrate, the backstop rehydrate) at info; a catch-up's
     *  updated-since slice, which runs on every read-model sweep, only at debug. */
    def report(films: Int, wholeCorpus: Boolean): Unit = {
      val line = s"MovieRepository.scanStitched: $films film(s) in ${wall.millis}ms — slot reads " +
        s"${slotReads.elapsed.toMillis}ms, screening reads ${screeningReads.elapsed.toMillis}ms (alongside), " +
        s"stitch ${stitching.elapsed.toMillis}ms over ${stitching.count} page(s)."
      if (wholeCorpus) logger.info(line) else logger.debug(line)
    }
  }

  /** The `movies` documents matching `filter`, keyset-paged by `_id` ([[MoviesKeysetPages]]). */
  def scanByKeyset(collection: Option[MongoCollection[StoredMovieDto]], filter: Bson)(onBatch: Seq[StoredMovieDto] => Unit): tools.ScanOutcome =
    pages.documents(collection, filter)(onBatch)
}
