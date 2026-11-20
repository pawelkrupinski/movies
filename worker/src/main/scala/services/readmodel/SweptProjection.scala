package services.readmodel

import models.{FilmId, StoredMovieRecord}
import services.movies.{MovieChangeFeed, MovieRepositoryReader}

/** What a sweep ([[ReadModelSweeper]], its [[CorpusSweep]] and their parts) asks of the projector it sweeps: its lock,
 *  its source rows, its per-row projection, and the memo of what it last wrote — which a sweep reads and corrects,
 *  since the read model, not the memo, is the truth. Every method but the stores, `exclusively` and `partition`
 *  expects the lock held. A [[ReadModelProjector]]'s `projection`. */
trait SweptProjection {
  /** The source store the projection reads. */
  def movieRepository: MovieRepositoryReader & MovieChangeFeed
  /** Every source row, read whole (`withShowtimes`) or slots-only — the `screenings` collection unread. */
  def scan(withShowtimes: Boolean)(row: StoredMovieRecord => Unit): tools.ScanOutcome
  /** One source row by id, read whole (`withShowtimes`) or slots-only. */
  def read(id: FilmId, withShowtimes: Boolean): tools.ReadOutcome[StoredMovieRecord]
  /** The read model it reads back (it writes only through the methods below). */
  def reader: ReadModelReader
  def metrics: ReadModelProjectionMetrics
  /** The clock it keeps time by — the rolling content check numbers its slice by it. */
  def clock: java.time.Clock
  /** Caller holds the lock. Tell `listener`, from now on, of each change the projector applies. */
  private[readmodel] def reportTo(listener: SweptProjection.Listener): Unit
  /** Run `body` under the projection lock: every change-stream apply waits for it. */
  def exclusively[A](body: => A): A
  /** The row split into its cards' display-title groups, under the repository's title rules. */
  def partition(row: StoredMovieRecord): ReadModelProjection.Partition
  /** Project a ready row's partition and write what changed: the documents written. */
  def project(partition: ReadModelProjection.Partition, trigger: ReadModelProjectionMetrics.ProjectTrigger): Int
  /** [[project]] a row read whole. */
  def projectRow(row: StoredMovieRecord, trigger: ReadModelProjectionMetrics.ProjectTrigger): Int
  /** Re-project only the cards of a row read without its showtimes: the cards rewritten. */
  def projectCards(partition: ReadModelProjection.Partition): Int
  /** Re-project a row some of whose cards or venues are missing: whether it repaired one, `None`
   *  when nothing was projected. */
  def heal(id: FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean]
  /** The venues a heal found to be phantoms of this row, while its metadata is what it was. */
  def phantomsOf(rowId: String, metadataHash: Int): Set[String]
  /** The card ids a row projects to, remembered when the projector has none for it yet. */
  def noteRowCards(rowId: String, cards: Set[String]): Unit
  /** The card ids a row projects to, remembered over whatever the projector had. */
  def recordRowCards(rowId: String, cards: Set[String]): Unit
  /** Forget every source row but the live ones — after a COMPLETE scan only. */
  def retainRows(live: String => Boolean): Unit
  /** Remove a card and its remembered screenings from the read model; `audit` names the path. */
  def removeCard(filmId: String, audit: String): Unit
  /** Delete a screenings row a sweep found orphaned, and forget it. */
  def removeScreening(ref: ScreeningRef): Unit
}

object SweptProjection {
  /** What the projector tells the sweeper over it, under its lock: the rows the change stream applied
   *  or deleted, and the content check's rewrites. */
  private[readmodel] trait Listener {
    def noteApplied(rowId: String): Unit
    def noteDrift(row: RowDrift): Unit
  }
  private[readmodel] object Listener {
    /** No sweeper over the projector: nothing to tell. */
    val none: Listener = new Listener {
      def noteApplied(rowId: String): Unit = ()
      def noteDrift(row: RowDrift): Unit   = ()
    }
  }

  /** How many prune sweeps it takes to re-project the whole corpus once — the ROLLING
   *  CONTENT CHECK. Every backstop before it compared IDS: the prune removes a card whose
   *  row is gone, the heal writes a card or a venue that is missing. None of them can see a
   *  row that EXISTS and is WRONG, and on 2026-09-08 three UK films had held the wrong
   *  showtimes since 2026-08-29 — Troy and 2046 at the Prince Charles, Glastonbury at the
   *  Southsea — served to real users, invisible to every sweep, and unrepairable by the
   *  change stream because the source had long since stopped changing. A projection is
   *  diff-based, so re-projecting a slice costs a read per row and writes only what actually
   *  drifted; at 48 slices on a 30-minute sweep the whole corpus is verified once a day.
   *
   *  A constant, not a knob: its one override was 1, to push a derivation change out at once
   *  (2026-09-24), and the derivation pass now does that on its own. */
  private[readmodel] val ContentSlices = 48

  /** The content slice a row falls in: by its id, so the slices partition the corpus. */
  private[readmodel] def contentSliceOf(id: FilmId): Int =
    math.floorMod(id.value.##.toLong, ContentSlices.toLong).toInt
}
