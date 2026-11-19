package services.readmodel

import tools.ChangeStreamLiveness

import models.StoredMovieRecord

import tools.{ScheduledTick, TickRecorder, TickRun}

import play.api.Logging
import services.movies.{MovieChangeFeed, MovieRepositoryReader}
import settings.{ReadModelPruneBootDelay, ReadModelPruneInterval}

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

/**
 * The read model's whole-corpus passes, beside [[ReadModelProjector]]'s per-row projection of each
 * change-stream event: what repairs the read model when an event never arrives.
 *
 *  - THE PRUNE SWEEP (`pruneOrphans`, scheduled): an id-only scan that removes cards and venue rows
 *    no live row projects to, heals a ready row missing a card or a venue, rewrites one rolling
 *    content slice of the corpus, catches up the rows a silent change stream failed to deliver,
 *    and judges which of its heals and rewrites were real misses ([[SweepVerdict]]).
 *  - THE FULL RECONCILE (`reconcile`, never scheduled): re-project every row and prune — the
 *    one-shot seed/backfill primitive.
 *  - THE DERIVATION PASS ([[ReadModelDerivationPass]], paced): the whole-corpus re-projection a
 *    deploy owes when it changes what the projection derives from an unchanged row; the first
 *    complete prune sweep arms it with a walk over the rows it saw ([[ReadModelSweeper.SliceWalk]]).
 *  - THE BOOT HEAL ([[ReadModelBootHeal]]): every ready row one of whose cards or venues is missing,
 *    projected before the first prune can delete the card it has under an old id.
 *
 * A sweep's pass itself is [[PruneSweep]]'s; this class schedules the passes and judges a prune
 * sweep's heals ([[SweepVerdict]]).
 *
 * Each projects through the projector it sweeps ([[ReadModelSweeper.Projection]]), under that
 * projector's lock, so a sweep and a stream apply never interleave. Built over a projector's
 * `projection` by the composition root (`ReadModelWiring`), which also schedules its paced passes
 * ([[schedule]]); the projector runs its boot heal ([[healMissingCards]]) between its boot reads.
 */
class ReadModelSweeper(
  projection:      ReadModelSweeper.Projection,
  // How often the id-only orphan prune runs, and how long after boot the first one waits
  // (`KINOWO_READMODEL_PRUNE_SECONDS` / `…_BOOT_DELAY_SECONDS`, resolved by the worker's root).
  pruneInterval:   ReadModelPruneInterval  = ReadModelSweeper.DefaultPruneInterval,
  pruneBootDelay:  ReadModelPruneBootDelay = ReadModelSweeper.DefaultPruneBootDelay,
  // Blocks until the change stream has applied what it had in flight when a sweep let go of the
  // lock — what tells a heal the stream would have made anyway from a real miss (see [[SweepVerdict]]).
  // The root passes the wait on its own `sleep`; the default serves the specs that build a sweeper bare.
  awaitStreamApplied: ChangeStreamLiveness => Unit = StreamApplyWait.Blocking,
  // The derivation pass the first complete sweep arms, and `schedule` paces; `None` owes no pass.
  derivationPass: Option[ReadModelDerivationPass] = None
) extends Logging {
  // The boot heal, run over the same projection before the first prune.
  private val bootHeal        = new ReadModelBootHeal(projection)
  // What the stream did while a prune sweep held the lock, and so which of its heals and rewrites were misses.
  private val verdict         = new SweepVerdict(projection, () => awaitStreamApplied(projection.movieRepository.changeStreamLiveness), logger)

  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick). The expensive full re-projection is no longer
  // scheduled at all (the resume-token change stream made it redundant — see
  // [[ReadModelProjector]]); it survives only as the explicit `reconcile()` seed/backfill primitive.
  private[readmodel] val PruneSeconds = pruneInterval.value.toSeconds
  // Deferred off the boot path — running a full scan synchronously at `start()` stacked a
  // second scan onto the cache hydrate + first scrape on a cold JVM (the boot CPU drain).
  private[readmodel] val PruneBootDelaySeconds = pruneBootDelay.value.toSeconds

  // One pass over the corpus: the scan, heals, prune, content check and catch-up ([[PruneSweep]]).
  private val passes = new PruneSweep(projection, derivationPass, PruneSeconds, logger)
  /** Schedule the paced passes on `scheduler`: the orphan prune and the derivation pass's slices,
   *  each completed tick reported to `ticks` (`ScheduledTickStale`). The worker's root calls it once
   *  the projector's stream watch is up. */
  def schedule(scheduler: ScheduledExecutorService, ticks: TickRecorder): Unit = {
    val prune      = scala.concurrent.duration.Duration(PruneSeconds, TimeUnit.SECONDS)
    val bootDelay  = scala.concurrent.duration.Duration(PruneBootDelaySeconds, TimeUnit.SECONDS)
    ticks.started("read-model-prune", prune, bootDelay)
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU). Deferred
    // off the boot path so it doesn't compete with boot hydrate + the first scrape.
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-prune", prune, logger, ticks)(pruneOrphans()),
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    // The derivation pass's slices, paced: a no-op unless the first sweep found one owed.
    derivationPass.foreach { pass =>
      val derivation = scala.concurrent.duration.Duration(ReadModelDerivationPass.TickSeconds, TimeUnit.SECONDS)
      ticks.started("read-model-derivation-pass", derivation, bootDelay)
      scheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("read-model-derivation-pass", derivation, logger, ticks)(pass.advance()),
        PruneBootDelaySeconds, ReadModelDerivationPass.TickSeconds, TimeUnit.SECONDS)
    }
    logger.info(s"read-model sweeps scheduled: orphan-prune every ${PruneSeconds}s (first in ${PruneBootDelaySeconds}s).")
  }

  /** Full re-projection + prune. NOT scheduled — the resume-token change stream made the
   *  periodic reproject redundant. Kept as an explicit one-shot seed/backfill primitive:
   *  fixture/e2e read-model seeding calls it to project a settled corpus synchronously
   *  (it stitches split films via `foreachRecord`, which a per-row `onMovieUpsert` seed
   *  would not). Mirrors `scripts.BackfillReadModel`.
   *
   *  Returns whether the source scan was COMPLETE, every row it read projected and every orphan it
   *  found was pruned. An incomplete one (a page whose read failed) projected only the rows it could
   *  read whole, a row whose projection threw may stand half-written, and a prune that threw leaves
   *  an orphan served, so a caller seeding a read model it then asserts on owes another reconcile
   *  once Mongo recovers. */
  def reconcile(): Boolean = {
    val (_, complete, prunedUnlisted) = passes.sweep(reproject = true)
    verdict.reportUnlisted("reproject", prunedUnlisted, applied = _ => false)
    complete
  }

  /** Cheap id-only orphan prune — the frequent backstop for deleted / merged-away rows, and
   *  for the rows a silent change stream failed to deliver (see `sweep`). [[TickRun.Incomplete]] when the sweep's
   *  scan, a projection or the prune failed (logged by the sweep): the prune it skipped is not a completed run. */
  def pruneOrphans(): TickRun = {
    projection.exclusively(verdict.begin())
    val (healed, complete, rewritten, prunedUnlisted) =
      tools.AnyThrowable.rethrownAfter {
        val (healed, complete, prunedUnlisted) = passes.sweep(reproject = false)
        projection.exclusively((healed, complete, verdict.rewritten, prunedUnlisted))
      }(_ => projection.exclusively(verdict.abandon()))
    verdict.verdictOnSweep(healed, rewritten, prunedUnlisted)
    TickRun.completedWhen(complete)
  }

  /** Project, at boot, every ready row one of whose cards or venues is missing — [[ReadModelBootHeal]]'s; the
   *  projector runs it between its boot reads. */
  private[readmodel] def healMissingCards(study: Option[Seq[BootRow]]): Unit = bootHeal.healMissingCards(study)

  // Hear of every change the projector applies, from now on (the verdict's `noteApplied`, `noteDrift`).
  projection.exclusively(projection.reportTo(verdict))
}

object ReadModelSweeper {
  /** What a sweep asks of the projector it sweeps: its lock, its per-row projection, and the memo
   *  of what it last wrote — which a sweep reads and corrects, since the read model, not the memo,
   *  is the truth. Every method but the stores, `exclusively` and `partition` expects the lock held. */
  trait Projection {
    /** The source store the projection reads. */
    def movieRepository: MovieRepositoryReader & MovieChangeFeed
    /** The read model it reads back (it writes only through the methods below). */
    def reader: ReadModelReader
    def metrics: ReadModelProjectionMetrics
    /** The clock it keeps time by — the rolling content check numbers its slice by it. */
    def clock: java.time.Clock
    /** Caller holds the lock. Tell `listener`, from now on, of each change the projector applies. */
    private[readmodel] def reportTo(listener: Listener): Unit
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
    def heal(id: models.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean]
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
  private[readmodel] def contentSliceOf(id: models.FilmId): Int =
    math.floorMod(id.value.##.toLong, ContentSlices.toLong).toInt

  /** The rows a complete prune sweep saw, walked one content slice at a time: what a derivation pass
   *  ([[ReadModelDerivationPass]]) re-projects, without knowing the rows or how they are read. */
  private[readmodel] trait SliceWalk {
    /** How many rows the walk covers. */
    def rows: Int
    /** Caller holds the projection lock. Re-project the rows in content slice `slice` — their cards alone for a
     *  `Cards` scope: the documents written, and whether every row was read and projected. */
    def reproject(slice: Int, scope: DerivationScope): (Int, Boolean)
  }

  /** The id-only orphan prune's compiled-in cadence: every 30 minutes, first 5 minutes after boot. */
  val DefaultPruneInterval: ReadModelPruneInterval =
    ReadModelPruneInterval(scala.concurrent.duration.Duration(1800L, TimeUnit.SECONDS))
  val DefaultPruneBootDelay: ReadModelPruneBootDelay =
    ReadModelPruneBootDelay(scala.concurrent.duration.Duration(300L, TimeUnit.SECONDS))

  /** The most unlisted venue rows one sweep prunes: 20, or 1% of the read model's venue rows when
   *  that is more. A stream's missed removals come a few at a time (one on 2026-09-30); past this,
   *  the comparison is the likelier fault, and the sweep prunes none. */
  private[readmodel] def unlistedPruneCap(screeningRows: Int): Int = math.max(20, screeningRows / 100)

  /** How many row ids one heal line names before it summarises the rest. */
  private[readmodel] val LoggedIdsPerLine = 20

  /** The healed row ids for a log line — every heal is otherwise a count nobody can trace
   *  back to the retirement that caused it — capped so a mass heal stays one line. */
  private[readmodel] def idsForLog(ids: collection.Seq[String]): String =
    ids.take(LoggedIdsPerLine).mkString(", ") +
      (if (ids.sizeIs > LoggedIdsPerLine) s" (+${ids.size - LoggedIdsPerLine} more)" else "")
}
