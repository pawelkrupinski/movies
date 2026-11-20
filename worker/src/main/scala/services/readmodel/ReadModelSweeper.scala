package services.readmodel

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

import play.api.Logging
import settings.{ReadModelPruneBootDelay, ReadModelPruneInterval}
import tools.{ScheduledTick, TickRecorder, TickRun}

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
 *    complete prune sweep arms it with a walk over the rows it saw ([[ReadModelDerivationPass.SliceWalk]]).
 *  - THE BOOT HEAL ([[ReadModelBootHeal]]): every ready row one of whose cards or venues is missing,
 *    projected before the first prune can delete the card it has under an old id.
 *
 * A sweep's pass itself is [[CorpusSweep]]'s; this class schedules the passes and judges a prune
 * sweep's heals ([[SweepVerdict]]).
 *
 * Each projects through the projector it sweeps ([[SweptProjection]]), under that
 * projector's lock, so a sweep and a stream apply never interleave. Built over a projector's
 * `projection` by the composition root (`ReadModelWiring`), which also schedules its paced passes
 * ([[schedule]]); the projector runs its boot heal ([[healMissingCards]]) between its boot reads.
 */
class ReadModelSweeper private[readmodel] (
  projection:      SweptProjection,
  // How often the id-only orphan prune runs, and how long after boot the first one waits
  // (`KINOWO_READMODEL_PRUNE_SECONDS` / `…_BOOT_DELAY_SECONDS`, resolved by the worker's root).
  pruneInterval:   ReadModelPruneInterval,
  pruneBootDelay:  ReadModelPruneBootDelay,
  // The derivation pass the first complete sweep arms, and `schedule` paces; `None` owes no pass.
  derivationPass:  Option[ReadModelDerivationPass],
  // The boot heal, run over the same projection before the first prune.
  bootHeal:        ReadModelBootHeal,
  // What the stream did while a prune sweep held the lock, and so which of its heals and rewrites were misses.
  verdict:         SweepVerdict,
  // One pass over the corpus: the scan, heals, prune, content check and catch-up ([[CorpusSweep]]).
  corpusSweep:     CorpusSweep
) extends Logging {

  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick). The expensive full re-projection is no longer
  // scheduled at all (the resume-token change stream made it redundant — see
  // [[ReadModelProjector]]); it survives only as the explicit `reconcile()` seed/backfill primitive.
  private[readmodel] val PruneSeconds = pruneInterval.value.toSeconds
  // Deferred off the boot path — running a full scan synchronously at `start()` stacked a
  // second scan onto the cache hydrate + first scrape on a cold JVM (the boot CPU drain).
  private[readmodel] val PruneBootDelaySeconds = pruneBootDelay.value.toSeconds

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
    val (_, complete, prunedUnlisted) = corpusSweep.sweep(reproject = true)
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
        val (healed, complete, prunedUnlisted) = corpusSweep.sweep(reproject = false)
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
  /** A sweeper over `projection` and the parts of its passes — the boot heal, the verdict on a sweep's heals, the
   *  corpus pass ([[CorpusSweep.apply]]) — as the worker's composition root and the specs build it. Every
   *  part logs as the sweeper: a sweep's lines read as the sweeper's, which they are. */
  def apply(
    projection:     SweptProjection,
    pruneInterval:  ReadModelPruneInterval  = DefaultPruneInterval,
    pruneBootDelay: ReadModelPruneBootDelay = DefaultPruneBootDelay,
    // Blocks until the change stream has applied what it had in flight when a sweep let go of the
    // lock — what tells a heal the stream would have made anyway from a real miss (see [[SweepVerdict]]).
    // The root passes the wait on its own `sleep`; the default serves the specs that build a sweeper bare.
    awaitStreamApplied: SweptProjection => Unit = StreamApplyWait.Blocking,
    derivationPass: Option[ReadModelDerivationPass] = None
  ): ReadModelSweeper = {
    val logger      = play.api.Logger(classOf[ReadModelSweeper])
    val verdict     = new SweepVerdict(projection, () => awaitStreamApplied(projection), logger)
    val corpusSweep = CorpusSweep(projection, derivationPass, pruneInterval.value.toSeconds, logger)
    new ReadModelSweeper(projection, pruneInterval, pruneBootDelay, derivationPass, new ReadModelBootHeal(projection), verdict, corpusSweep)
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
