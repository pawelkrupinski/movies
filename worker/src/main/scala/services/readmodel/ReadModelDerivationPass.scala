package services.readmodel

import tools.TickRun

import play.api.Logging

/** THE DERIVATION PASS: one content slice of the whole-corpus re-projection a derivation change
 *  owes, per tick.
 *
 *  A deploy that changes what the projection DERIVES from an unchanged row moves no row, so the
 *  change stream re-projects nothing and each stored card keeps the old derivation until the
 *  rolling content check reaches its slice — up to a day. On 2026-09-24 the poster selection
 *  changed that way and `ReadModelContentMismatch` held ~2.5 h, until a human forced a
 *  whole-corpus content check by hand. So the first complete prune sweep ([[ReadModelSweeper]])
 *  hands this pass a walk over the rows it saw ([[armIfUnchecked]]); the pass compares this code's
 *  derivation ([[ReadModelDerivation.current]]) with the one the store recorded and, when they
 *  differ, walks the same 48 content slices over every one of them — one slice per tick ([[advance]]), each
 *  under the lock only for its own rows, so the change stream keeps flowing and Mongo takes a
 *  slice's reads at a time: the whole corpus in 8 minutes, inside the content audit's
 *  15-minute re-check. Diff-based like every projection, so only cards that moved are written.
 *  When only card fields moved it reads each row slots-only and re-projects its cards alone.
 *  Each complete slice records the pass's progress, so a restart resumes rather than repeats.
 *  Only a pass that read and projected every row records the new version; one that did not
 *  leaves the old one, and the next sweep starts it again from the first slice that missed a
 *  row. Nothing happens when the store's version is this process's.
 *
 *  Built over the same projector `projection` as the sweeper by the composition root
 *  (`ReadModelWiring`); the sweeper schedules its slices.
 */
class ReadModelDerivationPass(
  projection: ReadModelSweeper.Projection,
  // Which derivation the stored read model was last re-projected whole under — see
  // [[ReadModelDerivationMarker]]. `none` owes no pass.
  derivationMarker:  ReadModelDerivationMarker = ReadModelDerivationMarker.none,
  // Every derivation this code knows, the last its own: what a store behind it is owed (see
  // [[ReadModelDerivation.owedSince]]). A parameter so a spec can stage a history.
  derivationHistory: Seq[Derivation] = ReadModelDerivation.History
) extends ContinuingSteps with Logging {
  import ReadModelDerivationPass.{Pass, TickSeconds}
  import ReadModelSweeper.{ContentSlices, SliceWalk}

  // Where the whole-corpus re-projection a derivation change owes stands.
  private var state: Pass = Pass.Unchecked

  /** Caller holds the projection lock. Unless the store's derivation has been read already (and found current, or
   *  a pass started), read it and arm the pass it owes over `walk` — the rows a complete sweep saw. */
  private[readmodel] def armIfUnchecked(walk: => SliceWalk): Unit =
    if (state == Pass.Unchecked) arm(walk)

  /** Caller holds the projection lock. Read which derivation the stored read model was last re-projected whole
   *  under and, when this code derives another, start the pass it owes over `walk` — the cards
   *  alone when every derivation since moved only cards ([[ReadModelDerivation.owedSince]]), and
   *  from the slice a previous process's pass towards the same version had reached. A read that
   *  fails decides nothing: the next sweep reads again. */
  private def arm(walk: SliceWalk): Unit =
    (for (recorded <- derivationMarker.recorded(); progress <- derivationMarker.progress()) yield (recorded, progress)) match {
      case scala.util.Failure(exception) =>
        logger.warn(s"read-model derivation pass: the recorded derivation could not be read (${exception.getMessage}) — " +
          "asking again next sweep.")
      case scala.util.Success((recorded, progress)) =>
        ReadModelDerivation.owedSince(recorded, derivationHistory) match {
          case None =>
            state = Pass.Current
            resetStaleProgress(progress)
          case Some(scope) => start(walk, recorded, progress, scope)
        }
    }

  /** Progress towards some other version is void once this store is current under this code: a pass
   *  for version B stopped at slice k, then a rollback to A (current), and A's change-stream
   *  projections rewrite cards in slices before k under A. Resumed at k on the roll forward, B's pass
   *  would never reach them — so the progress starts over. */
  private def resetStaleProgress(progress: Option[DerivationProgress]): Unit = {
    val reset = DerivationProgress(derivationHistory.last.version, 0)
    if (progress.exists(_ != reset))
      continuing("read-model derivation pass: resetting a stale pass's progress failed; a later pass may resume past rows it owes") {
        derivationMarker.recordProgress(reset)
      }
  }

  /** Caller holds the projection lock. Start the pass `scope` owes over `walk`, from the slice a previous
   *  process's pass towards the same version had reached. */
  private def start(walk: SliceWalk, recorded: Option[DerivationVersion],
                    progress: Option[DerivationProgress], scope: DerivationScope): Unit = {
    val current = derivationHistory.last.version
    val from    = progress.collect { case DerivationProgress(`current`, next) => next }.getOrElse(0)
    state = Pass.Running(walk, scope, nextSlice = from, written = 0, complete = true)
    logger.warn(s"read-model derivation pass: the stored read model was derived under " +
      s"${recorded.getOrElse("no recorded version")}, this worker derives $current — re-projecting " +
      s"${if (scope == DerivationScope.Cards) "the cards of" else "all"} ${walk.rows} row(s), one content slice of " +
      s"$ContentSlices every ${TickSeconds}s" +
      (if (from > 0) s", resuming at slice $from where an earlier pass stopped." else "."))
  }

  /** One slice of the pass, when one is running.
   *
   *  As a tick's run: [[TickRun.Incomplete]] when the slice missed a row, or its progress or the
   *  pass's derivation could not be recorded (each logged) — a pass owed and failing every slice
   *  must go stale. Nothing owed is a completed run. */
  def advance(): TickRun = projection.exclusively {
    state match {
      case running: Pass.Running =>
        val (written, complete) = running.walk.reproject(running.nextSlice, running.scope)
        val next = running.copy(nextSlice = running.nextSlice + 1, written = running.written + written,
                                complete = running.complete && complete)
        // Only while every slice so far was complete: a restart then resumes after the last one,
        // and a pass that missed a row starts over from the first slice it missed one in.
        val progressRecorded =
          if (next.complete && next.nextSlice < ContentSlices)
            continuing("read-model derivation pass: recording its progress failed; a restart repeats this slice") {
              derivationMarker.recordProgress(DerivationProgress(derivationHistory.last.version, next.nextSlice))
            }.isDefined
          else true
        state = if (next.nextSlice < ContentSlices) next else finish(next)
        TickRun.completedWhen(complete && progressRecorded && state != Pass.Unchecked)
      case _ => TickRun.Completed
    }
  }

  /** Caller holds the projection lock. A pass past its last slice: recorded when it read and projected every
   *  row, otherwise left unrecorded for the next sweep to run again. */
  private def finish(done: Pass.Running): Pass =
    if (!done.complete) {
      logger.warn(s"read-model derivation pass: re-projected ${done.walk.rows} row(s), rewrote ${done.written} " +
        "document(s), but some rows could not be read or projected — the derivation stays unrecorded and the " +
        "next sweep runs the pass again.")
      Pass.Unchecked
    } else
      continuing("read-model derivation pass: recording the derivation failed; the next sweep runs the pass again") {
        derivationMarker.record(derivationHistory.last.version)
        logger.info(s"read-model derivation pass: re-projected ${if (done.scope == DerivationScope.Cards) "the cards of " else ""}" +
          s"all ${done.walk.rows} row(s) under ${derivationHistory.last.version}, rewriting ${done.written} document(s); recorded.")
        Pass.Current
      }.getOrElse(Pass.Unchecked)
}

object ReadModelDerivationPass {
  /** Seconds between two slices of a derivation pass: 48 slices, the whole corpus in 8 minutes. */
  private[readmodel] val TickSeconds = 10L

  /** Where the whole-corpus re-projection a derivation change owes stands. */
  private sealed trait Pass
  private object Pass {
    /** The store's recorded derivation has not been read (yet, or again after a failed pass). */
    case object Unchecked extends Pass
    /** The store was derived under this process's version: nothing owed. */
    case object Current extends Pass
    /** Re-projecting `walk`'s rows (their cards alone, for a `Cards` scope), slice `nextSlice` next;
     *  `complete` while every row so far was read and projected. */
    final case class Running(walk: ReadModelSweeper.SliceWalk, scope: DerivationScope, nextSlice: Int, written: Int,
                             complete: Boolean) extends Pass
  }
}
