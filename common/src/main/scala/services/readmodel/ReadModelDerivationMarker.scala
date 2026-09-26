package services.readmodel

import scala.util.{Success, Try}

/** How far the pass owed to `version` got: every content slice before `nextSlice` was read and
 *  projected under it. */
final case class DerivationProgress(version: DerivationVersion, nextSlice: Int)

/**
 * Which derivation ([[ReadModelDerivation]]) the stored read model was last re-projected WHOLE
 * under — the fact that tells a booting worker whether the cards in `web_movies` /
 * `web_screenings` were derived by the code it is running — and how far a pass towards the next
 * one has got.
 *
 * A stored card only moves when its row is projected again, and a deploy that changes what the
 * projection DERIVES from an unchanged row (2026-09-24: the poster selection) moves no row. The
 * rolling content check would reach every card within a day, and until it did the content audit
 * paged; so the projector compares this marker with its own version and, when they differ,
 * re-projects the corpus once and records the new one (see `ReadModelProjector`).
 *
 * The PROGRESS is what lets a pass survive a restart. A pass takes ~8 minutes and starts ~5
 * after boot, so a deploy every ten minutes restarted every one from its first slice: on
 * 2026-09-26 one version's pass started three times on every worker and finished once.
 *
 * Reads answer a FAILED read as a `Failure`, never as "nothing recorded": the second starts a
 * whole-corpus pass, and a flaky read must not start one on every sweep.
 */
trait ReadModelDerivationMarker {
  /** The version last recorded; `Success(None)` when none ever was (a fresh store, or a store
   *  from before the marker existed — both owe a pass). */
  def recorded(): Try[Option[DerivationVersion]]

  /** Record that every row has now been projected under `version`. Throws when the write fails. */
  def record(version: DerivationVersion): Unit

  /** The progress last recorded by a pass, whichever version it was for. */
  def progress(): Try[Option[DerivationProgress]]

  /** Record a pass's progress. Throws when the write fails. */
  def recordProgress(progress: DerivationProgress): Unit
}

object ReadModelDerivationMarker {
  /** For a wiring with no store to keep the marker in (a spec, a Mongo-less boot): reports the
   *  running version as already recorded, so no pass is ever owed. */
  val none: ReadModelDerivationMarker = new ReadModelDerivationMarker {
    def recorded(): Try[Option[DerivationVersion]]     = Success(Some(ReadModelDerivation.current))
    def record(version: DerivationVersion): Unit       = ()
    def progress(): Try[Option[DerivationProgress]]    = Success(None)
    def recordProgress(progress: DerivationProgress): Unit = ()
  }
}
