package services.readmodel

import scala.util.{Success, Try}

/**
 * Which [[ReadModelProjection.DerivationVersion]] the stored read model was last re-projected
 * WHOLE under — the fact that tells a booting worker whether the cards in `web_movies` /
 * `web_screenings` were derived by the code it is running.
 *
 * A stored card only moves when its row is projected again, and a deploy that changes what the
 * projection DERIVES from an unchanged row (2026-09-24: the poster selection) moves no row. The
 * rolling content check would reach every card within a day, and until it did the content audit
 * paged; so the projector compares this marker with its own version and, when they differ,
 * re-projects the whole corpus once and records the new one (see `ReadModelProjector`).
 *
 * `recorded` answers a FAILED read as a `Failure`, never as "nothing recorded": the second
 * starts a whole-corpus pass, and a flaky read must not start one on every sweep.
 */
trait ReadModelDerivationMarker {
  /** The version last recorded; `Success(None)` when none ever was (a fresh store, or a store
   *  from before the marker existed — both owe a pass). */
  def recorded(): Try[Option[String]]

  /** Record that every row has now been projected under `version`. Throws when the write fails. */
  def record(version: String): Unit
}

object ReadModelDerivationMarker {
  /** For a wiring with no store to keep the marker in (a spec, a Mongo-less boot): reports the
   *  running version as already recorded, so no pass is ever owed. */
  val none: ReadModelDerivationMarker = new ReadModelDerivationMarker {
    def recorded(): Try[Option[String]] = Success(Some(ReadModelProjection.DerivationVersion))
    def record(version: String): Unit   = ()
  }
}
