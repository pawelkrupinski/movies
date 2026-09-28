package services.cadence

import scala.util.hashing.MurmurHash3

/**
 * The boundary arithmetic of a phase-spread refresh schedule, in one place for the
 * worker's `DueWindow` (which decides what is due) and [[CadenceReport]] (which shows
 * when it next will be). A key refreshes once per period, at the boundaries
 * `phase + n·period`; a refresh counts toward one of them, and the key is due again
 * once the boundary after that one is reached. Which one it counts toward is the
 * schedule's [[DueBoundary.Counting]].
 */
object DueBoundary {

  /** Which boundary a refresh counts toward. */
  sealed trait Counting {
    private[DueBoundary] def countedBoundary(atMillis: Long, phaseMillis: Long, periodMillis: Long): Long
  }

  /** The boundary at or before the refresh: a key refreshed anywhere in a window is due
   *  at the next boundary, so the next refresh lands in `(last, last + period]`. Right
   *  for a schedule whose boundaries never move. */
  case object PrecedingBoundary extends Counting {
    private[DueBoundary] def countedBoundary(atMillis: Long, phaseMillis: Long, periodMillis: Long): Long =
      Math.floorDiv(atMillis - phaseMillis, periodMillis)
  }

  /** The boundary NEAREST the refresh, so the next one lands in
   *  `[last + period/2, last + 1.5·period)` whatever moved. For a schedule whose
   *  boundaries move — a rebuilt cost-spaced phase plan, a venue's shortened period.
   *  Under [[PrecedingBoundary]] a moved key is due at once whenever its new boundary
   *  falls between its last refresh and now, which on a plan change is about half the
   *  corpus in one burst; and a refresh that ran late in its window (behind a backlog)
   *  leaves the key due again moments later. */
  case object NearestBoundary extends Counting {
    private[DueBoundary] def countedBoundary(atMillis: Long, phaseMillis: Long, periodMillis: Long): Long =
      Math.floorDiv(atMillis - phaseMillis + periodMillis / 2, periodMillis)
  }

  /** The default phase: a deterministic offset in `[0, period)` hashed from the key —
   *  stable across restarts and needing no storage. */
  def hashedPhaseMillis(key: String, periodMillis: Long): Long =
    Math.floorMod(MurmurHash3.stringHash(key).toLong, periodMillis)

  /** Due iff a boundary after the one `lastMillis` counts toward has been reached. */
  def isDue(lastMillis: Long, nowMillis: Long, phaseMillis: Long, periodMillis: Long, counting: Counting): Boolean =
    Math.floorDiv(nowMillis - phaseMillis, periodMillis) > counting.countedBoundary(lastMillis, phaseMillis, periodMillis)

  /** The first instant at which a key refreshed at `lastMillis` is due again. */
  def nextDueMillis(lastMillis: Long, phaseMillis: Long, periodMillis: Long, counting: Counting): Long =
    phaseMillis + (counting.countedBoundary(lastMillis, phaseMillis, periodMillis) + 1) * periodMillis
}
