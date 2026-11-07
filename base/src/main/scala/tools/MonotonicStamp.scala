package tools

import java.time.{Clock, Instant}
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicReference

/** A change stamp that only moves forward. A wall clock is coarse and can step back, so two
 *  changes in a row can read the same `Instant` — or an earlier one — and a validator that did
 *  not move is a 304 for changed bytes. */
object MonotonicStamp {
  /** `clock`'s instant when it is past `previous`, else a nanosecond past `previous`. */
  def after(previous: Instant, clock: Clock): Instant = after(previous, clock, ChronoUnit.NANOS)

  /** `clock`'s instant, truncated to `unit`, when that is past `previous`, else one `unit` past
   *  `previous` — for a stamp stored at `unit`'s precision (a BSON date keeps milliseconds), where
   *  a nanosecond step would be truncated away on the way in. */
  def after(previous: Instant, clock: Clock, unit: ChronoUnit): Instant = {
    val now = clock.instant().truncatedTo(unit)
    if (now.isAfter(previous)) now else previous.plus(1, unit)
  }
}

/** A process's sequence of stamps, each strictly after every one it handed out before, at `unit`'s
 *  precision: `clock`'s time while it moves, and one `unit` on from the last stamp while it does not
 *  (two writes in one millisecond, a clock stepped back, a test's pinned clock).
 *
 *  Strict order is what lets one stamp double as a version and a cursor: a guarded write that
 *  compares the stamp it read sees any write landed since, and a catch-up that read "everything
 *  stamped after S" can take a stamp from the same sequence as its own floor, knowing every write
 *  stamped later is after it. Within this process only — another process has its own sequence. */
final class MonotonicStampSequence(clock: Clock, unit: ChronoUnit = ChronoUnit.MILLIS) {
  private val last = new AtomicReference[Instant](Instant.MIN)

  def next(): Instant = last.updateAndGet(MonotonicStamp.after(_, clock, unit))
}
