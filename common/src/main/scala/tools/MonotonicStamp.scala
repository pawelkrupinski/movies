package tools

import java.time.{Clock, Instant}

/** A change stamp that only moves forward. A wall clock is coarse and can step back, so two
 *  changes in a row can read the same `Instant` — or an earlier one — and a validator that did
 *  not move is a 304 for changed bytes. */
object MonotonicStamp {
  /** `clock`'s instant when it is past `previous`, else a nanosecond past `previous`. */
  def after(previous: Instant, clock: Clock): Instant = {
    val now = clock.instant()
    if (now.isAfter(previous)) now else previous.plusNanos(1)
  }
}
