package tools

import java.time.Duration

/** The monotonic [[Stopwatch]] a unit spec hands code that only METERS something (a repository's
 *  write timer, a failure log's relog window): no spec reads real `System.nanoTime`, so what it
 *  measures is the same on every run. [[Frozen]] never moves; [[of]] reads a [[MutableClock]] the
 *  spec advances, for code that bounds a wait or throttles by elapsed time. */
object SpecStopwatch {

  /** Reads 0 forever: every elapsed time is zero. */
  val Frozen: Stopwatch = new Stopwatch(() => 0L)

  /** Nanoseconds since `clock`'s instant at the time of this call, so advancing the clock moves the stopwatch. */
  def of(clock: MutableClock): Stopwatch = {
    val origin = clock.instant()
    new Stopwatch(() => Duration.between(origin, clock.instant()).toNanos)
  }
}
