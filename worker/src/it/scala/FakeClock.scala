package integration

/** A hand-advanced millisecond clock, so a spec can exercise a retry budget without
 *  sleeping through it. Shared by [[RetryWithBackoffSpec]] and [[LiveUpstreamSpec]] —
 *  both drive the same `sleep`/`now` seam that [[RetryWithBackoff]] injects. */
class FakeClock(private var t: Long) {
  def now(): Long = t
  def advance(ms: Long): Unit = t += ms
}
