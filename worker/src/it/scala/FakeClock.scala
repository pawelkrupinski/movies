package integration

/** A hand-advanced millisecond clock, so a spec can exercise a retry budget without
 *  sleeping through it. Shared by [[RetryWithBackoffSpec]] and [[LiveUpstreamSpec]] —
 *  both drive the same `sleep`/`stopwatch` seam that [[RetryWithBackoff]] injects. */
class FakeClock(private var t: Long) {
  def now(): Long = t
  def advance(ms: Long): Unit = t += ms
  /** The same hand-advanced time as the monotonic source a retry budget is measured on. */
  def stopwatch: _root_.tools.Stopwatch = new _root_.tools.Stopwatch(() => t * 1_000_000L)
}
