package tools

/** Tiny polling helpers for tests that exercise event-driven async work — a change
 *  stream, a cursor fan-out, a worker-pool dispatch — where the assertion needs a
 *  small window to settle rather than being true the instant the write returns. */
object Eventually {
  /** Retry `check` until it passes or `timeoutMs` elapses. Re-throws the LAST failure
   *  when the deadline hits, so the caller's own assertion failure is what's reported
   *  rather than a generic timeout. Always tries at least once, and the last sleep is
   *  cut short so there is always one try AT the deadline. */
  def eventually(check: => org.scalatest.Assertion,
                 timeoutMs: Long = 2000,
                 pollMs: Long = 20): org.scalatest.Assertion = {
    val deadline = System.nanoTime() / 1000000 + timeoutMs
    while (true) {
      try return check
      catch {
        case t: Throwable =>
          val remaining = deadline - System.nanoTime() / 1000000
          if (remaining <= 0) throw t
          Thread.sleep(math.min(pollMs, remaining))
      }
    }
    throw new IllegalStateException("unreachable")
  }

  /** Poll `probe` until it holds or `timeoutMs` elapses, returning whatever it last
   *  read rather than throwing — for a spec that wants to fold the outcome into its
   *  own `withClue`/assertion instead of taking `eventually`'s generic failure. Was
   *  reimplemented inline, slightly differently, in three separate `it/` specs before
   *  this became the one copy. */
  def poll(timeoutMs: Long, pollMs: Long = 100)(probe: => Boolean): Boolean = {
    val deadline = System.nanoTime() / 1000000 + timeoutMs
    var ok = probe
    while (!ok && System.nanoTime() / 1000000 < deadline) { Thread.sleep(pollMs); ok = probe }
    ok
  }
}
