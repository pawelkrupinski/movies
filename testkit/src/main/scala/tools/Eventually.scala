package tools

/** Tiny polling helpers for tests that exercise event-driven async work — a change
 *  stream, a cursor fan-out, a worker-pool dispatch — where the assertion needs a
 *  small window to settle rather than being true the instant the write returns. */
object Eventually {
  /** Retry `check` until it passes or `timeoutMs` elapses. Re-throws the LAST failure
   *  when the deadline hits, so the caller's own assertion failure is what's reported
   *  rather than a generic timeout. */
  def eventually(check: => org.scalatest.Assertion,
                 timeoutMs: Long = 2000,
                 pollMs: Long = 20): org.scalatest.Assertion = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var last: Throwable = null
    while (System.currentTimeMillis() < deadline) {
      try return check
      catch { case t: Throwable => last = t; Thread.sleep(pollMs) }
    }
    throw last
  }

  /** Poll `probe` until it holds or `timeoutMs` elapses, returning whatever it last
   *  read rather than throwing — for a spec that wants to fold the outcome into its
   *  own `withClue`/assertion instead of taking `eventually`'s generic failure. Was
   *  reimplemented inline, slightly differently, in three separate `it/` specs before
   *  this became the one copy. */
  def poll(timeoutMs: Long, pollMs: Long = 100)(probe: => Boolean): Boolean = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var ok = probe
    while (!ok && System.currentTimeMillis() < deadline) { Thread.sleep(pollMs); ok = probe }
    ok
  }
}
