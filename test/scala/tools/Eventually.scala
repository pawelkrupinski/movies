package tools

/** Tiny polling helper for tests that exercise event-driven async work. The
 *  bus listener dispatches to a worker pool, so the assertion needs a small
 *  window to settle. Re-throws the last failure when the deadline hits. */
object Eventually {
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
}
