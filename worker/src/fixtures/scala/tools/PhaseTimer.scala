package tools

/**
 * Live phase timing for the long replay harnesses.
 *
 * These runs are tens of minutes and, until now, opaque while they ran: a convergence
 * leg printed `bootCorpus …` and then nothing for 56 minutes. When one timed out, the
 * only way to learn where the time went was to attach a profiler to a live JVM and
 * sample it — which is not available on a CI runner, and is not something anyone should
 * need in order to read a build log.
 *
 * So every phase announces itself, reports what it cost, and long phases report progress
 * as they go. The point is diagnosis WHILE a run is in flight: a leg that is going to
 * blow its ceiling should say which phase is eating the budget by the time it is
 * half-way there, not after it is killed.
 *
 * `println` rather than a logger on purpose. This is harness narration for whoever is
 * watching the build, it must not be filtered by a log level meant for application
 * output, and it has to interleave correctly with ScalaTest's own stdout.
 */
object PhaseTimer {

  /** Announce `label`, run it, and report the elapsed seconds. */
  def timed[A](scope: String, label: String)(body: => A): A = {
    val started = System.nanoTime()
    println(s"[$scope] $label …")
    val result = body
    println(f"[$scope] $label done in ${elapsedSeconds(started)}%.1fs")
    result
  }

  /** A mid-phase heartbeat: `[pl] scrape 150/281 venues in 42.3s`. Long phases without
   *  one are indistinguishable from hung ones. */
  def progress(scope: String, label: String, done: Int, total: Int, startedNanos: Long): Unit =
    println(f"[$scope] $label $done/$total in ${elapsedSeconds(startedNanos)}%.1fs")

  /** How often a phase over many items should report. Frequent enough to see a stall
   *  develop, rare enough not to bury the log it is meant to make readable. */
  def shouldReport(done: Int, total: Int): Boolean =
    done == total || (done % math.max(1, total / 10) == 0)

  def elapsedSeconds(startedNanos: Long): Double = (System.nanoTime() - startedNanos) / 1e9
}
