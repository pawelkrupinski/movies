package tools.costs

import org.scalatest.Assertions.fail

import java.lang.management.ManagementFactory
import scala.concurrent.duration.{DurationLong, FiniteDuration}

/**
 * CPU time a block takes on the calling thread: the bound a spec puts on how much WORK something is, where allocation
 * ([[AllocationMeter]]) does not show it — a regex's backtracking, a walk of a map, which allocate nothing either way.
 *
 * CPU time, not wall time: the thread's CPU clock stands still while another process has the core, so a loaded machine
 * does not stretch it — wall time measured 113 ms for a 100 ms bound at load 150 (IdentityMeasuresSpec, 2026-10-18).
 * What still moves it a little (caches, a sibling hyperthread, the JIT) the fastest of several runs absorbs; a bound set
 * between a fixed path's cost and a broken one's (linear against quadratic) holds by orders of magnitude.
 */
object CpuMeter {
  private val threads = ManagementFactory.getThreadMXBean

  /** The fastest of `runs` measurements of the CPU `block` takes on this thread, after one unmeasured warm-up. */
  def fastest(runs: Int = 5)(block: => Any): FiniteDuration = {
    if (!threads.isCurrentThreadCpuTimeSupported) fail("this JVM cannot measure per-thread CPU time; the suites run on HotSpot")
    block
    (1 to runs).map { _ =>
      val before = threads.getCurrentThreadCpuTime
      block
      threads.getCurrentThreadCpuTime - before
    }.min.nanos
  }
}
