package tools

import java.lang.management.ManagementFactory

/**
 * CPU time burned by the CALLING thread, in nanoseconds — the honest denominator
 * for "how much CPU does this code path cost".
 *
 * Exists because wall-clock is not CPU, and on this fleet the gap is not a rounding
 * error. `ReadModelProjector` used to time its projection with `System.nanoTime()`
 * on the argument that the projection is pure CPU with no I/O, so the two should
 * agree. They don't, for two independent reasons:
 *
 *  - **Concurrency.** Several pool threads project at once, so summing each call's
 *    wall-clock counts more seconds than the second contains. Fine for a latency
 *    histogram, meaningless as a share of one machine's CPU.
 *  - **Steal.** On a credit-throttled shared-cpu box the hypervisor deschedules the
 *    thread mid-projection. Wall-clock keeps running; the thread is not on CPU.
 *    The more throttled the machine, the more wall-clock overstates CPU — so the
 *    metric was least trustworthy exactly when it was being read to diagnose a
 *    throttle. Measured on `kinowo-worker-uk` 2026-07-28: projection wall-clock
 *    read 45.9 centi-cores against a whole-process CPU of 18.0, i.e. the "share of
 *    CPU" exceeded the CPU.
 *
 * `getCurrentThreadCpuTime` is immune to both: it counts only nanos this thread was
 * actually executing, so summing it across threads is a true core-seconds total.
 */
trait ThreadCpuClock {

  /** CPU nanoseconds consumed by the calling thread so far. Only DIFFERENCES between
   *  two readings on the SAME thread are meaningful — the origin is unspecified. */
  def nanos(): Long
}

object ThreadCpuClock {

  /**
   * The real thing, via `ThreadMXBean`. Falls back to `System.nanoTime()` when the
   * JVM reports no per-thread CPU measurement — the fallback re-introduces the
   * wall-clock skew above, but a slightly wrong number beats a metric that silently
   * stops recording. Every mainstream JVM (HotSpot included) supports and enables it.
   */
  val threadMxBean: ThreadCpuClock = new ThreadCpuClock {
    private val bean      = ManagementFactory.getThreadMXBean
    private val supported = bean.isCurrentThreadCpuTimeSupported && bean.isThreadCpuTimeEnabled

    def nanos(): Long = if (supported) bean.getCurrentThreadCpuTime else System.nanoTime()
  }
}
