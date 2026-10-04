package tools.costs

import org.scalatest.Assertions.fail

/**
 * Heap a block allocates on the calling thread, read the way a budget spec must read it to be
 * stable: warmed first, then measured several times and the MEDIAN taken.
 *
 * Allocation, not wall time: the per-thread counter does not move when another process takes the
 * CPU, so a budget measured this way holds on a loaded CI runner. What does move it is the JIT —
 * an interpreted or freshly compiled method allocates what escape analysis later removes — which
 * is what the warm-up runs and the median absorb.
 *
 * The only way a spec reads the counter (`NoHandRolledAllocationMeasurementSpec`); production
 * reads it through `tools.ThreadAllocation`, which this wraps.
 */
object AllocationMeter {

  /** Bytes `block` allocated on this thread, run once. */
  def once(block: => Any): Long = of(block)._2

  /** `block`'s value and the bytes it allocated on this thread, run once. */
  def of[A](block: => A): (A, Long) = {
    if (!tools.ThreadAllocation.supported) fail("this JVM cannot measure per-thread allocation; the suites run on HotSpot")
    tools.ThreadAllocation.of(block)
  }

  /** The median of `runs` measurements of `block`, after `warmups` unmeasured runs. */
  def median(warmups: Int = 5, runs: Int = 7)(block: => Any): Long = medianFrom(warmups, runs)(())(_ => block)

  /** [[median]] of `block` over a `fresh` state built, unmeasured, before each run — a cold cache, a
   *  new controller: what a cache-miss budget measures without counting the construction. */
  def medianFrom[S](warmups: Int = 5, runs: Int = 7)(fresh: => S)(block: S => Any): Long = {
    (1 to warmups).foreach(_ => block(fresh))
    val sorted = Vector.fill(runs) { val state = fresh; once(block(state)) }.sorted
    sorted(sorted.size / 2)
  }
}
