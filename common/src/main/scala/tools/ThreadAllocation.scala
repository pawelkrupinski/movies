package tools

/** How much heap a block allocated on the calling thread, where the JVM can say
 *  (HotSpot's `com.sun.management.ThreadMXBean`). Elsewhere the block simply runs and
 *  nothing is reported.
 *
 *  The ONE reader of the per-thread allocation counter — production's render and
 *  projection metrics and the budget specs (`tools.AllocationMeter`) all measure through
 *  it, and `NoHandRolledAllocationMeasurementSpec` keeps it that way. */
object ThreadAllocation {

  private val threads: Option[com.sun.management.ThreadMXBean] =
    java.lang.management.ManagementFactory.getThreadMXBean match {
      case mx: com.sun.management.ThreadMXBean if mx.isThreadAllocatedMemorySupported && mx.isThreadAllocatedMemoryEnabled => Some(mx)
      case _ => None
    }

  /** Whether this JVM can say what a block allocated. */
  def supported: Boolean = threads.isDefined

  /** `block`'s value, after handing `record` the bytes it allocated on this thread. */
  def measure[A](record: Long => Unit)(block: => A): A = threads match {
    case Some(mx) =>
      val thread = Thread.currentThread.threadId
      val before = mx.getThreadAllocatedBytes(thread)
      val result = block
      record(mx.getThreadAllocatedBytes(thread) - before)
      result
    case None => block
  }

  /** `block`'s value and the bytes it allocated on this thread — 0 where the JVM cannot say. */
  def of[A](block: => A): (A, Long) = {
    var bytes = 0L
    val value = measure(bytes = _)(block)
    (value, bytes)
  }
}
