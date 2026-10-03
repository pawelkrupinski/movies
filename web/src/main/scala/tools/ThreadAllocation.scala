package tools

/** How much heap a block allocated on the calling thread, where the JVM can say
 *  (HotSpot's `com.sun.management.ThreadMXBean`). Elsewhere the block simply runs and
 *  nothing is reported. */
object ThreadAllocation {

  private val threads: Option[com.sun.management.ThreadMXBean] =
    java.lang.management.ManagementFactory.getThreadMXBean match {
      case mx: com.sun.management.ThreadMXBean if mx.isThreadAllocatedMemorySupported && mx.isThreadAllocatedMemoryEnabled => Some(mx)
      case _ => None
    }

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
}
