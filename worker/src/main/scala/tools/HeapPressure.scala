package tools

import java.lang.management.{ManagementFactory, MemoryType}
import scala.jdk.CollectionConverters._

/**
 * How full the heap stays: what its pools held after their last collection, as a share of the heap's maximum — the live
 * set, not the garbage between collections that `used` counts. Read by work that may be put off ([[pressed]]) so it never
 * pushes a heap-bound worker (US, DE) towards an OOM.
 */
object HeapPressure {
  /** The share of the heap held after the last collection past which work that can wait waits. */
  val Threshold = 0.7

  /** What the heap held after its last collection, as a share of its maximum (0 before any collection). */
  def heldAfterCollection(): Double = {
    val max = Runtime.getRuntime.maxMemory
    val held = ManagementFactory.getMemoryPoolMXBeans.asScala.iterator.filter(_.getType == MemoryType.HEAP)
      .flatMap(pool => Option(pool.getCollectionUsage)).map(_.getUsed).sum
    if (max <= 0 || max == Long.MaxValue) 0.0 else held.toDouble / max
  }

  /** Whether the heap held at least `threshold` of its maximum after its last collection. */
  def pressed(threshold: Double = Threshold): Boolean = heldAfterCollection() >= threshold
}
