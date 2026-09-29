package integration

import java.lang.management.ManagementFactory
import javax.management.ObjectName

/**
 * The bytes of the objects still reachable, as the JVM's class histogram totals them: the
 * histogram forces a full collection and counts only live objects.
 *
 * `System.gc()` plus `totalMemory - freeMemory` — what the identity measure used before — also
 * counts whatever that one collection left behind, and inside sbt's own process that swung by
 * ±12 MB between identical runs; PL's model once "grew" 24 → 36 MB with no change of its own.
 * A difference of two of these readings is what a structure itself keeps.
 */
object LiveHeap {
  private val Total = """(?m)^\s*Total\s+\d+\s+(\d+)\s*$""".r

  /** Read twice, the second kept: the first collection after a busy stretch still leaves some
   *  ~10 MB a second one reclaims (a fresh JVM's first two readings differed by 11 MB). */
  def bytes(): Long = { total(); total() }

  private def total(): Long = {
    val histogram = ManagementFactory.getPlatformMBeanServer.invoke(
      new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram",
      Array[AnyRef](Array.empty[String]), Array("[Ljava.lang.String;")).toString
    Total.findFirstMatchIn(histogram).map(_.group(1).toLong)
      .getOrElse(throw new IllegalStateException(s"no Total line in the class histogram:\n${histogram.takeRight(500)}"))
  }

  def megabytes(): Long = bytes() >> 20
}
