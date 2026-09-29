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
    val read = histogram()
    Total.findFirstMatchIn(read).map(_.group(1).toLong)
      .getOrElse(throw new IllegalStateException(s"no Total line in the class histogram:\n${read.takeRight(500)}"))
  }

  def megabytes(): Long = bytes() >> 20

  /** One class's live objects: how many, and their bytes. */
  final case class Live(name: String, instances: Long, bytes: Long)

  private val Row = """(?m)^\s*\d+:\s+(\d+)\s+(\d+)\s+(\S+)""".r

  /** Every class's live objects, by name — a full collection first, read twice like [[bytes]]. */
  def classes(): Map[String, Live] = { histogram(); parse(histogram()) }

  /** The classes that grew between two readings, largest growth first: what a structure keeps. */
  def grown(before: Map[String, Live], after: Map[String, Live]): Seq[Live] =
    after.values.toSeq.map { now =>
      val was = before.getOrElse(now.name, Live(now.name, 0, 0))
      Live(now.name, now.instances - was.instances, now.bytes - was.bytes)
    }.filter(_.bytes > 0).sortBy(live => (-live.bytes, live.name))

  /** The top of [[grown]] as one report line: `MB name ×instances`. */
  def render(grown: Seq[Live], top: Int = 8): String =
    grown.take(top).map(live => f"${live.bytes / 1048576.0}%.1f MB ${live.name} ×${live.instances}").mkString("; ")

  private def parse(histogram: String): Map[String, Live] =
    Row.findAllMatchIn(histogram).map(m => m.group(3) -> Live(m.group(3), m.group(1).toLong, m.group(2).toLong)).toMap

  private def histogram(): String =
    ManagementFactory.getPlatformMBeanServer.invoke(
      new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram",
      Array[AnyRef](Array.empty[String]), Array("[Ljava.lang.String;")).toString
}
