package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import play.api.Logging
import tools.DaemonExecutors

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit
import javax.management.ObjectName
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodic JVM native-memory + vitals sampler, on the SAME registry/`/metrics`
 * endpoint as [[WorkerTaskMetrics]] / [[WorkerCorpusMetrics]].
 *
 * WHY THIS EXISTS: the worker self-exits ~every 5-6h with a JVM-internal
 * NON-heap OOM (`exit_code=3`, `oom_killed=false`, no heap dump). A 2026-07-09
 * probe ruled out heap/thread/CodeCache/Metaspace/direct leaks — what grows is
 * ~100 MB of NATIVE memory per JVM life that is INVISIBLE to the standard
 * `jvm_memory_*` pool metrics. This sampler makes that visible:
 *
 *   - `kinowo_worker_native_memory_bytes{category=…}` — committed bytes per
 *     HotSpot NMT category (Thread / Code / GC / Metaspace / Internal / Class /
 *     Arena Chunk / …). Chart it against `process_resident_memory_bytes` to see
 *     which native bucket climbs toward the next OOM, and alert on it.
 *   - a compact `VITALS …` log line to `/data/logs` (heap / nonheap / threads /
 *     native-committed + the top categories) so the pre-death trend SURVIVES a
 *     restart even when the Grafana / `flyctl logs` windows don't.
 *
 * Native memory is read in-process via the HotSpot DiagnosticCommand MBean's
 * `vmNativeMemory("summary")` (no `jcmd` needed — the box is JRE-only). It
 * requires `-XX:NativeMemoryTracking=summary` (set in `fly.worker.toml`
 * JAVA_OPTS). If NMT is off the command returns a "not enabled" string, the
 * parser yields no categories, the gauges stay at their seeded 0 and the sampler
 * logs once — it never throws.
 */
class JvmVitalsSampler(
  registry:       PrometheusRegistry,
  sampleInterval: FiniteDuration  = JvmVitalsSampler.DefaultSampleInterval,
  readNmtSummary: () => String    = JvmVitalsSampler.readNmtSummaryViaMBean
) extends Logging {
  import JvmVitalsSampler._

  private val nativeMemory = Gauge.builder()
    .name(Name)
    .help("Committed native memory in bytes per HotSpot NMT category (Thread/Code/GC/Metaspace/Internal/Class/…). Requires -XX:NativeMemoryTracking=summary. Surfaces the native growth behind the worker's non-heap OOM restarts, which is invisible to the jvm_memory_* pools.")
    .labelNames("category")
    .register(registry)

  // Materialize the common categories at 0 so the panel has series from boot.
  SeedCategories.foreach(c => nativeMemory.labelValues(c).set(0.0))

  private val scheduler = DaemonExecutors.scheduler("worker-jvm-vitals")
  private var tick      = 0L

  /** Read NMT, publish per-category committed bytes, and (every `LogEveryTicks`
   *  ticks) emit the durable VITALS line. Never throws. */
  def sample(): Unit = {
    val committed = parseCommittedByCategory(Try(readNmtSummary()).getOrElse(""))
    committed.foreach { case (category, bytes) => nativeMemory.labelValues(category).set(bytes.toDouble) }
    if (tick % LogEveryTicks == 0) logger.info(vitalsLine(committed))
    tick += 1
  }

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"worker-jvm-vitals initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"worker-jvm-vitals sample tick failed: ${e.getMessage}") },
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object JvmVitalsSampler {
  val Name = "kinowo_worker_native_memory_bytes"

  /** 60 s: high enough resolution to catch a native climb within a ~1 h flare,
   *  cheap (one MBean call). The VITALS log is throttled to 1-in-5 ticks so it
   *  doesn't spam `flyctl logs`. */
  val DefaultSampleInterval: FiniteDuration = 60.seconds
  val LogEveryTicks: Long = 5 // log the VITALS line every 5th sample (~5 min)

  /** The stable HotSpot NMT categories, seeded at 0 so Grafana has no boot gap.
   *  Extra categories a given JVM reports are still published on first sample. */
  val SeedCategories: Seq[String] = Seq(
    "Java Heap", "Class", "Thread", "Code", "GC", "Compiler", "Internal", "Other",
    "Symbol", "Native Memory Tracking", "Arena Chunk", "Metaspace")

  // "-                    Thread (reserved=59815KB, committed=59815KB)"
  // "-        Shared class space (reserved=16384KB, committed=12944KB, readonly=0KB)"
  private val CategoryLine = """-\s*(\S.*?)\s*\(reserved=\d+KB,\s*committed=(\d+)KB""".r

  /** Parse committed BYTES per category from an NMT `summary` dump. Pure — feed a
   *  captured summary string. Empty when NMT is disabled (no matching lines). */
  def parseCommittedByCategory(nmt: String): Map[String, Long] =
    CategoryLine.findAllMatchIn(nmt).map(m => m.group(1).trim -> m.group(2).toLong * 1024L).toMap

  /** One durable log line: uptime + heap/nonheap + threads + native total + top
   *  native categories by committed size. */
  def vitalsLine(committedByCategory: Map[String, Long]): String = {
    val mb            = ManagementFactory.getMemoryMXBean
    val heapMb        = mb.getHeapMemoryUsage.getUsed / MB
    val nonHeapMb     = mb.getNonHeapMemoryUsage.getUsed / MB
    val threads       = ManagementFactory.getThreadMXBean.getThreadCount
    val uptimeMin     = ManagementFactory.getRuntimeMXBean.getUptime / 60000
    val nativeTotalMb = committedByCategory.values.sum / MB
    val top = committedByCategory.toSeq.sortBy(-_._2).take(4)
      .map { case (c, b) => s"$c=${b / MB}m" }.mkString(" ")
    s"VITALS uptime=${uptimeMin}m heap=${heapMb}m nonheap=${nonHeapMb}m threads=$threads nativeCommitted=${nativeTotalMb}m [$top]"
  }

  private val MB = 1024L * 1024L

  /** Invoke the HotSpot DiagnosticCommand MBean's `vmNativeMemory summary`
   *  in-process (equivalent to `jcmd <pid> VM.native_memory summary`). */
  def readNmtSummaryViaMBean(): String = {
    val server = ManagementFactory.getPlatformMBeanServer
    val name   = new ObjectName("com.sun.management:type=DiagnosticCommand")
    server.invoke(
      name, "vmNativeMemory",
      Array[Object](Array[String]("summary")),
      Array("[Ljava.lang.String;")
    ).asInstanceOf[String]
  }
}
