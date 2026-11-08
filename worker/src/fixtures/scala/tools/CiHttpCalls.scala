package tools

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.{ConcurrentLinkedQueue, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters._
import services.metrics.WorkerHttpMetrics

/**
 * WHAT A CI RUN ASKED THE OUTSIDE WORLD, per service and outcome: every live `kinowo_worker_http_total` cell of every
 * wiring this process built that reaches the real wire (a RECORDING convergence leg, the pre-suite fill), summed and
 * written as a TSV under [[Directory]]. scripts/ci/ci_http_calls.py turns the files into the job summary's table and
 * pushes them to Prometheus as `kinowo_ci_http_calls` (job="ci-recording"), beside production's own counter.
 *
 * Per wiring, not per process: a convergence leg builds a wiring per pass, each with its own registry, so the counts
 * are summed over every [[track]]ed counter. Only the counters are held -- not the registries, whose gauge callbacks
 * would keep each pass's caches alive.
 *
 * In CI only (a `GITHUB_STEP_SUMMARY` to report to): a flusher rewrites the file every minute, so a leg cut off at its
 * step ceiling still reports what it had asked by then, and [[write]] writes the final count.
 */
object CiHttpCalls {

  /** Where each process's file goes, relative to the checkout (sbt runs the suite and the fill from its root). */
  val Directory: Path = Path.of("target", "ci-http-calls")

  private val tracked = new ConcurrentLinkedQueue[WorkerHttpMetrics]()
  private val flushing = new AtomicBoolean(false)
  private lazy val inCi: Boolean = settings.HarnessConfiguration.resolve().stepSummaryFile.isDefined

  /** Counts `metrics` toward this process's report. */
  def track(metrics: WorkerHttpMetrics): Unit = {
    tracked.add(metrics)
    if (inCi && flushing.compareAndSet(false, true)) {
      val flusher = Executors.newSingleThreadScheduledExecutor { (r: Runnable) =>
        val thread = new Thread(r, "ci-http-calls-flush")
        thread.setDaemon(true)
        thread
      }
      flusher.scheduleWithFixedDelay(() => { val _ = scala.util.Try(write()) }, 1, 1, TimeUnit.MINUTES)
    }
  }

  /** Every non-zero cell, its labels by name, summed over the tracked counters. */
  def totals(): Map[Seq[(String, String)], Double] =
    tracked.asScala.toSeq
      .flatMap(_.snapshot().getDataPoints.asScala)
      .map(point => point.getLabels.asScala.toSeq.map(l => l.getName -> l.getValue).sortBy(_._1) -> point.getValue)
      .groupMapReduce(_._1)(_._2)(_ + _)
      .filter(_._2 > 0)

  /** The totals as a TSV: a header naming every label then `count`, one row per cell. */
  def tsv(cells: Map[Seq[(String, String)], Double]): String = {
    val names = cells.keys.flatMap(_.map(_._1)).toSeq.distinct.sorted
    val rows = cells.toSeq
      .map { case (labels, count) => names.map(n => labels.toMap.getOrElse(n, "")) :+ f"$count%.0f" }
      .sortBy(_.mkString("\t"))
    ((names :+ "count") +: rows).map(_.mkString("\t")).mkString("", "\n", "\n")
  }

  /** Rewrites this process's file, in CI and when any tracked wiring asked anything. */
  def write(): Unit = synchronized {
    val cells = totals()
    if (inCi && cells.nonEmpty) {
      Files.createDirectories(Directory)
      val file = Directory.resolve(s"${ProcessHandle.current().pid()}.tsv")
      val partial = Directory.resolve(s"${ProcessHandle.current().pid()}.tsv.partial")
      Files.writeString(partial, tsv(cells))
      val _ = Files.move(partial, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    }
  }
}
