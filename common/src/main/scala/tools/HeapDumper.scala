package tools

import com.sun.management.HotSpotDiagnosticMXBean
import play.api.Logging

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

/**
 * Writes an HPROF heap dump on demand via the HotSpot diagnostic MXBean — the
 * same dump `jmap -dump:live` produces, but triggerable from inside the process.
 *
 * Used by the [[services.tasks.LivenessWatchdog]]: when it decides the JVM is
 * wedged it grabs a dump of the about-to-die heap BEFORE it exits, so the cause
 * (leak vs merely-too-tight working set) can be analysed offline. This is the
 * companion to the `-XX:+HeapDumpOnOutOfMemoryError` JVM flag: the flag fires
 * only at the hard `OutOfMemoryError`, but the watchdog often exits a heap
 * death-spiral minutes BEFORE that point, so without this the dump would be
 * missed in exactly the case we most want it. The dump is best-effort — a
 * failure (no volume, disk full, a non-HotSpot JVM with no MXBean) is logged and
 * swallowed so it never blocks the restart that actually recovers the worker.
 *
 * `dumpHeap` won't overwrite an existing file, so the filename carries a
 * caller-supplied millis stamp; `dir` is created if absent (the Fly volume mount).
 */
object HeapDumper extends Logging {

  /** Write a live-objects HPROF dump to `dir/wedge-<millis>.hprof`. Returns the
   *  path on success, None on any failure. */
  def dump(dir: String, now: () => Long = () => System.currentTimeMillis()): Option[String] =
    Try {
      Files.createDirectories(Paths.get(dir))
      val path = Paths.get(dir, s"wedge-${now()}.hprof").toString
      val bean = ManagementFactory.newPlatformMXBeanProxy(
        ManagementFactory.getPlatformMBeanServer,
        "com.sun.management:type=HotSpotDiagnostic",
        classOf[HotSpotDiagnosticMXBean])
      // live=true walks only reachable objects (smaller, and what a leak analysis wants).
      bean.dumpHeap(path, true)
      path
    } match {
      case Success(path) =>
        logger.error(s"HeapDumper: wrote wedged-heap dump to $path")
        Some(path)
      case Failure(e) =>
        logger.error(s"HeapDumper: heap dump to $dir failed (${e.getClass.getSimpleName}: ${e.getMessage}) — continuing to restart without it.")
        None
    }
}
