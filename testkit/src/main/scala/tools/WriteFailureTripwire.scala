package tools

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Logger as LogbackLogger, LoggerContext}
import ch.qos.logback.core.AppenderBase
import org.scalatest.Reporter
import org.scalatest.events.*
import org.slf4j.LoggerFactory
import services.movies.RepositoryWrite

import scala.jdk.CollectionConverters.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

/**
 * Fails an `it/` or `e2e` run in which any repository WRITE failed — the failure the
 * repositories swallow into a WARN so production keeps going, and which therefore sails
 * through a spec that never reads the row back.
 *
 * THE BUG THIS PINS. On 2026-09-23 every write of a cache-stripped record threw "Can't find
 * a codec for class [I", logged as `MovieRepository.upsert(…) failed` and moved on.
 * `RekeyScreeningsIntegrationSpec` writes such records and stays green with those lines in
 * its output (reproduced by reverting 4596f6b20); under the tripwire the run fails.
 *
 * HOW IT IS WIRED. A ScalaTest reporter, registered for every `it` and `e2e` config by
 * `project/WriteFailureTripwire.scala` (`-C tools.WriteFailureTripwire`) — so every suite,
 * present and future, runs under it with no mixin. It attaches a logback appender to the
 * ROOT logger when it is built and appends every line carrying `RepositoryWrite.FailedMarker`
 * — the one line `RepositoryWrite.attempt` logs as it counts `repository_write_failed` — that
 * the [[expected]] list does not excuse to a file in the directory the build passes as [[DirKey]]. The build's
 * `Tests.Cleanup` fails the task if that directory holds any: a reporter cannot fail a
 * test itself. The config is kept unforked, or the reporter would run in sbt's JVM while
 * the tests log in another.
 */
final class WriteFailureTripwire extends Reporter {
  import WriteFailureTripwire.*

  // The test last started on each thread, as far as this reporter has heard — events reach
  // it ASYNCHRONOUSLY, so this only labels a finding; it never decides one.
  private val lastStarted = new java.util.concurrent.ConcurrentHashMap[String, String]()
  private val sink        = new FindingSink(thread => Option(lastStarted.get(thread)))

  // Attached HERE, not at `RunStarting`: the reporter is built before any suite starts,
  // while events reach it asynchronously — a spec logging in its first millisecond would
  // otherwise beat the attach.
  private val appender = new TripwireAppender(sink)
  private val root     = rootLogger()
  root.foreach { logger =>
    appender.setContext(logger.getLoggerContext)
    appender.start()
    logger.addAppender(appender)
  }

  def apply(event: Event): Unit = event match {
    case e: RunStarting  => e.configMap.get(DirKey).foreach(dir => sink.writeTo(Paths.get(dir.toString)))
    case e: TestStarting => lastStarted.put(e.threadName, s"${e.suiteClassName.getOrElse(e.suiteName)} / ${e.testName}"); ()
    // Detached at the end: sbt may keep logback's classloader layer across runs, and an
    // appender left on its root would write this run's sink into the next.
    case _: RunCompleted | _: RunStopped | _: RunAborted => root.foreach(_.detachAppender(appender)); appender.stop()
    case _               => ()
  }
}

object WriteFailureTripwire {

  /** ScalaTest config-map key (`-D<key>=<dir>`) naming where findings are written. */
  val DirKey = "kinowo.writeFailureTripwire.dir"

  /**
   * A write a test fails ON PURPOSE — against a closed client, a dead port, a collection it
   * just broke — to assert the caller survives it. Excused by a fragment of the logged line
   * (the test's own sentinel title or id is the precise choice), each with the spec that
   * does it and why. Keep this list short: every entry is a place the tripwire is blind.
   */
  final case class Expected(lineContains: String, spec: String, reason: String)

  val expected: Seq[Expected] = Seq(
    Expected("__no_screenings_write_may_satisfy_this__", "ScreeningsWriteMetricIntegrationSpec",
      "installs a validator no screenings write can satisfy, to prove a refused write is not metered as written"),
    Expected("__repository-write-failure-sentinel__", "RepositoryWriteFailureIntegrationSpec",
      "installs a validator no film document can satisfy, to prove a failed write is counted, rolled back and retried"),
  )

  private def excused(line: String): Boolean = expected.exists(e => line.contains(e.lineContains))

  /** A write `RepositoryWrite.attempt` reported as failed — recognised by the marker that
   *  one log line carries, so the tripwire and the `repository_write_failed` counter share
   *  one source of truth rather than a message pattern. */
  def isWriteFailure(event: ILoggingEvent): Boolean =
    Option(event.getMarkerList).exists(_.asScala.exists(_.contains(RepositoryWrite.FailedMarker)))

  private def describe(event: ILoggingEvent): String =
    s"[${event.getLevel} ${event.getLoggerName}] ${event.getFormattedMessage}"

  /** The logback root, or None when logging is not logback (nothing to trip on). */
  private def rootLogger(): Option[LogbackLogger] = {
    val deadline = System.nanoTime() + 5L * 1000000000L
    var factory  = LoggerFactory.getILoggerFactory
    while (!factory.isInstanceOf[LoggerContext] && System.nanoTime() < deadline) {
      Thread.sleep(10)
      factory = LoggerFactory.getILoggerFactory
    }
    factory match {
      case context: LoggerContext => Some(context.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME))
      case _                      => None
    }
  }

  /** Findings, written the moment they are logged: sbt runs the build's cleanup before the
   *  runner's `done()`, so nothing may wait for `RunCompleted`. Until `RunStarting` names the
   *  directory, findings are held and flushed when it does.
   *
   *  The cleanup bumps a `generation` file once it has read the findings. A sink writes only
   *  while the generation it started under is current, so an appender a failed run left
   *  attached (a throwing cleanup can skip the runner's `done()`, and with it the detach)
   *  can never write into the next run's verdict. */
  private final class FindingSink(testOn: String => Option[String]) {
    private val held = scala.collection.mutable.ArrayBuffer.empty[String]
    private var file: Option[Path] = None
    private var generation = ""

    def writeTo(dir: Path): Unit = synchronized {
      generation = generationOf(dir)
      file = Some(dir.resolve(s"${ProcessHandle.current().pid()}-${System.nanoTime()}.txt"))
      held.foreach(append)
      held.clear()
    }

    def record(event: ILoggingEvent): Unit = synchronized {
      val line = s"thread ${event.getThreadName}" + testOn(event.getThreadName).fold("")(t => s" (last test started there: $t)") +
        s": ${describe(event)}"
      if (file.isDefined) append(line) else held += line
    }

    private def append(line: String): Unit = file.filter(f => generationOf(f.getParent) == generation).foreach { f =>
      Files.createDirectories(f.getParent)
      Files.write(f, (line + "\n").getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND); ()
    }

    private def generationOf(dir: Path): String = {
      val marker = dir.resolve("generation")
      if (Files.exists(marker)) Files.readString(marker) else ""
    }
  }

  private final class TripwireAppender(sink: FindingSink) extends AppenderBase[ILoggingEvent] {
    override def append(event: ILoggingEvent): Unit =
      if (isWriteFailure(event) && !excused(describe(event))) sink.record(event)
  }
}
