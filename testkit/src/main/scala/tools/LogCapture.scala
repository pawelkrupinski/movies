package tools

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger as LogbackLogger}
import ch.qos.logback.core.AppenderBase
import org.slf4j.LoggerFactory

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
 * Collects the log events a block of test code emits, safely under a parallel suite run.
 *
 * The obvious way to do this is logback's own `ListAppender`, and five specs did — but
 * its `list` is a bare `ArrayList` appended to WITHOUT synchronization, and every logger
 * a test attaches to is process-global. ScalaTest runs suites in parallel, so while one
 * spec captures `kinowo.removal-audit` (or, worse, ROOT), other suites' threads are
 * appending to the very list it is copying: `app.list.asScala.toSeq` walks an
 * `ArrayList` iterator another thread has just grown, and throws
 * `ConcurrentModificationException`. `MovieCacheSpec` failed exactly that way on
 * 2026-09-02, inside a test about degraded cinema scrapes that has nothing to do with
 * logging — which is what makes the failure expensive to read.
 *
 * A `ConcurrentLinkedQueue` fixes it at the root: its iterator is weakly consistent, so
 * a snapshot taken while other threads append is merely a snapshot, never an exception.
 *
 * What it does NOT fix, because logback owns it: attaching an appender to a logger that
 * another thread is hammering can lose events. `COWArrayList.refreshCopy` may publish an
 * array snapshot taken just before the `add`, and every event until the next refresh then
 * goes to the old array — so a capture on a hot global logger can come back empty even
 * though the line was logged. Measured at roughly one attach in twenty against a thread
 * logging in a tight loop. Capture a class-specific logger where you can.
 *
 * Lives in testkit so worker, web and e2e share one copy. `common`'s own tests cannot
 * reach it — testkit depends on `common`, and the reverse would be a project cycle — so
 * `RemovalAuditSpec` carries the same appender inline; keep the two in step.
 */
object LogCapture {

  /**
   * Runs `body` with a collecting appender on `loggerName`, returning everything that
   * landed on it.
   *
   * `level` is an OPTION, and the default of `None` means "leave the logger's level
   * alone" — the specs that assert an audit line reaches the ROOT appenders are
   * asserting the configured level lets it through, and forcing DEBUG for the capture
   * would answer their question for them. Pass `Some(Level.DEBUG)` when the point is to
   * see lines the configuration would otherwise suppress.
   *
   * Events from OTHER threads are included, which is the honest answer for a global
   * logger; a caller that must pin an exact list uses [[thisThread]] instead.
   */
  def capture(loggerName: String, level: Option[Level] = None)(body: => Unit): Seq[ILoggingEvent] = {
    val logger    = LoggerFactory.getLogger(loggerName).asInstanceOf[LogbackLogger]
    val collector = new CollectingAppender
    collector.setContext(logger.getLoggerContext)
    collector.start()
    val previous = logger.getLevel
    level.foreach(logger.setLevel)
    logger.addAppender(collector)
    try {
      body
      collector.events
    } finally {
      logger.detachAppender(collector)
      collector.stop()
      level.foreach(_ => logger.setLevel(previous))
    }
  }

  /**
   * The subset of [[capture]] this thread emitted.
   *
   * For the specs that pin an EXACT event list: `kinowo.removal-audit` is a
   * fixed-name, process-global logger and `StagingFoldSpec`, `MovieCacheSpec` and
   * friends all perform real deletes in parallel, so one foreign line fails an
   * exact-match assertion. Every call under test is synchronous on the test thread,
   * which makes this filter both precise and complete.
   */
  def thisThread(loggerName: String, level: Option[Level] = None)(body: => Unit): Seq[ILoggingEvent] = {
    val thread = Thread.currentThread().getName
    capture(loggerName, level)(body).filter(_.getThreadName == thread)
  }

  /** Appends into a queue whose iterator cannot throw, unlike `ListAppender`'s `ArrayList`. */
  final class CollectingAppender extends AppenderBase[ILoggingEvent] {
    private val queue = new ConcurrentLinkedQueue[ILoggingEvent]()
    override def append(event: ILoggingEvent): Unit = { queue.add(event); () }
    def events: Seq[ILoggingEvent] = queue.iterator().asScala.toSeq
  }
}
