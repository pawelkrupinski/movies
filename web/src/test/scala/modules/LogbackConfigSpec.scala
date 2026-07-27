package modules

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.read.ListAppender
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import services.movies.RemovalAudit

import scala.jdk.CollectionConverters._

class LogbackConfigSpec extends AnyFlatSpec with Matchers {

  private val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    .asInstanceOf[Logger]

  private def appenderNames: Seq[String] = {
    val it = root.iteratorForAppenders()
    val buf = collection.mutable.Buffer.empty[String]
    while (it.hasNext) buf += it.next().getName
    buf.toSeq
  }

  "logback.xml" should "attach the SENTRY appender to the root logger" in {
    appenderNames should contain("SENTRY")
  }

  it should "attach the STDOUT appender to the root logger" in {
    appenderNames should contain("STDOUT")
  }

  it should "configure the Sentry appender as a SentryAppender" in {
    val appender = root.getAppender("SENTRY")
    appender shouldBe a[io.sentry.logback.SentryAppender]
  }

  // `RemovalAudit` writes to the fixed name `kinowo.removal-audit`, NOT a class
  // logger, so the `services` / `clients` levels above don't cover it — left to
  // inherit the root's WARN it silently swallows every removal the web app makes
  // (`MongoMovieRepository.delete` / `deleteById` audit through it). Asserted
  // behaviourally: the line has to land on a real appender, not merely resolve to
  // a level.
  it should s"let ${RemovalAudit.LoggerName} reach the root appenders at INFO" in {
    root.getLoggerContext.getLogger(RemovalAudit.LoggerName)
      .getEffectiveLevel.toInt should be <= Level.INFO.toInt

    val captured = new ListAppender[ILoggingEvent]
    captured.setContext(root.getLoggerContext)
    captured.start()
    root.addAppender(captured)
    try RemovalAudit.filmRemoved("movies.delete", "a-film-2026", reason = "orphan-id-reap")
    finally {
      root.detachAppender(captured)
      captured.stop()
    }

    captured.list.asScala
      .filter(_.getLoggerName == RemovalAudit.LoggerName)
      .map(_.getFormattedMessage) should contain(
        "[movies.delete] film removed: id=a-film-2026 reason=orphan-id-reap")
  }
}
