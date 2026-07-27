package modules

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.core.read.ListAppender
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger.ROOT_LOGGER_NAME
import org.slf4j.LoggerFactory
import services.movies.RemovalAudit

import scala.jdk.CollectionConverters._

/**
 * Guards that [[RemovalAudit]]'s INFO lines actually REACH an appender.
 *
 * The audit stream deliberately uses a fixed name (`kinowo.removal-audit`) rather
 * than a class logger, so none of the `services` / `clients` / `modules` levels in
 * `logback-base.xml` cover it — it inherits whatever the root has. With the root at
 * WARN that made every `filmRemoved` / `filmsRemoved` / `cardRemoved` call a silent
 * no-op: the audit trail added in 4f0c24ccf (2026-07-20) never emitted a single
 * line, and the 2026-07-27 `UnscreenedCleanup` that dropped 245 German rows was
 * only attributable because that class happens to log a summary of its own.
 *
 * Two levels of guard, because a level typo can break either independently:
 * behaviourally (a real call lands on the root's appenders under the config the
 * tests run with) and structurally (the prod `logback.xml` resolves the same way).
 */
class RemovalAuditLoggingSpec extends AnyFlatSpec with Matchers {

  "RemovalAudit" should "emit its 'N films removed' line to the root appenders" in {
    val root     = LoggerFactory.getLogger(ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
    val captured = new ListAppender[ILoggingEvent]
    captured.setContext(root.getLoggerContext)
    captured.start()
    root.addAppender(captured)
    try RemovalAudit.filmsRemoved("unscreened-cleanup", Seq("Ein Film (2026)"), reason = "no-current-screenings")
    finally {
      root.detachAppender(captured)
      captured.stop()
    }

    val audited = captured.list.asScala.filter(_.getLoggerName == RemovalAudit.LoggerName)
    audited.map(_.getFormattedMessage) should contain(
      "[unscreened-cleanup] 1 film(s) removed: reason=no-current-screenings ids=[Ein Film (2026)]")
  }

  "the worker logback.xml" should s"leave ${RemovalAudit.LoggerName} at INFO or finer" in {
    val ctx = new LoggerContext()
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(ctx)
      configurator.doConfigure(getClass.getResourceAsStream("/logback.xml"))

      ctx.getLogger(RemovalAudit.LoggerName).getEffectiveLevel.toInt should be <= Level.INFO.toInt
    } finally ctx.stop()
  }
}
