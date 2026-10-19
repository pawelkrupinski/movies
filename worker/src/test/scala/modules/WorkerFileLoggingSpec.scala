package modules

import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.joran.JoranConfigurator
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.rolling.RollingFileAppender
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.Logger.ROOT_LOGGER_NAME

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * Guards that the worker's real `logback.xml` ships logs to a PERSISTENT file
 * under `KINOWO_LOG_DIR` — the mechanism that keeps logs across a Fly restart
 * (the volume at /data), since `kubectl logs` only retains a short rolling window
 * and loses exactly the pre-restart logs needed to diagnose a crash/throttle
 * spiral. Loads the actual production config into a private `LoggerContext`,
 * finds the file appender on the root logger, and asserts it is a rolling file
 * appender rooted at KINOWO_LOG_DIR that actually writes an event to disk.
 */
class WorkerFileLoggingSpec extends AnyFlatSpec with Matchers {

  "the worker logback.xml" should "send root logs to a rolling file under KINOWO_LOG_DIR (survives a restart)" in {
    val directory: Path = Files.createTempDirectory("kinowo-log-test")
    // Handed to THIS context only: logback resolves ${KINOWO_LOG_DIR} from the context's own
    // properties before system properties and the OS environment, so the spec names the
    // directory without a JVM-wide property every other suite would see.
    val context = new LoggerContext()
    context.putProperty("KINOWO_LOG_DIR", directory.toString)
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(context)
      configurator.doConfigure(getClass.getResourceAsStream("/logback.xml"))

      // The root logger must carry a started rolling FILE appender whose active
      // file lives under KINOWO_LOG_DIR — i.e. on the persistent volume, so the
      // bytes survive a restart. (logback's own writing of events to a configured
      // RollingFileAppender is its contract, not re-tested here; this guards OUR
      // wiring: before the file appender was added, getAppender("FILE") is null.)
      val appender = context.getLogger(ROOT_LOGGER_NAME).getAppender("FILE")
      appender shouldBe a[RollingFileAppender[?]]

      val fileAppender = appender.asInstanceOf[RollingFileAppender[ILoggingEvent]]
      fileAppender.isStarted shouldBe true
      Path.of(fileAppender.getFile).toAbsolutePath.toString should startWith(directory.toAbsolutePath.toString)
      Path.of(fileAppender.getFile).getFileName.toString shouldBe "worker.log"
    } finally {
      context.stop()
      Files.walk(directory).iterator().asScala.toSeq.reverse.foreach(p => Files.deleteIfExists(p))
    }
  }

  /** Stdout is what Vector ships to VictoriaLogs: one JSON object per line there, so the build the boot stamps and every
   *  pair a log call adds arrive as fields (`tools.JsonLineEncoder`); the file on the volume, read by a person after a
   *  restart, stays text with the pairs appended. */
  it should "write stdout as one JSON object per line, with the build and a call's pairs as fields, and the file as text with them" in {
    val directory: Path = Files.createTempDirectory("kinowo-log-test")
    val context = new LoggerContext()
    context.setMDCAdapter(new ch.qos.logback.classic.util.LogbackMDCAdapter)
    context.putProperty("KINOWO_LOG_DIR", directory.toString)
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(context)
      configurator.doConfigure(getClass.getResourceAsStream("/logback.xml"))
      tools.LogFields.stamp(context, settings.CommitSha("0e41a4812"))
      val root  = context.getLogger(ROOT_LOGGER_NAME)
      val event = new ch.qos.logback.classic.spi.LoggingEvent("fqcn", context.getLogger("services.identity.adapters.IdentityProjection"),
        ch.qos.logback.classic.Level.INFO, "identity take change: lost", null, null)
      event.addKeyValuePair(new org.slf4j.event.KeyValuePair("kind", "lost"))
      def encoded(name: String) = root.getAppender(name).asInstanceOf[ch.qos.logback.core.OutputStreamAppender[ILoggingEvent]].getEncoder
      val stdout = play.api.libs.json.Json.parse(encoded("STDOUT").encode(event))
      (stdout \ "message").as[String] shouldBe "identity take change: lost"
      (stdout \ "kind").as[String] shouldBe "lost"
      (stdout \ "build").as[String] shouldBe "0e41a4812"
      new String(encoded("FILE").encode(event), java.nio.charset.StandardCharsets.UTF_8) should
        (include("identity take change: lost") and include("kind=\"lost\"") and include("[0e41a4812]"))
    } finally {
      context.stop()
      Files.walk(directory).iterator().asScala.toSeq.reverse.foreach(p => Files.deleteIfExists(p))
    }
  }
}
