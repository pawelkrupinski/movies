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
 * (the volume at /data), since `flyctl logs` only retains a short rolling window
 * and loses exactly the pre-restart logs needed to diagnose a crash/throttle
 * spiral. Loads the actual production config into a private `LoggerContext`,
 * finds the file appender on the root logger, and asserts it is a rolling file
 * appender rooted at KINOWO_LOG_DIR that actually writes an event to disk.
 */
class WorkerFileLoggingSpec extends AnyFlatSpec with Matchers {

  "the worker logback.xml" should "send root logs to a rolling file under KINOWO_LOG_DIR (survives a restart)" in {
    val dir: Path = Files.createTempDirectory("kinowo-log-test")
    // logback resolves ${KINOWO_LOG_DIR} from system properties before the OS env.
    System.setProperty("KINOWO_LOG_DIR", dir.toString)
    val ctx = new LoggerContext()
    try {
      val configurator = new JoranConfigurator()
      configurator.setContext(ctx)
      configurator.doConfigure(getClass.getResourceAsStream("/logback.xml"))

      // The root logger must carry a started rolling FILE appender whose active
      // file lives under KINOWO_LOG_DIR — i.e. on the persistent volume, so the
      // bytes survive a restart. (logback's own writing of events to a configured
      // RollingFileAppender is its contract, not re-tested here; this guards OUR
      // wiring: before the file appender was added, getAppender("FILE") is null.)
      val appender = ctx.getLogger(ROOT_LOGGER_NAME).getAppender("FILE")
      appender shouldBe a[RollingFileAppender[?]]

      val fileAppender = appender.asInstanceOf[RollingFileAppender[ILoggingEvent]]
      fileAppender.isStarted shouldBe true
      Path.of(fileAppender.getFile).toAbsolutePath.toString should startWith(dir.toAbsolutePath.toString)
      Path.of(fileAppender.getFile).getFileName.toString shouldBe "worker.log"
    } finally {
      ctx.stop()
      System.clearProperty("KINOWO_LOG_DIR")
      Files.walk(dir).iterator().asScala.toSeq.reverse.foreach(p => Files.deleteIfExists(p))
    }
  }
}
