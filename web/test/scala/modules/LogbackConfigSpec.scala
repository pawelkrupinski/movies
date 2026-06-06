package modules

import ch.qos.logback.classic.Logger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

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
}
