package tools

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

/**
 * What a fallback chain SAYS while it is working normally.
 *
 * Falling through is the design, not an incident. The convergence legs put a
 * recorded-fixture backend in front of a cache-or-live one, and roughly half a
 * country's films never resolve — every one of those misses the fixture layer by
 * construction, because a 404 leaves no response to record, and is then answered
 * from the remembered-verdict cache in the same millisecond.
 *
 * Logged at WARN, that produced thousands of nine-line warnings per run listing
 * every candidate fixture path tried, on a run that was making no network calls at
 * all and had nothing wrong with it. It reads as a broken cache — it was reported as
 * one — and it buries the warnings that do matter.
 */
class FallbackHttpFetchLoggingSpec extends AnyFlatSpec with Matchers {

  private def capture[A](body: => A): (A, Seq[ILoggingEvent]) = {
    val logger   = LoggerFactory.getLogger(classOf[FallbackHttpFetch]).asInstanceOf[ch.qos.logback.classic.Logger]
    val captured = new ListAppender[ILoggingEvent]
    captured.setContext(logger.getLoggerContext)
    captured.start()
    logger.addAppender(captured)
    val previous = logger.getLevel
    logger.setLevel(Level.TRACE)          // so a DEBUG line is observable if one is emitted
    try (body, captured.list.asScala.toSeq)
    finally {
      logger.setLevel(previous)
      logger.detachAppender(captured)
      captured.stop()
    }
  }

  private def failing(message: String): HttpFetch = new HttpFetch {
    override def get(url: String): String = throw new java.io.FileNotFoundException(message)
    override def post(url: String, body: String, contentType: String): String = throw new java.io.FileNotFoundException(message)
  }

  private val answering: HttpFetch = new HttpFetch {
    override def get(url: String): String = "answer"
    override def post(url: String, body: String, contentType: String): String = "answer"
  }

  "a fallback chain" should "not warn when a later backend answers" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "cache-or-live" -> answering))

    val (result, events) = capture(chain.get("https://www.rottentomatoes.com/m/nope"))

    result shouldBe "answer"
    withClue(s"a successful fallback is not a warning, but logged: ${events.map(_.getFormattedMessage)}: ") {
      events.filter(_.getLevel == Level.WARN) shouldBe empty
    }
  }

  // Still recorded, just not shouted: the fall-through is exactly what you want when
  // diagnosing why a fixture wasn't used.
  it should "still record the fall-through at debug" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "cache-or-live" -> answering))

    val (_, events) = capture(chain.get("https://example.test/x"))

    events.filter(_.getLevel == Level.DEBUG).map(_.getFormattedMessage).mkString should include ("fixtures")
  }

  it should "warn once, naming every backend, when they all fail" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> failing("connection refused")))

    val (_, events) = capture(a [RuntimeException] should be thrownBy chain.get("https://example.test/gone"))

    val warnings = events.filter(_.getLevel == Level.WARN)
    warnings.size shouldBe 1
    warnings.head.getFormattedMessage should include ("no fixture file")
    warnings.head.getFormattedMessage should include ("connection refused")
  }
}
