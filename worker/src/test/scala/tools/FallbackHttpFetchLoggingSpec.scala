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

  /** Unique per test, so events can be attributed. The appender hangs off the SHARED
   *  `FallbackHttpFetch` logger, and specs run concurrently — without this, another
   *  suite's fall-through warning lands in this one's capture and fails it, but only when
   *  the whole layer runs. */
  private def uniqueUrl(label: String): String =
    s"https://fallback-logging-spec.test/$label-${java.util.UUID.randomUUID()}"

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

    val url = uniqueUrl("answered")
    val (result, events) = capture(chain.get(url))

    result shouldBe "answer"
    val ours = events.filter(_.getFormattedMessage.contains(url))
    withClue(s"a successful fallback is not a warning, but logged: ${ours.map(_.getFormattedMessage)}: ") {
      ours.filter(_.getLevel == Level.WARN) shouldBe empty
    }
  }

  // Still recorded, just not shouted: the fall-through is exactly what you want when
  // diagnosing why a fixture wasn't used.
  it should "still record the fall-through at debug" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "cache-or-live" -> answering))

    val url = uniqueUrl("debug")
    val (_, events) = capture(chain.get(url))

    events.filter(e => e.getLevel == Level.DEBUG && e.getFormattedMessage.contains(url))
      .map(_.getFormattedMessage).mkString should include ("fixtures")
  }

  it should "warn once, naming every backend, when they all fail" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> failing("connection refused")))

    val url = uniqueUrl("gone")
    val (_, events) = capture(a [RuntimeException] should be thrownBy chain.get(url))

    val warnings = events.filter(e => e.getLevel == Level.WARN && e.getFormattedMessage.contains(url))
    warnings.size shouldBe 1
    warnings.head.getFormattedMessage should include ("no fixture file")
    warnings.head.getFormattedMessage should include ("connection refused")
  }

  /**
   * A definitive 404 from the last backend must reach the caller AS a 404.
   *
   * Wrapped in the composite `RuntimeException`, it stopped looking like one:
   * `EnrichmentRead` tests the message for a leading `HTTP <code>`, and
   * "All 2 backends failed for ..." does not match, so an answer was booked as a
   * failed read. Metacritic and Rotten Tomatoes probe ~20 candidate slugs of which
   * at most one exists, so the first losing probe then aborted the whole ladder --
   * a convergence leg came out with Metacritic 17 and RT 73 against production's
   * 308 and 354.
   */
  it should "propagate a last-backend NOT FOUND instead of burying it in a composite failure" in {
    val chain = new FallbackHttpFetch(Seq(
      "fixtures" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new java.io.FileNotFoundException("no fixture for " + url)
      },
      "live" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new HttpStatusException(404, "GET", url, None)
      }))

    // The shape the slug ladders depend on: absent, not broken.
    EnrichmentRead.absentOnNotFound(chain.get("https://www.metacritic.com/movie/nope")) shouldBe None
  }

  // ...while a genuine outage still reads as one, so a dead upstream can never be
  // mistaken for "this film has no page" -- the distinction EnrichmentRead exists for.
  it should "still report a composite failure when the last backend did not answer" in {
    val chain = new FallbackHttpFetch(Seq(
      "fixtures" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new java.io.FileNotFoundException("no fixture")
      },
      "live" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new HttpStatusException(503, "GET", url, None)
      }))

    a [RuntimeException] should be thrownBy
      EnrichmentRead.absentOnNotFound(chain.get("https://www.metacritic.com/movie/nope"))
  }
}
