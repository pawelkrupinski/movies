package tools

import ch.qos.logback.classic.Level
import clients.tools.{ConstantHttpFetch, FailingHttpFetch}
import ch.qos.logback.classic.spi.ILoggingEvent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
    var result = Option.empty[A]
    // TRACE so a DEBUG line is observable if one is emitted.
    val events = LogCapture.capture(classOf[FallbackHttpFetch].getName, Some(Level.TRACE)) {
      result = Some(body)
    }
    (result.get, events)
  }

  private def failing(message: String): HttpFetch =
    new FailingHttpFetch((_, _) => new java.io.FileNotFoundException(message))

  private val answering: HttpFetch = new ConstantHttpFetch("answer")

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
   * `ReadOutcome.classify` keys on the typed `HttpStatusException`, which the
   * composite is not, so an answer was booked as a failed read. Metacritic and Rotten Tomatoes probe ~20 candidate slugs of which
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
    ReadOutcome.of(chain.get("https://www.metacritic.com/movie/nope")).toOptionOrThrow shouldBe None
  }

  // ...while a genuine outage still reads as one, so a dead upstream can never be
  // mistaken for "this film has no page" -- the distinction ReadOutcome exists for.
  it should "still report a composite failure when the last backend did not answer" in {
    val chain = new FallbackHttpFetch(Seq(
      "fixtures" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new java.io.FileNotFoundException("no fixture")
      },
      "live" -> new GetOnlyHttpFetch {
        override def get(url: String): String = throw new HttpStatusException(503, "GET", url, None)
      }))

    a [RuntimeException] should be thrownBy
      ReadOutcome.of(chain.get("https://www.metacritic.com/movie/nope")).toOptionOrThrow
  }

  // ---- A URL failing the same way on every ask is warned ONCE, not once per ask ----
  //
  // Record-scrape-fixtures UK leg: Cineworld's details API 403s CI runners, the 403 is
  // remembered so no network is spent, yet each of 136 URLs asked ~1,300 times logged its
  // own nine-line warning: 12,397 warnings, ~110k of the leg's 382k lines.

  private def status(code: Int): HttpFetch =
    new FailingHttpFetch((method, url) => new HttpStatusException(code, method, url, None))

  private def warningsFor(events: Seq[ILoggingEvent], url: String): Seq[String] =
    events.filter(e => e.getLevel == Level.WARN && e.getFormattedMessage.contains(url)).map(_.getFormattedMessage)

  private def askFailing(chain: HttpFetch, url: String, times: Int): Unit =
    (1 to times).foreach(_ => a [RuntimeException] should be thrownBy chain.get(url))

  it should "warn once for a failure repeated identically on every ask, still throwing it every time" in {
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> status(403)))
    val url   = uniqueUrl("repeated")

    val (_, events) = capture {
      (1 to 50).foreach { _ =>
        val thrown = the [RuntimeException] thrownBy chain.get(url)
        thrown.getMessage should include ("HTTP 403")
        thrown.getMessage should include ("no fixture file")
      }
    }

    warningsFor(events, url) should have size 1
  }

  it should "warn again when the same URL fails DIFFERENTLY, naming the repeats it held back" in {
    var code  = 403
    val live  = new FailingHttpFetch((method, url) => new HttpStatusException(code, method, url, None))
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> live))
    val url   = uniqueUrl("changed")

    val (_, events) = capture {
      askFailing(chain, url, 5)
      code = 503
      askFailing(chain, url, 5)
    }

    val warnings = warningsFor(events, url)
    warnings should have size 2
    warnings(0) should include ("HTTP 403")
    warnings(1) should include ("HTTP 503")
    warnings(1) should include ("4 unlogged repeat(s)")
  }

  it should "log a recovery, and warn afresh if the URL then fails again" in {
    var up    = false
    val live  = new GetOnlyHttpFetch {
      override def get(url: String): String = if (up) "answer" else throw new HttpStatusException(403, "GET", url, None)
    }
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> live))
    val url   = uniqueUrl("recovered")

    val (_, events) = capture {
      askFailing(chain, url, 3)
      up = true
      chain.get(url) shouldBe "answer"
      up = false
      askFailing(chain, url, 3)
    }

    warningsFor(events, url) should have size 2
    events.filter(e => e.getLevel == Level.INFO && e.getFormattedMessage.contains(url))
      .map(_.getFormattedMessage).mkString should include ("answered after its logged failure (2 unlogged repeat(s))")
  }

  it should "re-warn a persisting failure once per window, with its count" in {
    var clock = java.time.Instant.parse("2026-10-02T10:00:00Z")
    val chain = new FallbackHttpFetch(Seq("fixtures" -> failing("no fixture file"), "live" -> status(403)),
      repeats = RepeatedFailureLog.Settings(relogEvery = java.time.Duration.ofHours(1), now = () => clock))
    val url   = uniqueUrl("window")

    val (_, events) = capture {
      askFailing(chain, url, 10)
      clock = clock.plusSeconds(3600)
      askFailing(chain, url, 10)
    }

    val warnings = warningsFor(events, url)
    warnings should have size 2
    warnings(1) should include ("9 identical repeat(s) since last logged")
  }

  it should "track at most maxEntries URLs, warning an evicted one afresh" in {
    val log   = new RepeatedFailureLog(play.api.Logger(classOf[FallbackHttpFetch]), RepeatedFailureLog.Settings(maxEntries = 2))
    val urls  = (1 to 3).map(i => uniqueUrl(s"bounded-$i"))

    val events = LogCapture.capture(classOf[FallbackHttpFetch].getName, Some(Level.TRACE)) {
      (1 to 2).foreach(_ => urls.foreach(u => log.failed(u, s"failed $u")))
    }

    log.tracked shouldBe 2
    // Cycling three URLs through two slots evicts each before it repeats, so every ask warns.
    urls.foreach(u => warningsFor(events, u) should have size 2)
  }
}
