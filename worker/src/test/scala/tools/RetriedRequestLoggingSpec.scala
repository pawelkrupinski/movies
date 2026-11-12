package tools

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.spi.ILoggingEvent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicInteger

/**
 * What a request asked again on the spot ([[RetryingHttpFetch]] over the wire, [[RealHttpFetch]]) SAYS: the call's
 * outcome, once, not every attempt. Each try of a failing request used to warn twice -- the leaf's "HTTP 500 from GET
 * …" and the retry's "attempt 1/3 failed" -- so one broken Iluzjon film page, asked three times every six hours, wrote
 * six warnings a scrape, and a request that recovered on its second try still warned twice (2026-10-29..11-12: ~600
 * retry lines and ~300 leaf lines a quiet day). A call that ends failed warns once, naming its attempts; a call that
 * recovered warns nothing; the attempts are kept at DEBUG.
 *
 * Talks to an in-process server on a free port: the leaf's own logging is what is asserted.
 */
class RetriedRequestLoggingSpec extends AnyFlatSpec with Matchers {

  /** `test` against a server that answers each request to a path with the next of `statuses` (the last repeated). */
  private def withServer(statuses: Int*)(test: String => Unit): Unit = {
    val server = com.sun.net.httpserver.HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    val asked  = new AtomicInteger(0)
    server.createContext("/", exchange => {
      val status = statuses(math.min(asked.getAndIncrement(), statuses.size - 1))
      val body   = "page".getBytes(java.nio.charset.StandardCharsets.UTF_8)
      exchange.sendResponseHeaders(status, body.length.toLong)
      exchange.getResponseBody.write(body)
      exchange.close()
    })
    server.start()
    try test(s"http://127.0.0.1:${server.getAddress.getPort}/film-${java.util.UUID.randomUUID()}")
    finally server.stop(0)
  }

  /** Every event the leaf and the retry log about `url` while `body` runs, DEBUG included. */
  private def logged(url: String)(body: => Unit): Seq[ILoggingEvent] = {
    var retry = Seq.empty[ILoggingEvent]
    val leaf = LogCapture.capture(classOf[RealHttpFetch].getName, Some(Level.DEBUG)) {
      retry = LogCapture.capture(RetryWithBackoff.getClass.getName.stripSuffix("$"), Some(Level.DEBUG))(body)
    }
    (leaf ++ retry).filter(_.getFormattedMessage.contains(url.stripPrefix("http://")))
  }

  private def warnings(events: Seq[ILoggingEvent]): Seq[String] =
    events.filter(_.getLevel.isGreaterOrEqual(Level.WARN)).map(_.getFormattedMessage)

  private def retrying = new RetryingHttpFetch(new RealHttpFetch(), sleep = _ => ())

  "a request failing every attempt" should "warn once, naming its attempts, and keep each attempt at DEBUG" in
    withServer(500) { url =>
      val events = logged(url)(an[HttpStatusException] should be thrownBy retrying.get(url))
      val warned = warnings(events)
      withClue(s"logged: ${events.map(e => s"${e.getLevel} ${e.getFormattedMessage}")}: ") {
        warned.size shouldBe 1
        warned.head should include(s"after ${RetryingHttpFetch.Attempts} attempts")
        warned.head should include("HTTP 500")
        events.count(_.getLevel == Level.DEBUG) should be >= RetryingHttpFetch.Attempts
      }
    }

  "a request that recovers on a retry" should "warn nothing" in
    withServer(500, 200) { url =>
      val events = logged(url)(retrying.get(url) shouldBe "page")
      withClue(s"logged: ${events.map(e => s"${e.getLevel} ${e.getFormattedMessage}")}: ") {
        warnings(events) shouldBe empty
        events should not be empty // the failed attempt is still told, at DEBUG
      }
    }

  "a request no retry layer reports" should "still warn its own server error" in
    withServer(500) { url =>
      val events = logged(url)(an[HttpStatusException] should be thrownBy new RealHttpFetch().get(url))
      warnings(events).size shouldBe 1
    }
}
