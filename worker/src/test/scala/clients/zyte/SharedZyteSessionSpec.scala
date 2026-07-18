package clients.zyte

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{SharedZyteSession, ZyteClient}

import java.net.http.HttpClient
import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import scala.concurrent.duration._

/**
 * `SharedZyteSession` warms one Zyte session and reuses it across fetches to
 * cut Multikino's per-scrape warm-up cost, while staying correct if the session
 * dies (re-warm + retry) — proven here with a recording client and a clock the
 * test advances, no network.
 */
class SharedZyteSessionSpec extends AnyFlatSpec with Matchers {

  private val Home = "https://www.multikino.pl/"

  /** Clock the test advances by hand. */
  private class MutableClock(var nowMillis: Long) extends Clock {
    override def getZone: ZoneId            = ZoneOffset.UTC
    override def withZone(z: ZoneId): Clock = this
    override def instant: Instant           = Instant.ofEpochMilli(nowMillis)
    override def millis(): Long             = nowMillis
  }

  /** Records warm/fetch calls; `failFetchOnce` makes the first fetch throw to
   *  exercise the session-died path. */
  private class RecordingClient(failFetchOnce: Boolean = false)
      extends ZyteClient(HttpClient.newHttpClient(), "k") {
    var warms:   List[String] = Nil // sessionIds warmed
    var fetches: List[String] = Nil // sessionIds fetched under
    private var failsLeft = if (failFetchOnce) 1 else 0
    override def warm(cookieSourceUrl: String, sessionId: String): Unit =
      warms = warms :+ sessionId
    override def fetchWithSession(targetUrl: String, sessionId: String): String = {
      fetches = fetches :+ sessionId
      if (failsLeft > 0) { failsLeft -= 1; throw new RuntimeException("HTTP 401 — session expired") }
      "BODY"
    }
  }

  "SharedZyteSession" should "warm once and reuse the session across many fetches within the ttl" in {
    val client  = new RecordingClient
    val session = new SharedZyteSession(client, Home, 8.minutes, new MutableClock(0L))

    (1 to 5).foreach(i => session.get(s"$Home/api/$i") shouldBe "BODY")

    client.warms should have size 1
    client.fetches should have size 5
    client.fetches.distinct shouldBe List(client.warms.head) // all under the one warmed session
  }

  it should "re-warm a fresh session once the ttl has elapsed" in {
    val clock   = new MutableClock(0L)
    val client  = new RecordingClient
    val session = new SharedZyteSession(client, Home, 8.minutes, clock)

    session.get(s"$Home/api/a")
    clock.nowMillis += (9.minutes).toMillis // past ttl
    session.get(s"$Home/api/b")

    client.warms should have size 2
    client.warms.distinct should have size 2 // two distinct session ids
  }

  it should "drop the session, re-warm, and retry once when a fetch fails (session died upstream)" in {
    val client  = new RecordingClient(failFetchOnce = true)
    val session = new SharedZyteSession(client, Home, 8.minutes, new MutableClock(0L))

    session.get(s"$Home/api/a") shouldBe "BODY" // recovered

    client.warms should have size 2   // initial warm + re-warm after the 401
    client.fetches should have size 2 // failed attempt + successful retry
    client.fetches.head should not be client.fetches(1) // retried under the fresh session
  }

  it should "propagate a warm failure so the surrounding FallbackHttpFetch can roll to direct" in {
    val client = new ZyteClient(HttpClient.newHttpClient(), "k") {
      override def warm(cookieSourceUrl: String, sessionId: String): Unit =
        throw new RuntimeException("Zyte warm-up returned upstream status=403")
      override def fetchWithSession(targetUrl: String, sessionId: String): String = "BODY"
    }
    val session = new SharedZyteSession(client, Home, 8.minutes, new MutableClock(0L))

    a[RuntimeException] should be thrownBy session.get(s"$Home/api/a")
  }
}
