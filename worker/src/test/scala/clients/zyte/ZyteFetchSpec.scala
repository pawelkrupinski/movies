package clients.zyte

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{ZyteClient, ZyteFetch}

import java.net.http.HttpClient

/**
 * Pins which `ZyteClient` fetch shape `ZyteFetch` picks per `cookieSource`.
 * A recording client captures the calls without touching the network (the
 * overrides never reach `httpClient`).
 *
 *   - `None` (biletyna) → a single `get`, no cookie warm-up.
 *   - `Some(homepage)` (Multikino) → a `SharedZyteSession`: warm once, then
 *     `fetchWithSession`, reusing the session across fetches.
 */
class ZyteFetchSpec extends AnyFlatSpec with Matchers {

  private class RecordingZyteClient extends ZyteClient(HttpClient.newHttpClient(), "k") {
    var gets:    List[String]           = Nil
    var warms:   List[(String, String)] = Nil // (cookieSourceUrl, sessionId)
    var fetches: List[(String, String)] = Nil // (targetUrl,      sessionId)
    override def get(url: String): String = { gets ::= url; "BODY" }
    override def warm(cookieSourceUrl: String, sessionId: String): Unit =
      warms = warms :+ (cookieSourceUrl -> sessionId)
    override def fetchWithSession(targetUrl: String, sessionId: String): String =
      { fetches = fetches :+ (targetUrl -> sessionId); "BODY" }
  }

  "ZyteFetch with no cookie source" should "do a single get — no warm-up — for a stateless page" in {
    val client = new RecordingZyteClient
    val body   = new ZyteFetch(client, None).get("https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe")

    body shouldBe "BODY"
    client.gets shouldBe List("https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe")
    client.warms shouldBe empty
    client.fetches shouldBe empty
  }

  "ZyteFetch with a cookie source" should "warm a session then fetch under it, reusing the same id" in {
    val client = new RecordingZyteClient
    new ZyteFetch(client, Some("https://www.multikino.pl/")).get("https://www.multikino.pl/api/x")

    client.gets shouldBe empty
    client.warms.map(_._1) shouldBe List("https://www.multikino.pl/")
    client.fetches.map(_._1) shouldBe List("https://www.multikino.pl/api/x")
    // warm + fetch used the SAME session id — that's the cookie carryover.
    client.warms.head._2 shouldBe client.fetches.head._2
  }

  it should "share ONE warmed session across many fetches (the fleet-wide cost saving)" in {
    val client = new RecordingZyteClient
    val fetch  = new ZyteFetch(client, Some("https://www.multikino.pl/"))
    (1 to 5).foreach(i => fetch.get(s"https://www.multikino.pl/api/cinemas/000$i/films"))

    client.warms should have size 1       // warmed once, not five times
    client.fetches should have size 5     // every cinema still fetched
    client.fetches.map(_._2).distinct shouldBe List(client.warms.head._2) // all under that one session
  }
}
