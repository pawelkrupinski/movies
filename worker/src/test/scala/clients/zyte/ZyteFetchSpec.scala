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

  private class RecordingZyteClient extends ZyteClient(HttpClient.newHttpClient(), settings.ZyteApiKey("k")) {
    var gets:    List[String]           = Nil
    var warms:   List[(String, String)] = Nil // (cookieSourceUrl, sessionId)
    var fetches: List[(String, String)] = Nil // (targetUrl,      sessionId)
    var headed:  List[(String, Map[String, String])] = Nil // (targetUrl, headers)
    override def get(url: String): String = { gets ::= url; "BODY" }
    override def get(url: String, headers: Map[String, String]): String = { headed ::= (url -> headers); "BODY" }
    var byteGets: List[String] = Nil
    override def getBytes(url: String, headers: Map[String, String]): Array[Byte] = { byteGets ::= url; Array[Byte](0xB1.toByte) }
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

  // Odeon's ocapi authenticates with `Authorization: Bearer`, and `odeonFetch`
  // falls back to Zyte when the residential proxy is down. Inheriting
  // HttpFetch's default `get(url, headers) = get(url)` sent that fallback
  // unauthenticated — a billed Zyte request guaranteed to come back 401.
  "ZyteFetch with no cookie source" should "carry the caller's request headers through to Zyte" in {
    val client = new RecordingZyteClient
    val url    = "https://vwc.odeon.co.uk/WSVistaWebClient/ocapi/v1/sites/1/showtimes"
    new ZyteFetch(client, None).get(url, Map("Authorization" -> "Bearer t0k"))

    client.headed shouldBe List(url -> Map("Authorization" -> "Bearer t0k"))
    client.gets shouldBe empty
  }

  "ZyteFetch with a cookie source" should "refuse headers it cannot carry rather than silently drop them" in {
    val client = new RecordingZyteClient
    an [UnsupportedOperationException] should be thrownBy
      new ZyteFetch(client, Some("https://www.multikino.pl/")).get("https://www.multikino.pl/api/x", Map("Authorization" -> "Bearer t0k"))
    client.fetches shouldBe empty
  }

  // A legacy single-byte page must reach its parser as the bytes Zyte returned:
  // the inherited `get(url).getBytes(UTF_8)` had already decoded them as UTF-8.
  "ZyteFetch with no cookie source" should "fetch raw bytes without a UTF-8 round-trip" in {
    val client = new RecordingZyteClient
    new ZyteFetch(client, None).getBytes("https://kino.example.pl/repertuar") shouldBe Array[Byte](0xB1.toByte)
    client.byteGets shouldBe List("https://kino.example.pl/repertuar")
  }
}
