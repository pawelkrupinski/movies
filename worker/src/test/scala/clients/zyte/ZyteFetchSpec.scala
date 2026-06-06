package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{ZyteClient, ZyteFetch}

import java.net.http.HttpClient

/**
 * Pins which `ZyteClient` fetch shape `ZyteFetch` picks per `cookieSource`.
 * A recording client captures the call without touching the network (the
 * overrides never reach `httpClient`).
 *
 *   - `None` (biletyna) → a single `get`, no cookie warm-up.
 *   - `Some(homepage)` (Multikino) → `getWithCookies`, warming the session.
 */
class ZyteFetchSpec extends AnyFlatSpec with Matchers {

  private class RecordingZyteClient extends ZyteClient(HttpClient.newHttpClient(), "k") {
    var gets:    List[String]           = Nil
    var cookied: List[(String, String)] = Nil
    override def get(url: String): String = { gets ::= url; "BODY" }
    override def getWithCookies(target: String, cookieSource: String): String = {
      cookied ::= (target -> cookieSource); "BODY"
    }
  }

  "ZyteFetch with no cookie source" should "do a single get — no warm-up — for a stateless page" in {
    val client = new RecordingZyteClient
    val body   = new ZyteFetch(client, None).get("https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe")

    body shouldBe "BODY"
    client.gets shouldBe List("https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe")
    client.cookied shouldBe empty
  }

  "ZyteFetch with a cookie source" should "warm a session via getWithCookies" in {
    val client = new RecordingZyteClient
    new ZyteFetch(client, Some("https://www.multikino.pl/")).get("https://www.multikino.pl/api/x")

    client.cookied shouldBe List("https://www.multikino.pl/api/x" -> "https://www.multikino.pl/")
    client.gets shouldBe empty
  }
}
