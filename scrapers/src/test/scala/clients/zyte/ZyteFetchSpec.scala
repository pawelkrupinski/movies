package clients.zyte

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{ZyteClient, ZyteFetch}

import java.net.http.HttpClient

/**
 * Pins how `ZyteFetch` maps the `HttpFetch` calls onto `ZyteClient`. A
 * recording client captures the calls without touching the network (the
 * overrides never reach `httpClient`).
 */
class ZyteFetchSpec extends AnyFlatSpec with Matchers {

  private class RecordingZyteClient extends ZyteClient(HttpClient.newHttpClient(), settings.ZyteApiKey("k")) {
    var gets:     List[String]                       = Nil
    var headed:   List[(String, Map[String, String])] = Nil // (targetUrl, headers)
    var byteGets: List[String]                       = Nil
    override def get(url: String): String = { gets ::= url; "BODY" }
    override def get(url: String, headers: Map[String, String]): String = { headed ::= (url -> headers); "BODY" }
    override def getBytes(url: String, headers: Map[String, String]): Array[Byte] = { byteGets ::= url; Array[Byte](0xB1.toByte) }
  }

  "ZyteFetch" should "do a single get for a page" in {
    val client = new RecordingZyteClient
    new ZyteFetch(client).get("https://bilety.ck105.koszalin.pl/MSI/mvc/pl") shouldBe "BODY"
    client.gets shouldBe List("https://bilety.ck105.koszalin.pl/MSI/mvc/pl")
  }

  // Inheriting HttpFetch's default `get(url, headers) = get(url)` sent a
  // header-authenticated request (Odeon's `Authorization: Bearer`) out without
  // its header — a billed Zyte request guaranteed to come back 401.
  it should "carry the caller's request headers through to Zyte" in {
    val client = new RecordingZyteClient
    val url    = "https://bilety.ck105.koszalin.pl/MSI/mvc/pl"
    new ZyteFetch(client).get(url, Map("Authorization" -> "Bearer t0k"))

    client.headed shouldBe List(url -> Map("Authorization" -> "Bearer t0k"))
    client.gets shouldBe empty
  }

  // A legacy single-byte page must reach its parser as the bytes Zyte returned:
  // the inherited `get(url).getBytes(UTF_8)` had already decoded them as UTF-8.
  it should "fetch raw bytes without a UTF-8 round-trip" in {
    val client = new RecordingZyteClient
    new ZyteFetch(client).getBytes("https://kino.example.pl/repertuar") shouldBe Array[Byte](0xB1.toByte)
    client.byteGets shouldBe List("https://kino.example.pl/repertuar")
  }
}
