package clients.zyte

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{DetailFetchOutcome, ZyteClient}
import tools.{HttpOutcome, HttpStatusException}

import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Unit tests for ZyteClient's pure response-parsing primitives. The
 * over-the-wire `warm` / `fetchWithSession` calls are exercised against a
 * real key out of band; here we just pin the bits that decode the response —
 * so a future Zyte API change (different field name, body not base64) breaks
 * here loudly rather than silently emptying the Multikino cinema slot.
 */
class ZyteClientSpec extends AnyFlatSpec with Matchers {

  "extractStatus" should "read the upstream HTTP status from a Zyte response" in {
    val json =
      """{"url":"https://example.com","statusCode":200,"httpResponseBody":""}"""
    ZyteClient.extractStatus(json) shouldBe 200
  }

  it should "carry through a non-2xx upstream status — the caller decides what to do" in {
    val json = """{"url":"https://example.com","statusCode":401,"httpResponseBody":""}"""
    ZyteClient.extractStatus(json) shouldBe 401
  }

  it should "return -1 when the field is missing — treated as error by callers" in {
    ZyteClient.extractStatus("""{"url":"x"}""") shouldBe -1
  }

  "extractBody" should "base64-decode the httpResponseBody field as UTF-8" in {
    val payload = """{"hello":"świat"}"""  // non-ASCII to prove UTF-8
    val b64     = Base64.getEncoder.encodeToString(payload.getBytes(StandardCharsets.UTF_8))
    val json    = s"""{"statusCode":200,"httpResponseBody":"$b64"}"""
    ZyteClient.extractBody(json) shouldBe Some(payload)
  }

  it should "return None when httpResponseBody is absent (e.g. browserHtml-only response)" in {
    ZyteClient.extractBody("""{"statusCode":200,"browserHtml":"<html></html>"}""") shouldBe empty
  }

  "bodyOrThrow" should "return the decoded upstream body on a 2xx status" in {
    val payload = """{"hello":"świat"}"""
    val b64     = Base64.getEncoder.encodeToString(payload.getBytes(StandardCharsets.UTF_8))
    val json    = s"""{"statusCode":200,"httpResponseBody":"$b64"}"""
    ZyteClient.bodyOrThrow(json, "https://example.com") shouldBe payload
  }

  it should "throw naming the non-2xx upstream status (the biletyna 403 case)" in {
    val json = """{"statusCode":403,"httpResponseBody":""}"""
    val exception   = the[RuntimeException] thrownBy ZyteClient.bodyOrThrow(json, "https://biletyna.pl/x")
    exception.getMessage should include("403")
    exception.getMessage should include("https://biletyna.pl/x")
  }

  // The paid-egress counter classifies what the Zyte leg throws. A bare
  // RuntimeException classified as `other`, so Odeon's Zyte fallback paying for
  // a 401 on every request (the Authorization header dropped) was
  // indistinguishable from any other miss.
  it should "throw an exception carrying the upstream status, so the paid-egress counter can class it" in {
    val json      = """{"statusCode":401,"httpResponseBody":""}"""
    val exception = the[RuntimeException] thrownBy ZyteClient.bodyOrThrow(json, "https://vwc.odeon.co.uk/x")
    HttpOutcome.classify(exception) shouldBe HttpOutcome.Http401
    exception.getMessage shouldBe "Zyte API call returned upstream status=401 for https://vwc.odeon.co.uk/x"
  }

  // An ORIGIN status relayed by Zyte is the origin's answer: a 404/410 via Zyte
  // is as durable as one fetched directly, so a detail cache stamps it gone.
  it should "surface an origin 404 relayed by Zyte as the origin's own status" in {
    val json    = """{"statusCode":404,"httpResponseBody":""}"""
    val failure = the[HttpStatusException] thrownBy ZyteClient.bodyOrThrow(json, "https://biletyna.pl/gone")
    failure.code shouldBe 404
    a[HttpStatusException] should be thrownBy DetailFetchOutcome.transientToNone(ZyteClient.bodyOrThrow(json, "https://biletyna.pl/gone"))
  }

  // Zyte naming no origin status at all is Zyte's failure, not an origin answer.
  it should "not dress a missing origin status up as an HTTP status" in {
    val failure = the[RuntimeException] thrownBy ZyteClient.bodyOrThrow("""{"httpResponseBody":""}""", "https://x.pl/")
    failure should not be a[HttpStatusException]
  }

  // Zyte's OWN status (its API answering non-200) says nothing about the origin.
  // As an HttpStatusException, Zyte's 429 opened the origin host's breaker and
  // throttled it, Zyte's 403 counted as the origin blocking us, and a Zyte 404/410
  // got a detail page stamped gone for good.
  "apiBodyOrThrow" should "return Zyte's JSON on 200" in {
    ZyteClient.apiBodyOrThrow(200, "{}", "https://x.pl/") shouldBe "{}"
  }

  it should "throw Zyte's own failures as the provider's, never as the origin's status" in {
    Seq(401 -> HttpOutcome.Http401, 403 -> HttpOutcome.Http403, 404 -> HttpOutcome.Http404,
        429 -> HttpOutcome.Http429, 520 -> HttpOutcome.Http5xx).foreach { case (status, outcome) =>
      val failure = the[RuntimeException] thrownBy ZyteClient.apiBodyOrThrow(status, "ban", "https://x.pl/")
      withClue(s"Zyte $status: ") {
        failure should not be a[HttpStatusException]
        failure.getMessage shouldBe s"Zyte http=$status for https://x.pl/: ban"
        // Still classed by Zyte's status on the paid-egress counter.
        HttpOutcome.classify(failure) shouldBe outcome
        // A transient for the detail fetch: retried, never stamped gone.
        DetailFetchOutcome.transientToNone(ZyteClient.apiBodyOrThrow(status, "ban", "https://x.pl/")) shouldBe None
      }
    }
  }

  "bodyOrThrow" should "throw when the body is missing even on a 2xx status" in {
    val json = """{"statusCode":200,"browserHtml":"<html></html>"}"""
    the[RuntimeException] thrownBy ZyteClient.bodyOrThrow(json, "x") should have message
      "Zyte response missing httpResponseBody for x"
  }

  "requestBody" should "OMIT the session field on the stateless get path (None)" in {
    // Regression: a stray per-call session id pinned a sticky Zyte egress IP that
    // bilety.ck105.koszalin.pl banned (520 /download/website-ban) → Kino Kryterium
    // showed a permanent white /uptime bar. The cookie-less get path must send no
    // session so Zyte picks a fresh IP each call.
    val body = ZyteClient.requestBody("https://bilety.ck105.koszalin.pl/MSI/mvc/pl", None)
    body should include(""""url":"https://bilety.ck105.koszalin.pl/MSI/mvc/pl"""")
    body should include(""""httpResponseBody":true""")
    body should not include "session"
  }

  it should "INCLUDE the session id on the cookie-carryover path (Some) — Multikino needs it" in {
    val body = ZyteClient.requestBody("https://multikino.pl/api/x", Some("sess-123"))
    body should include(""""session":{"id":"sess-123"}""")
  }

  it should "pass request headers as Zyte's customHttpRequestHeaders, and omit the field when there are none" in {
    val body = play.api.libs.json.Json.parse(
      ZyteClient.requestBody("https://vwc.odeon.co.uk/x", None, Map("Authorization" -> "Bearer t0k")))
    (body \ "customHttpRequestHeaders").as[Seq[Map[String, String]]] shouldBe
      Seq(Map("name" -> "Authorization", "value" -> "Bearer t0k"))
    ZyteClient.requestBody("https://vwc.odeon.co.uk/x", None) should not include "customHttpRequestHeaders"
  }

  "bodyBytesOrThrow" should "return the upstream bytes exactly, not a UTF-8 round-trip of them" in {
    // "ą" in ISO-8859-2 is the single byte 0xB1 — invalid as UTF-8, so decoding
    // to a String and re-encoding would turn it into U+FFFD's three bytes.
    val raw  = Array[Byte]('K'.toByte, 0xB1.toByte)
    val json = s"""{"statusCode":200,"httpResponseBody":"${java.util.Base64.getEncoder.encodeToString(raw)}"}"""
    ZyteClient.bodyBytesOrThrow(json, "https://example.pl/") shouldBe raw
  }

  "basicAuth" should "format Authorization as 'Basic <b64(key:)>' per Zyte's auth spec" in {
    // Zyte uses Basic auth with the API key as username and an empty
    // password — verify the encoding shape so a refactor can't quietly
    // start sending `key` without the trailing colon.
    val header  = ZyteClient.basicAuth("test-key-123")
    val encoded = header.stripPrefix("Basic ")
    val decoded = new String(Base64.getDecoder.decode(encoded), StandardCharsets.UTF_8)
    decoded shouldBe "test-key-123:"
  }
}
