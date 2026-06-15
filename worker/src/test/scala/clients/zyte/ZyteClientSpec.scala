package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ZyteClient

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

  it should "throw when the body is missing even on a 2xx status" in {
    val json = """{"statusCode":200,"browserHtml":"<html></html>"}"""
    the[RuntimeException] thrownBy ZyteClient.bodyOrThrow(json, "x") should have message
      "Zyte response missing httpResponseBody for x"
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
