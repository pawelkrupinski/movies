package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream
import scala.concurrent.Future

/**
 * A body fixed for the life of the process, served under its content ETag: a 304
 * to any `If-None-Match` that offers the tag however it is spelled, otherwise the
 * body — gzipped for a client that takes it — with the caller's `Cache-Control`.
 */
class FixedBodySpec extends AnyFlatSpec with Matchers {

  private val text = """{"hello":"świat"}"""
  private val body = FixedBody.json(text)
  private def serve(headers: (String, String)*) = Future.successful(body.serve(FakeRequest().withHeaders(headers*), "no-cache"))

  private def gunzip(bytes: Array[Byte]): String =
    new String(new GZIPInputStream(new ByteArrayInputStream(bytes)).readAllBytes(), StandardCharsets.UTF_8)

  "a FixedBody" should "name its content with a quoted 16-hex ETag, and its URL version without the quotes" in {
    body.etag shouldBe tools.Digest.contentEtag(text)
    body.etag should fullyMatch regex "\"[0-9a-f]{16}\""
    body.version shouldBe body.etag.stripPrefix("\"").stripSuffix("\"")
    FixedBody.json(text + " ").etag should not be body.etag
  }

  it should "serve the plain body, with its validators, to a client that refuses gzip" in {
    val result = serve()
    status(result) shouldBe OK
    contentType(result) shouldBe Some("application/json")
    contentAsString(result) shouldBe text
    header("ETag", result) shouldBe Some(body.etag)
    header("Cache-Control", result) shouldBe Some("no-cache")
    header("Vary", result) shouldBe Some("Accept-Encoding")
    header("Content-Encoding", result) shouldBe None
  }

  it should "serve the gzipped copy, declaring its encoding, to a client that takes gzip" in {
    val result = serve("Accept-Encoding" -> "gzip, deflate, br")
    status(result) shouldBe OK
    header("Content-Encoding", result) shouldBe Some("gzip")
    header("Vary", result) shouldBe Some("Accept-Encoding")
    gunzip(contentAsBytes(result).toArray) shouldBe text
  }

  it should "pass the caller's Cache-Control through" in {
    header("Cache-Control", Future.successful(body.serve(FakeRequest(), "public, max-age=31536000, immutable"))) shouldBe
      Some("public, max-age=31536000, immutable")
  }

  it should "answer 304, bodiless and with the 200's validators, to its own tag" in {
    val result = serve("If-None-Match" -> body.etag)
    status(result) shouldBe NOT_MODIFIED
    contentAsString(result) shouldBe ""
    header("ETag", result) shouldBe Some(body.etag)
    header("Cache-Control", result) shouldBe Some("no-cache")
    header("Vary", result) shouldBe Some("Accept-Encoding")
  }

  it should "answer 304 to its tag offered weak, or in a list" in {
    status(serve("If-None-Match" -> s"W/${body.etag}")) shouldBe NOT_MODIFIED
    status(serve("If-None-Match" -> s""""0000000000000000", ${body.etag}""")) shouldBe NOT_MODIFIED
    status(serve("If-None-Match" -> "*")) shouldBe NOT_MODIFIED
  }

  it should "serve the body again to a stale tag" in {
    status(serve("If-None-Match" -> "\"0000000000000000\"")) shouldBe OK
  }
}
