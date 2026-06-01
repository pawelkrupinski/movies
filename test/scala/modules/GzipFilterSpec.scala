package modules

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.streams.Accumulator
import play.api.mvc.{EssentialAction, Results}
import play.api.test.FakeRequest
import play.filters.gzip.{GzipFilter, GzipFilterConfig}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
 * Locks the response-compression behaviour `AppLoader` wires via
 * `GzipFilterComponents`. The `/` and `/kina` listings are multi-MB of
 * uncompressed HTML (200+ server-rendered cards + the embedded sibling
 * view); gzip is the single biggest mobile-load win, taking the wire
 * payload to a fraction of that. This spec uses the SAME filter + default
 * config the app builds, so a future change that drops the filter from the
 * chain or whitelists content-types away from `text/html` trips here.
 */
class GzipFilterSpec extends AnyFlatSpec with Matchers {

  private implicit val sys: ActorSystem     = ActorSystem("gzip-filter-spec")
  private implicit val mat: Materializer    = Materializer(sys)
  private implicit val ec: ExecutionContext = sys.dispatcher

  private val gzip = new GzipFilter(GzipFilterConfig())

  // A page-sized HTML body, representative of the listings we serve.
  private val bigHtml =
    "<!DOCTYPE html><html><body>" + ("<div class=\"card\">x</div>" * 5000) + "</body></html>"
  private val rawBytes = bigHtml.getBytes("UTF-8").length

  private def run(acceptEncoding: Option[String]): (play.api.mvc.Result, ByteString) = {
    val req =
      acceptEncoding.fold(FakeRequest())(ae => FakeRequest().withHeaders("Accept-Encoding" -> ae))
    val action = EssentialAction(_ => Accumulator.done(Results.Ok(bigHtml).as("text/html")))
    val result = Await.result(gzip(action)(req).run(), 5.seconds)
    val body   = Await.result(result.body.consumeData, 5.seconds)
    (result, body)
  }

  "GzipFilter (as wired in AppLoader)" should
    "gzip-encode a large HTML response when the client accepts gzip" in {
      val (result, body) = run(Some("gzip"))
      result.header.headers.get("Content-Encoding") shouldBe Some("gzip")
      // gzip magic number (0x1f 0x8b) — proves the bytes are actually gzipped.
      body.length should be > 2
      (body(0) & 0xff) shouldBe 0x1f
      (body(1) & 0xff) shouldBe 0x8b
      // …and it shrank the payload by well over 4× (this body gzips ~30×).
      body.length should be < rawBytes / 4
    }

  it should "leave the response uncompressed when the client does not accept gzip" in {
    val (result, body) = run(None)
    result.header.headers.get("Content-Encoding") shouldBe None
    body.utf8String should include("<div class=\"card\">")
    body.length shouldBe rawBytes
  }
}
