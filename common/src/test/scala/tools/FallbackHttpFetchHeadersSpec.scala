package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/**
 * A fallback chain must carry the caller's REQUEST HEADERS to whichever backend
 * serves — the header overload is where per-call auth lives.
 *
 * `HttpFetch.get(url, headers)` defaults to `get(url)`, which exists so header-blind
 * test fakes need only implement one method. Inherited by a DELEGATING wrapper that
 * default is a silent data loss: the wrapper answers the header call by re-entering
 * its own header-less `get`, and the headers never reach the leaf.
 *
 * That is exactly what took Odeon down on 2026-08-29. Its Vista ocapi authenticates
 * with `Authorization: Bearer <jwt>`; routing it through the residential proxy put a
 * `FallbackHttpFetch` on the path, the bearer was dropped, the origin answered 401,
 * the JVM's proxy Authenticator turned that into `IOException: WWW-Authenticate
 * header missing`, and the chain fell through to the direct leg — which 403'd on the
 * very Cloudflare block the proxy was added to clear. Every other proxied source
 * authenticates by cookie or not at all, so nothing had exercised this path before.
 */
class FallbackHttpFetchHeadersSpec extends AnyFlatSpec with Matchers {

  /** Leaf that records the headers each call actually arrived with, and can be told
   *  to fail so the next backend in the chain is tried. */
  private class RecordingFetch(name: String, fail: Boolean = false) extends GetOnlyHttpFetch {
    val seen: mutable.Buffer[Map[String, String]] = mutable.Buffer.empty
    override def get(url: String): String = get(url, Map.empty)
    override def get(url: String, headers: Map[String, String]): String = {
      seen += headers
      if (fail) throw new RuntimeException(s"$name is down") else name
    }
  }

  private val Url    = "https://vwc.odeon.co.uk/WSVistaWebClient/ocapi/v1/film-screening-dates?siteIds=040"
  private val Bearer = Map("Authorization" -> "Bearer jwt-token")

  "FallbackHttpFetch" should "carry request headers to the backend that serves" in {
    val primary = new RecordingFetch("proxy")
    new FallbackHttpFetch(Seq("proxy" -> primary, "direct" -> new RecordingFetch("direct")))
      .get(Url, Bearer) shouldBe "proxy"
    primary.seen shouldBe Seq(Bearer)
  }

  // The failure mode in prod: the proxy leg threw, and the direct leg it rolled to
  // was ALSO called without the bearer. Both legs must see the headers.
  it should "carry request headers to a fallback backend after the primary fails" in {
    val direct = new RecordingFetch("direct")
    new FallbackHttpFetch(Seq("proxy" -> new RecordingFetch("proxy", fail = true), "direct" -> direct))
      .get(Url, Bearer) shouldBe "direct"
    direct.seen shouldBe Seq(Bearer)
  }
}
