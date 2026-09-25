package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch, HttpOutcome, HttpOutcomeRecorder, HttpStatusException}

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.Optional
import scala.collection.mutable
import services.cinemas.common.ZyteFallback

/**
 * `ZyteFallback.fetchFor` composes the proxy chain at the composition root.
 * The key is injected (not read from the ambient env) so both branches are
 * deterministic regardless of whether CI has `ZYTE_API_KEY` set.
 *
 *   - no key → the chain collapses to `direct` (local dev, fixture replay).
 *   - key set → Zyte fronts `direct` in a `FallbackHttpFetch`.
 */
class ZyteFallbackSpec extends AnyFlatSpec with Matchers {

  private val direct: HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = "direct-body"
  }

  /** A JDK client that refuses every call and counts it — standing in for the Zyte API. */
  private class RefusingHttpClient extends HttpClient {
    val sends = new java.util.concurrent.atomic.AtomicInteger(0)
    override def send[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T]): HttpResponse[T] = {
      sends.incrementAndGet(); throw new java.io.IOException("refused by the spec's client")
    }
    override def sendAsync[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T]) = ???
    override def sendAsync[T](request: HttpRequest, handler: HttpResponse.BodyHandler[T],
                              push: HttpResponse.PushPromiseHandler[T]) = ???
    override def cookieHandler()   = Optional.empty()
    override def connectTimeout()  = Optional.empty()
    override def followRedirects() = HttpClient.Redirect.NEVER
    override def proxy()           = Optional.empty()
    override def sslContext()      = ???
    override def sslParameters()   = ???
    override def authenticator()   = Optional.empty()
    override def version()         = HttpClient.Version.HTTP_1_1
    override def executor()        = Optional.empty()
  }

  private def unbuilt: HttpClient = fail("the Zyte client was built for a chain with no Zyte leg")

  "fetchFor without a Zyte key" should "return direct unchanged — no proxy in front" in {
    ZyteFallback.fetchFor(direct, unbuilt, apiKey = None) should be theSameInstanceAs direct
  }

  it should "treat a blank key as no key" in {
    ZyteFallback.fetchFor(direct, unbuilt, apiKey = Some("")) should be theSameInstanceAs direct
  }

  "fetchFor with a Zyte key" should "front direct with a Zyte fallback chain" in {
    ZyteFallback.fetchFor(direct, new RefusingHttpClient, apiKey = Some("test-key")) shouldBe a[FallbackHttpFetch]
  }

  it should "call the Zyte API through the client it was handed" in {
    val client = new RefusingHttpClient
    val chain  = ZyteFallback.fetchFor(direct, client, apiKey = Some("test-key"))
    chain.get("https://www.biletyna.pl/a") shouldBe "direct-body"
    chain.get("https://www.biletyna.pl/b") shouldBe "direct-body"
    client.sends.get() shouldBe 2
  }

  // Odeon's Zyte fallback paid for a 401 on every request for as long as its
  // Authorization header was being dropped, and nothing counted it: the Zyte leg
  // was metered nowhere. The meter sees every Zyte attempt — and ONLY Zyte's, not
  // the free direct leg behind it.
  "the Zyte chain" should "meter each Zyte attempt's outcome, and not the direct leg's" in {
    val outcomes = mutable.ListBuffer.empty[String]
    val meter: HttpOutcomeRecorder = (outcome: String) => outcomes += outcome
    val rejecting = new GetOnlyHttpFetch {
      override def get(url: String): String = throw new HttpStatusException(401, "GET", url, None)
    }
    val chain = ZyteFallback.chain(Some(rejecting), direct, meter)

    chain.get("https://vwc.odeon.co.uk/a") shouldBe "direct-body"
    chain.get("https://vwc.odeon.co.uk/b") shouldBe "direct-body"
    outcomes.toList shouldBe List(HttpOutcome.Http401, HttpOutcome.Http401)
  }

  it should "record a success when Zyte answers" in {
    val outcomes = mutable.ListBuffer.empty[String]
    val zyte = new GetOnlyHttpFetch { override def get(url: String): String = "zyte-body" }
    ZyteFallback.chain(Some(zyte), direct, (o: String) => outcomes += o).get("https://x/") shouldBe "zyte-body"
    outcomes.toList shouldBe List(HttpOutcome.Success)
  }

  // The inner half of Odeon's chain: an origin 404 relayed by Zyte must not be buried
  // under the direct leg's Cloudflare 403 (see EgressWiringSpec's proxyPrimary case).
  it should "end on the origin's not-found relayed by Zyte rather than trying the blocked direct leg" in {
    var directCalls = 0
    val blockedDirect = new GetOnlyHttpFetch {
      override def get(url: String): String = { directCalls += 1; throw new HttpStatusException(403, "GET", url, None) }
    }
    val zyte = new GetOnlyHttpFetch {
      override def get(url: String): String =
        throw new services.cinemas.common.ZyteOriginStatusException(404, url, s"Zyte API call returned upstream status=404 for $url")
    }
    val thrown = the[HttpStatusException] thrownBy
      ZyteFallback.chain(Some(zyte), blockedDirect, HttpOutcomeRecorder.noop).get("https://vwc.odeon.co.uk/d")
    thrown.code shouldBe 404
    directCalls shouldBe 0
  }
}
