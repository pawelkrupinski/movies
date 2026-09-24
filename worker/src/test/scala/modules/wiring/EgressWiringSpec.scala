package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools._

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/** `EgressWiring.breakerGuarded` is what stands between a Decodo-account-wide
 *  outage (every tunnel 503ing, 2026-09-10) and a worker task queue backlog:
 *  before it existed, the proxy leg was a bare `RealHttpFetch` with none of
 *  `HttpWiring`'s protective wrapping, so every venue call paid the full
 *  connect/request timeout on a dead tunnel before `FallbackHttpFetch` could
 *  even try the next leg. See `project_decodo_tunnel_503_cascade_2026_09_10`. */
class EgressWiringSpec extends AnyFlatSpec with Matchers {

  private class CountingFailingFetch(fail: String => Throwable) extends GetOnlyHttpFetch {
    val calls = new AtomicInteger(0)
    override def get(url: String): String = { calls.incrementAndGet(); throw fail(url) }
  }

  private def tunnelFailed(url: String) = new java.io.IOException("Tunnel failed, got: 503")

  "breakerGuarded" should "let the first few failures reach the delegate, then stop calling it" in {
    val deadProxy = new CountingFailingFetch(tunnelFailed)
    val guarded   = EgressWiring.breakerGuarded(deadProxy)

    // The default breaker opens after 4 consecutive trip-worthy failures
    // (HostCircuitBreakerHttpFetch's failureThreshold) — each of these reaches
    // the dead proxy and pays for it.
    (1 to 4).foreach(_ => an[java.io.IOException] should be thrownBy guarded.get("https://vwc.odeon.co.uk/x"))
    deadProxy.calls.get() shouldBe 4

    // Once open, further calls fail FAST without touching the delegate at
    // all — this is the whole point: a dead tunnel stops costing its own
    // timeout on every single venue call.
    a[CircuitOpenException] should be thrownBy guarded.get("https://vwc.odeon.co.uk/y")
    deadProxy.calls.get() shouldBe 4
  }

  it should "let FallbackHttpFetch reach the working leg immediately once the proxy leg is open" in {
    val deadProxy = new CountingFailingFetch(tunnelFailed)
    val working   = new AtomicInteger(0)
    val fallback  = new GetOnlyHttpFetch {
      override def get(url: String): String = { working.incrementAndGet(); "ok" }
    }
    val chain = new FallbackHttpFetch(Seq("proxy" -> EgressWiring.breakerGuarded(deadProxy), "fallback" -> fallback))

    // Trip the breaker.
    (1 to 4).foreach(_ => chain.get("https://vwc.odeon.co.uk/x") shouldBe "ok")
    deadProxy.calls.get() shouldBe 4
    working.get() shouldBe 4

    // Ten more calls: the proxy leg is open, so the dead backend is never
    // touched again — every one of these resolves via the fallback alone,
    // which is the throughput a worker thread gets back during the outage.
    (1 to 10).foreach(_ => chain.get("https://vwc.odeon.co.uk/z") shouldBe "ok")
    deadProxy.calls.get() shouldBe 4
    working.get() shouldBe 14
  }

  it should "keep the wrapped leg working normally when the proxy is healthy" in {
    val healthy = new GetOnlyHttpFetch {
      override def get(url: String): String = "ok"
    }
    val guarded = EgressWiring.breakerGuarded(healthy)
    (1 to 20).foreach(_ => guarded.get("https://vwc.odeon.co.uk/x") shouldBe "ok")
  }

  // The Decodo leg's paid-egress meter sits INSIDE the breaker: an attempt that
  // reached the proxy is counted with its outcome, a breaker fast-fail (~0ms, no
  // request made) is not — otherwise an open breaker would read as a proxy
  // failing at 100%.
  "meteredProxyLeg" should "count the attempts that reached the proxy, and not the breaker's fast-fails" in {
    val outcomes  = mutable.ListBuffer.empty[String]
    val deadProxy = new CountingFailingFetch(tunnelFailed)
    val leg       = EgressWiring.meteredProxyLeg(deadProxy, (o: String) => outcomes += o)

    (1 to 4).foreach(_ => an[java.io.IOException] should be thrownBy leg.get("https://vwc.odeon.co.uk/x"))
    a[CircuitOpenException] should be thrownBy leg.get("https://vwc.odeon.co.uk/y")

    outcomes.toList shouldBe List.fill(4)(HttpOutcome.ConnectionError)
  }

  it should "count a 401 from the origin behind the proxy as the 401 it is" in {
    val outcomes = mutable.ListBuffer.empty[String]
    val leg = EgressWiring.meteredProxyLeg(
      new CountingFailingFetch(url => new HttpStatusException(401, "GET", url, None)), (o: String) => outcomes += o)
    an[HttpStatusException] should be thrownBy leg.get("https://vwc.odeon.co.uk/x")
    outcomes.toList shouldBe List(HttpOutcome.Http401)
  }

  // UK 2026-09-21/22: Odeon's ocapi answered 404 for a business date through the proxy
  // (the leg that reaches the origin), then the Zyte leg 401'd and direct 403'd on the
  // Cloudflare block. The composite "All 2 backends failed" hid the origin's answer, so
  // the ScrapeChunk rescheduled on a permanent 404 until it exhausted — 20+ minutes of
  // retries, each paying the paid legs again. Every leg is a route to the SAME origin, so
  // the origin's "not found" from any of them is the answer.
  "proxyPrimary" should "end the chain on the origin's not-found instead of masking it with a blocked fallback" in {
    val fallback = new CountingFailingFetch(url => new HttpStatusException(403, "GET", url, None))
    val originSaysGone = new CountingFailingFetch(url => new HttpStatusException(404, "GET", url, None))
    val chain = EgressWiring.proxyPrimary(IndexedSeq(originSaysGone), fallback)

    val thrown = the[HttpStatusException] thrownBy chain.get("https://vwc.odeon.co.uk/showtimes/by-business-date/2026-10-02")
    thrown.code shouldBe 404
    fallback.calls.get() shouldBe 0
  }

  it should "still fall through on a failure that is not the origin's answer" in {
    val working = new GetOnlyHttpFetch { override def get(url: String): String = "ok" }
    val chain = EgressWiring.proxyPrimary(IndexedSeq(new CountingFailingFetch(tunnelFailed)), working)
    chain.get("https://vwc.odeon.co.uk/x") shouldBe "ok"
  }
}
