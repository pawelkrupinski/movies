package modules.wiring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools._

import java.util.concurrent.atomic.AtomicInteger

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
}
