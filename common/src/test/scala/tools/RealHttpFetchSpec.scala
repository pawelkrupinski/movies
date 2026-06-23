package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetSocketAddress, URI}
import java.time.Duration

/**
 * The slow-TLS host routing — the only branch of RealHttpFetch reachable
 * without a live server. The actual slow-handshake behaviour (a host whose TLS
 * handshake runs 20-30s under our 5s default and dies with
 * HttpConnectTimeoutException) needs a real upstream and can't be reproduced in
 * a unit test, so we assert the closest reachable mechanism: that Iluzjon's host
 * is classified slow and gets a client whose actual connect timeout is the long
 * budget, while every other host keeps the tight default.
 */
class RealHttpFetchSpec extends AnyFlatSpec with Matchers {

  "isSlowTlsHost" should "match Kino Iluzjon's host and its sub-domains" in {
    RealHttpFetch.isSlowTlsHost("https://www.iluzjon.fn.org.pl/repertuar.html") shouldBe true
    RealHttpFetch.isSlowTlsHost("https://iluzjon.fn.org.pl/filmy/info/42/x.html") shouldBe true
  }

  it should "not match unrelated hosts (including a different fn.org.pl sub-domain)" in {
    RealHttpFetch.isSlowTlsHost("https://www.multikino.pl/repertuar") shouldBe false
    RealHttpFetch.isSlowTlsHost("https://api.themoviedb.org/3/movie/1") shouldBe false
    RealHttpFetch.isSlowTlsHost("https://other.fn.org.pl/x") shouldBe false
  }

  it should "not throw on a malformed URL" in {
    RealHttpFetch.isSlowTlsHost("not a url") shouldBe false
  }

  "the slow-TLS budget" should "be longer than the tight default" in {
    RealHttpFetch.SlowTlsConnectTimeout.compareTo(RealHttpFetch.DefaultConnectTimeout) should be > 0
    // Above the worst handshake measured (~27s) so a slow-but-alive Iluzjon
    // completes instead of timing out.
    RealHttpFetch.SlowTlsConnectTimeout.compareTo(Duration.ofSeconds(30)) should be > 0
  }

  "clientFor" should "give Iluzjon the long connect budget and everyone else the default" in {
    val http = new RealHttpFetch()
    http.clientFor("https://www.iluzjon.fn.org.pl/repertuar.html")
      .connectTimeout().orElseThrow() shouldBe RealHttpFetch.SlowTlsConnectTimeout
    http.clientFor("https://www.multikino.pl/repertuar")
      .connectTimeout().orElseThrow() shouldBe RealHttpFetch.DefaultConnectTimeout
  }

  // ── Fast-fail per-request timeout (stall-prone enrichment hosts) ───────────
  //
  // restapi.helios.pl's detail endpoints intermittently hang ~30s for our
  // datacenter egress; under the 30s default each hang pinned a ParallelDetail-
  // Fetch slot, ballooning Helios scrapes and draining the worker's CPU credit
  // into a throttle spiral (2026-06-23). We assert the closest reachable
  // mechanism — that the host is classified fast-fail and gets the tight
  // response-read budget, while every other host keeps the generous default.

  "isFastFailHost" should "match Helios's REST API host and its sub-domains" in {
    RealHttpFetch.isFastFailHost("https://restapi.helios.pl/api/cinema/4b/screen/6c") shouldBe true
    RealHttpFetch.isFastFailHost("https://www.restapi.helios.pl/api/movie/1") shouldBe true
  }

  it should "not match unrelated hosts (including Helios's own NUXT site)" in {
    RealHttpFetch.isFastFailHost("https://www.helios.pl/poznan/kino-helios/repertuar") shouldBe false
    RealHttpFetch.isFastFailHost("https://api.themoviedb.org/3/movie/1") shouldBe false
  }

  it should "not throw on a malformed URL" in {
    RealHttpFetch.isFastFailHost("not a url") shouldBe false
  }

  "the fast-fail request budget" should "be much tighter than the default" in {
    RealHttpFetch.FastFailRequestTimeout.compareTo(RealHttpFetch.DefaultRequestTimeout) should be < 0
    // Above the ~1-3s a healthy Helios detail call takes, with headroom.
    RealHttpFetch.FastFailRequestTimeout.compareTo(Duration.ofSeconds(5)) should be > 0
  }

  "requestTimeoutFor" should "give Helios's REST host the tight budget and everyone else the default" in {
    RealHttpFetch.requestTimeoutFor("https://restapi.helios.pl/api/cinema/4b/screen/6c") shouldBe
      RealHttpFetch.FastFailRequestTimeout
    RealHttpFetch.requestTimeoutFor("https://www.helios.pl/poznan/kino-helios/repertuar") shouldBe
      RealHttpFetch.DefaultRequestTimeout
    RealHttpFetch.requestTimeoutFor("https://api.themoviedb.org/3/movie/1") shouldBe
      RealHttpFetch.DefaultRequestTimeout
  }

  // ── Residential-proxy egress (Decodo static ISP) ──────────────────────────

  private def selectedPort(pc: RealHttpFetch.ProxyConfig): Int =
    pc.selector.select(URI.create("https://www.multikino.pl/api/x")).get(0)
      .address().asInstanceOf[InetSocketAddress].getPort

  "ProxyConfig.pinnedTo" should "pin the selector to the chosen pool port, stickily" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), "u", "p")
    val pinned = pool.pinnedTo(10002)
    // Sticky: every selection resolves to the one pinned IP — Multikino's session
    // cookie is IP-bound, so the homepage-warm + API retry must share an egress.
    pinned.port shouldBe 10002
    (1 to 5).map(_ => selectedPort(pinned)).distinct shouldBe List(10002)
  }

  it should "give distinct pool ports distinct selectors (so clients spread across IPs)" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), "u", "p")
    pool.ports.map(p => selectedPort(pool.pinnedTo(p))) shouldBe List(10001, 10002, 10003)
  }

  it should "reject a port that isn't one of the pool's ports" in {
    an[IllegalArgumentException] should be thrownBy
      RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002), "u", "p").pinnedTo(10099)
  }

  "ProxyConfig.perPort" should "yield one config pinned to each pool port (the shard egresses)" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), "u", "p")
    pool.perPort.map(selectedPort) shouldBe List(10001, 10002, 10003)
  }

  it should "clear jdk.http.auth.tunneling.disabledSchemes so Basic proxy auth works over HTTPS CONNECT" in {
    // The JDK default is "Basic", which 407s every HTTPS fetch through the proxy
    // — the gotcha that makes the worker's proxied egress fail without this.
    RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001), "u", "p")
    System.getProperty("jdk.http.auth.tunneling.disabledSchemes") shouldBe ""
  }

  it should "reject an empty port list (a misconfigured proxy)" in {
    an[IllegalArgumentException] should be thrownBy
      RealHttpFetch.ProxyConfig("isp.decodo.com", Seq.empty, "u", "p")
  }
}
