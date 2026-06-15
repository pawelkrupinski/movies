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

  // ── Residential-proxy egress (Decodo static ISP) ──────────────────────────

  private def selectedPort(pc: RealHttpFetch.ProxyConfig): Int =
    pc.selector.select(URI.create("https://www.multikino.pl/api/x")).get(0)
      .address().asInstanceOf[InetSocketAddress].getPort

  "RealHttpFetch.ProxyConfig" should "pin to ONE sticky IP (the first port), not rotate" in {
    val pc = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), "u", "p")
    // Sticky, NOT round-robin: Multikino's session cookie is IP-bound, so the
    // homepage-warm + API retry must share an egress IP. The other ports are
    // spares to switch to manually if the active one is burned.
    pc.port shouldBe 10001
    (1 to 5).map(_ => selectedPort(pc)).distinct shouldBe List(10001)
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
