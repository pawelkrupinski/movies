package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
}
