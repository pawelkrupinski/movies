package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetSocketAddress, URI}
import java.time.Duration

/**
 * RealHttpFetch's per-host timeout policy (RealHttpFetch.HostPolicies) — the only
 * part reachable without a live server. The actual slow-handshake / slow-read
 * behaviour (a host whose TLS handshake or response read runs far past our
 * defaults and dies with HttpConnectTimeoutException / HttpTimeoutException) needs
 * a real upstream and can't be reproduced in a unit test, so we assert the closest
 * reachable mechanism: that each policied host resolves to its intended connect /
 * request budget (and a real client carries the connect budget), while every other
 * host keeps the defaults. Host matching is exact-host-or-dotted-sub-domain, so
 * `www.x` matches `x` but an unrelated `*.y` does not, and a malformed URL falls
 * through to the defaults.
 */
class RealHttpFetchSpec extends AnyFlatSpec with Matchers {

  // ── Slow-TLS connect budget (Kino Iluzjon) ────────────────────────────────
  // Iluzjon's TLS handshake runs 20-30s server-side; under the 5s default connect
  // budget every fetch died with HttpConnectTimeoutException. Its host policy gives
  // it a long connect budget; the response-read budget stays the default.

  "connectTimeoutFor" should "give Kino Iluzjon's host and its sub-domains the long connect budget" in {
    RealHttpFetch.connectTimeoutFor("https://www.iluzjon.fn.org.pl/repertuar.html") shouldBe Duration.ofSeconds(40)
    RealHttpFetch.connectTimeoutFor("https://iluzjon.fn.org.pl/filmy/info/42/x.html") shouldBe Duration.ofSeconds(40)
  }

  it should "keep the tight default for unrelated hosts (incl. a different fn.org.pl sub-domain) and a malformed URL" in {
    RealHttpFetch.connectTimeoutFor("https://www.multikino.pl/repertuar") shouldBe RealHttpFetch.DefaultConnectTimeout
    RealHttpFetch.connectTimeoutFor("https://api.themoviedb.org/3/movie/1") shouldBe RealHttpFetch.DefaultConnectTimeout
    RealHttpFetch.connectTimeoutFor("https://other.fn.org.pl/x") shouldBe RealHttpFetch.DefaultConnectTimeout
    RealHttpFetch.connectTimeoutFor("not a url") shouldBe RealHttpFetch.DefaultConnectTimeout
  }

  it should "be longer than the tight default and above the worst handshake measured (~27s)" in {
    val iluzjon = RealHttpFetch.connectTimeoutFor("https://iluzjon.fn.org.pl/x")
    iluzjon.compareTo(RealHttpFetch.DefaultConnectTimeout) should be > 0
    iluzjon.compareTo(Duration.ofSeconds(30)) should be > 0
  }

  "clientFor" should "give Iluzjon the long connect budget and everyone else the default" in {
    val http = new RealHttpFetch()
    http.clientFor("https://www.iluzjon.fn.org.pl/repertuar.html")
      .connectTimeout().orElseThrow() shouldBe RealHttpFetch.connectTimeoutFor("https://iluzjon.fn.org.pl/x")
    http.clientFor("https://www.multikino.pl/repertuar")
      .connectTimeout().orElseThrow() shouldBe RealHttpFetch.DefaultConnectTimeout
  }

  // ── Fast-fail request budget (stall-prone enrichment host: Helios REST) ────
  // restapi.helios.pl's detail endpoints intermittently hang ~30s for our
  // datacenter egress; under the 30s default each hang pinned a ParallelDetail-
  // Fetch slot, draining the worker's CPU credit into a throttle spiral
  // (2026-06-23). Its host policy gives the tight response-read budget, while
  // every other host keeps the generous default.

  "requestTimeoutFor" should "give Helios's REST host and its sub-domains the tight fast-fail budget" in {
    RealHttpFetch.requestTimeoutFor("https://restapi.helios.pl/api/cinema/4b/screen/6c") shouldBe Duration.ofSeconds(4)
    RealHttpFetch.requestTimeoutFor("https://www.restapi.helios.pl/api/movie/1") shouldBe Duration.ofSeconds(4)
  }

  it should "keep the default for Helios's own NUXT site, an unrelated host, and a malformed URL" in {
    RealHttpFetch.requestTimeoutFor("https://www.helios.pl/poznan/kino-helios/repertuar") shouldBe RealHttpFetch.DefaultRequestTimeout
    RealHttpFetch.requestTimeoutFor("https://api.themoviedb.org/3/movie/1") shouldBe RealHttpFetch.DefaultRequestTimeout
    RealHttpFetch.requestTimeoutFor("not a url") shouldBe RealHttpFetch.DefaultRequestTimeout
  }

  it should "make Helios's budget below the ~5-7s degraded tail (so a slow host times out and trips the breaker) but above the ~1-3s healthy call" in {
    val helios = RealHttpFetch.requestTimeoutFor("https://restapi.helios.pl/api/x")
    helios.compareTo(Duration.ofSeconds(5)) should be < 0   // below the degraded tail → slow host times out
    helios.compareTo(Duration.ofSeconds(3)) should be > 0   // above the healthy ~1-3s call
  }

  // ── Metacritic middle budget (slow Cloudflare origin) ─────────────────────

  it should "give Metacritic and its sub-domains a budget between the fast-fail and the default" in {
    // ~35% of metascore fetches legitimately take >5s, so the 8s fast-fail budget
    // would cut them; 15s caps a real hang while clearing the healthy tail.
    RealHttpFetch.requestTimeoutFor("https://www.metacritic.com/movie/dune-part-two/") shouldBe Duration.ofSeconds(15)
    RealHttpFetch.requestTimeoutFor("https://metacritic.com/search/x/?category=2") shouldBe Duration.ofSeconds(15)
    val mc = RealHttpFetch.requestTimeoutFor("https://www.metacritic.com/x")
    mc.compareTo(RealHttpFetch.requestTimeoutFor("https://restapi.helios.pl/api/x")) should be > 0
    mc.compareTo(RealHttpFetch.DefaultRequestTimeout) should be < 0
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

  // ── Caller-supplied headers must REPLACE the defaults ─────────────────────
  // `HttpRequest.Builder.header` APPENDS. Applying caller overrides with it sent
  // BOTH values — so `WikidataClient`, which exists to satisfy Wikimedia's
  // "identify yourself" policy, was sending its polite UA *and* a Chrome string,
  // and any API that judges us by our UA saw a browser claiming to be a browser
  // twice. `setHeader` is the overwrite form.
  "buildRequest" should "let a caller override a default header instead of sending both values" in {
    val request = new RealHttpFetch().buildRequest(
      "https://example.org/x", Map("User-Agent" -> "kinowo/1.0 (contact)"))
    request.headers.allValues("User-Agent") should contain only "kinowo/1.0 (contact)"
  }

  it should "keep the defaults a caller did NOT override" in {
    val request = new RealHttpFetch().buildRequest(
      "https://example.org/x", Map("User-Agent" -> "kinowo/1.0 (contact)"))
    request.headers.allValues("Accept-Encoding") should contain only "gzip"
    request.headers.firstValue("Accept-Language").isPresent shouldBe true
  }

  it should "still send a caller header that has no default to collide with" in {
    val request = new RealHttpFetch().buildRequest(
      "https://www.wikidata.org/w/api.php", Map("Api-User-Agent" -> "kinowo/1.0"))
    request.headers.allValues("Api-User-Agent") should contain only "kinowo/1.0"
  }

  // ── Per-host identifying headers (IMDb's GraphQL CDN) ─────────────────────
  // On 2026-08-01 IMDb's edge started 403-ing every POST to
  // caching.graphql.imdb.com that arrives without a client-identifying header —
  // an nginx-shaped "403 Forbidden" page, not a GraphQL error, so the block is at
  // the CDN not the API. Every IMDb rating in all three countries silently became
  // "rating none". The same request WITH `x-imdb-client-name` returns 200 (verified
  // against the live endpoint from both a residential and the Fly egress, so this
  // is a request-shape rule, not an IP ban). The header rides on the host policy
  // table — data, one row, like every other per-host rule — so it applies at the
  // terminal fetch and no decorator in the chain can drop it.

  "headersFor" should "give IMDb's GraphQL CDN the client-name header its edge now demands" in {
    RealHttpFetch.headersFor("https://caching.graphql.imdb.com/") shouldBe
      Map("x-imdb-client-name" -> "imdb-web-next")
  }

  it should "leave every other host — including IMDb's suggestion endpoint, which is not blocked — header-free" in {
    RealHttpFetch.headersFor("https://v3.sg.media-imdb.com/suggestion/i/interstellar.json") shouldBe empty
    RealHttpFetch.headersFor("https://api.themoviedb.org/3/movie/1") shouldBe empty
    RealHttpFetch.headersFor("not a url") shouldBe empty
  }

  "postRequest" should "carry the host policy's headers on the POST that was being 403'd" in {
    val request = new RealHttpFetch().postRequest(
      "https://caching.graphql.imdb.com/", """{"query":"{x}"}""", "application/json")
    request.headers.allValues("x-imdb-client-name") should contain only "imdb-web-next"
    request.headers.allValues("Content-Type") should contain only "application/json"
  }

  it should "leave a POST to an unpoliced host exactly as it was" in {
    val request = new RealHttpFetch().postRequest(
      "https://query.wikidata.org/sparql", "SELECT", "application/sparql-query")
    request.headers.firstValue("x-imdb-client-name").isPresent shouldBe false
    request.headers.allValues("Content-Type") should contain only "application/sparql-query"
  }

  "buildRequest" should "carry the host policy's headers on the GET path too" in {
    val request = new RealHttpFetch().buildRequest("https://caching.graphql.imdb.com/x")
    request.headers.allValues("x-imdb-client-name") should contain only "imdb-web-next"
  }

  it should "let a caller's explicit header win over the host policy's" in {
    val request = new RealHttpFetch().buildRequest(
      "https://caching.graphql.imdb.com/x", Map("x-imdb-client-name" -> "caller-wins"))
    request.headers.allValues("x-imdb-client-name") should contain only "caller-wins"
  }
}
