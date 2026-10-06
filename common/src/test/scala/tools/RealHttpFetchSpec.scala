package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetSocketAddress, URI}

/**
 * The HTTP client itself — what it builds from the per-host policy it consults
 * ([[HostPolicies]], whose lookups HostPoliciesSpec pins). The actual
 * slow-handshake / slow-read behaviour needs a real upstream and can't be
 * reproduced in a unit test, so we assert the closest reachable mechanism: that a
 * real client carries the connect budget its host policy names, that the built
 * request carries the policy's headers, and that a caller's explicit header wins.
 */
class RealHttpFetchSpec extends AnyFlatSpec with Matchers {

  // ── Slow-TLS connect budget (Kino Iluzjon) ────────────────────────────────
  // Iluzjon's TLS handshake runs 20-30s server-side; under the 5s default connect
  // budget every fetch died with HttpConnectTimeoutException. Its host policy gives
  // it a long connect budget, and the client the fetch routes to must carry it.

  "clientFor" should "give Iluzjon the long connect budget and everyone else the default" in {
    val http = new RealHttpFetch()
    http.clientFor("https://www.iluzjon.fn.org.pl/repertuar.html")
      .connectTimeout().orElseThrow() shouldBe HostPolicies.connectTimeoutFor("https://iluzjon.fn.org.pl/x")
    http.clientFor("https://www.multikino.pl/repertuar")
      .connectTimeout().orElseThrow() shouldBe HostPolicies.DefaultConnectTimeout
  }

  // ── Residential-proxy egress (Decodo static ISP) ──────────────────────────

  private def selectedPort(pc: RealHttpFetch.ProxyConfig): Int =
    pc.selector.select(URI.create("https://www.multikino.pl/api/x")).get(0)
      .address().asInstanceOf[InetSocketAddress].getPort

  it should "handshake with the TLS context it was handed, on every connect budget" in {
    val tls  = TlsTrust.newContext()
    val http = new RealHttpFetch(tls = tls)
    Seq("https://www.iluzjon.fn.org.pl/repertuar.html", "https://www.multikino.pl/repertuar")
      .foreach(url => http.clientFor(url).sslContext() should be theSameInstanceAs tls)
  }

  it should "keep a context of its own when none is handed in, rather than one for the whole JVM" in {
    val url = "https://www.multikino.pl/repertuar"
    new RealHttpFetch().clientFor(url).sslContext() should not be theSameInstanceAs(new RealHttpFetch().clientFor(url).sslContext())
  }

  "ProxyConfig.pinnedTo" should "pin the selector to the chosen pool port, stickily" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), settings.ProxyUser("u"), settings.ProxyPassword("p"))
    val pinned = pool.pinnedTo(10002)
    // Sticky: every selection resolves to the one pinned IP — Multikino's session
    // cookie is IP-bound, so the homepage-warm + API retry must share an egress.
    pinned.port shouldBe 10002
    (1 to 5).map(_ => selectedPort(pinned)).distinct shouldBe List(10002)
  }

  it should "give distinct pool ports distinct selectors (so clients spread across IPs)" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), settings.ProxyUser("u"), settings.ProxyPassword("p"))
    pool.ports.map(p => selectedPort(pool.pinnedTo(p))) shouldBe List(10001, 10002, 10003)
  }

  it should "reject a port that isn't one of the pool's ports" in {
    an[IllegalArgumentException] should be thrownBy
      RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002), settings.ProxyUser("u"), settings.ProxyPassword("p")).pinnedTo(10099)
  }

  "ProxyConfig.perPort" should "yield one config pinned to each pool port (the shard egresses)" in {
    val pool = RealHttpFetch.ProxyConfig("isp.decodo.com", Seq(10001, 10002, 10003), settings.ProxyUser("u"), settings.ProxyPassword("p"))
    pool.perPort.map(selectedPort) shouldBe List(10001, 10002, 10003)
  }

  "ProxyConfig" should "reject an empty port list (a misconfigured proxy)" in {
    an[IllegalArgumentException] should be thrownBy
      RealHttpFetch.ProxyConfig("isp.decodo.com", Seq.empty, settings.ProxyUser("u"), settings.ProxyPassword("p"))
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
  // IMDb's edge 403s every POST to caching.graphql.imdb.com that arrives without
  // `x-imdb-client-name` (see HostPoliciesSpec for the row). The header rides on
  // the host policy table so it applies at the terminal fetch and no decorator
  // in the chain can drop it — which is only observable on the built request.

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

  // ── getPage: where the redirects ended ─────────────────────────────────────
  // The roster audit tells a bilety24 organiser address we wire from the one
  // bilety24 now 301s it to; that needs the final URL, which `get` drops.

  private val landed = new java.util.concurrent.atomic.AtomicInteger(0)

  private def withServer(test: String => Unit): Unit = {
    val server = com.sun.net.httpserver.HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    def respond(code: Int, headers: (String, String)*)(body: String)(exchange: com.sun.net.httpserver.HttpExchange): Unit = {
      headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
      val bytes = body.getBytes(java.nio.charset.StandardCharsets.UTF_8)
      exchange.sendResponseHeaders(code, if (bytes.isEmpty) -1 else bytes.length.toLong)
      if (bytes.nonEmpty) exchange.getResponseBody.write(bytes)
      exchange.close()
    }
    val base = s"http://127.0.0.1:${server.getAddress.getPort}"
    server.createContext("/organizator/old-slug-477", e => respond(301, "Location" -> s"$base/organizator/new-slug-477")("")(e))
    server.createContext("/organizator/new-slug-477", e => respond(200)("<h1>Kino Baszta</h1>")(e))
    server.createContext("/gone", e => respond(404)("")(e))
    // A redirect that moves the request onto ANOTHER local address — the shape of the
    // sfr.pl answer that 301'd Kino Kreska's listing POST to 127.0.0.1. `localhost` is
    // a different host from the 127.0.0.1 the request was sent to, so the hop moves it.
    server.createContext("/moved-to-localhost", e =>
      respond(301, "Location" -> s"http://localhost:${server.getAddress.getPort}/landed")("")(e))
    server.createContext("/landed", e => { landed.incrementAndGet(); respond(200)("landed")(e) })
    // 301/302 turn a POST into a GET, as browsers (and the JDK's own redirect filter) do.
    server.createContext("/form", e => respond(302, "Location" -> "/method")("")(e))
    server.createContext("/method", e => respond(200)(e.getRequestMethod)(e))
    server.start()
    try test(base) finally server.stop(0)
  }

  "getPage" should "report the URL a redirect ended on, with the page served there" in withServer { base =>
    val page = new RealHttpFetch().getPage(s"$base/organizator/old-slug-477")
    page.finalUrl shouldBe s"$base/organizator/new-slug-477"
    page.body shouldBe "<h1>Kino Baszta</h1>"
  }

  it should "fail a non-2xx the way get does" in withServer { base =>
    val thrown = the [HttpStatusException] thrownBy new RealHttpFetch().getPage(s"$base/gone")
    thrown.code shouldBe 404
  }

  // ── Redirects onto a local address are refused, never followed ─────────────
  // sfr.pl has been reported answering Kino Kreska's listing POST with `301 Location:
  // 127.0.0.1`. Following it sends the request to whatever listens on the worker's
  // own loopback; refusing it fails the read with the reason in the message, which
  // is what reaches the scrape's error on /uptime.

  "a redirect onto a local address" should "be refused before anything is sent there" in withServer { base =>
    landed.set(0)
    val thrown = the [RefusedRedirectException] thrownBy
      new RealHttpFetch().post(s"$base/moved-to-localhost", "a=1", "application/x-www-form-urlencoded")
    thrown.getMessage should include ("301")
    thrown.getMessage should include ("localhost")
    landed.get shouldBe 0
  }

  it should "be refused on the GET and async paths too" in withServer { base =>
    landed.set(0)
    a [RefusedRedirectException] should be thrownBy new RealHttpFetch().get(s"$base/moved-to-localhost")
    a [RefusedRedirectException] should be thrownBy new RealHttpFetch().getPage(s"$base/moved-to-localhost")
    val async = the [java.util.concurrent.ExecutionException] thrownBy
      new RealHttpFetch().getAsync(s"$base/moved-to-localhost").get()
    async.getCause shouldBe a [RefusedRedirectException]
    landed.get shouldBe 0
  }

  "a redirect" should "turn a POST into a GET on a 302, resolving a relative Location" in withServer { base =>
    new RealHttpFetch().post(s"$base/form", "a=1", "application/x-www-form-urlencoded") shouldBe "GET"
  }

  "RedirectGuard.refusal" should "refuse a hop onto a loopback, wildcard or private address" in {
    def refusal(to: String) =
      RedirectGuard.refusal(URI.create("https://www.sfr.pl/heroapp/terms/rest/load"), URI.create(to))
    Seq("http://127.0.0.1/heroapp/terms/rest/load", "https://localhost/x", "http://[::1]/x", "http://0.0.0.0/x",
        "http://10.20.0.11:9428/x", "http://192.168.1.1/x", "http://169.254.169.254/latest/meta-data",
        "http://app.localhost/x")
      .foreach(to => withClue(to)(refusal(to)) shouldBe defined)
  }

  it should "let a hop between public hosts, or within one local host, through" in {
    RedirectGuard.refusal(URI.create("http://sfr.pl/x"), URI.create("https://www.sfr.pl/x")) shouldBe None
    RedirectGuard.refusal(URI.create("http://127.0.0.1:9000/a"), URI.create("http://127.0.0.1:9000/b")) shouldBe None
    // A host NAME is never resolved: only a literal address or a localhost name is judged.
    RedirectGuard.refusal(URI.create("https://www.sfr.pl/x"), URI.create("https://bilety.sfr.pl/x")) shouldBe None
  }
}
