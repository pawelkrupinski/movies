package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetSocketAddress, URI}

/**
 * The HTTP client itself — what it builds from the per-host policy it consults
 * ([[HostPolicies]], whose lookups HostPoliciesSpec pins). A stalled or slow
 * response body is served by a local server below; the slow TLS handshake needs a
 * real upstream and can't be reproduced in a unit test, so we assert the closest
 * reachable mechanism for it and the headers: that a
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
    // A handler of its own per exchange: the stalled bodies below hold theirs until the server stops.
    val handlers = java.util.concurrent.Executors.newCachedThreadPool()
    server.setExecutor(handlers)
    val stopped = new java.util.concurrent.CountDownLatch(1)
    // Status and headers, the first bytes of a 1000-byte body, then nothing until the server stops.
    def stall(exchange: com.sun.net.httpserver.HttpExchange): Unit = {
      exchange.sendResponseHeaders(200, 1000)
      exchange.getResponseBody.write("<html>".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      exchange.getResponseBody.flush()
      stopped.await()
      exchange.close()
    }
    // A body that dribbles in over ~200 ms but does finish: well inside the 1 s request budget, since
    // what is asserted is that a body arriving in pieces is read whole, not how close to the budget it
    // can run. At 150 ms a chunk (750 ms) a loaded CI runner let the JDK's body timer cut it after the
    // fourth chunk ("fixed content-length: 30, bytes received: 23", run 37993027475).
    def dribble(exchange: com.sun.net.httpserver.HttpExchange): Unit = {
      val chunks = Seq("<html>", "<body>", "Kino", "</body>", "</html>")
      exchange.sendResponseHeaders(200, chunks.map(_.length).sum.toLong)
      chunks.foreach { chunk =>
        exchange.getResponseBody.write(chunk.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        exchange.getResponseBody.flush()
        Thread.sleep(50)
      }
      exchange.close()
    }
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
    server.createContext("/stalled-body", e => stall(e))
    server.createContext("/slow-body", e => dribble(e))
    server.start()
    try test(base) finally { stopped.countDown(); server.stop(0); handlers.shutdownNow(); () }
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

  // ── A body that stalls after its headers ──────────────────────────────────
  // Up to JDK 25 a request's timeout covers only the wait for the response HEADERS, so a
  // body that stalled after them held HttpClient.send for good (JDK 26+ times the body
  // too; the build targets Java 21). Every read gets a whole-exchange deadline (connect +
  // headers + body): the host's connect budget plus its request budget, failing as the
  // same HttpTimeoutException a header-phase timeout does (or, when the JDK's own body timer wins, its IOException).

  private val requestBudget = java.time.Duration.ofSeconds(1)
  private def tightFetch = new RealHttpFetch(requestTimeoutFor = _ => requestBudget)
  // The local server's host has the default connect budget.
  private val deadline = HostPolicies.DefaultConnectTimeout.plus(requestBudget)

  /** `read`'s failure and how long it took to come, bounded so a hanging read fails the spec instead of hanging it. */
  private def timedFailure(read: => Any): (Throwable, java.time.Duration) = {
    val started = System.nanoTime()
    // A thread of its own: a read that hangs keeps it, never a pool thread another suite needs.
    val outcome = java.util.concurrent.CompletableFuture.supplyAsync(() => scala.util.Try(read), (task: Runnable) => new Thread(task).start())
    val result  = outcome.get(deadline.toMillis * 4, java.util.concurrent.TimeUnit.MILLISECONDS)
    val elapsed = java.time.Duration.ofNanos(System.nanoTime() - started)
    result.failed.getOrElse(fail(s"the read succeeded: $result")) -> elapsed
  }

  private def unwrapped(failure: Throwable): Throwable = failure match {
    case wrapper @ (_: java.util.concurrent.ExecutionException | _: java.util.concurrent.CompletionException)
        if wrapper.getCause != null => unwrapped(wrapper.getCause)
    case other => other
  }

  "a body that stalls after its headers" should "time out get, post and getAsync within the host's budget" in withServer { base =>
    val reads: Seq[(String, () => Any)] = Seq(
      "get"      -> (() => tightFetch.get(s"$base/stalled-body")),
      "post"     -> (() => tightFetch.post(s"$base/stalled-body", "{}")),
      "getAsync" -> (() => tightFetch.getAsync(s"$base/stalled-body").get()),
    )
    reads.foreach { case (name, read) =>
      withClue(name) {
        val (failure, elapsed) = timedFailure(read())
        // Whichever timer fires first: our whole-exchange deadline fails it as an HttpTimeoutException; on JDK 26+ the
        // JDK's own body timer can beat it and cut the body short as a plain IOException ("bytes received: 6"). Both are
        // the read failing inside the bound instead of hanging, and every caller handles them as one failed read.
        unwrapped(failure) shouldBe an [java.io.IOException]
        elapsed.compareTo(requestBudget) should be >= 0
        elapsed.compareTo(deadline.plusSeconds(3)) should be < 0
      }
    }
  }

  "a slow body that finishes" should "still be read whole" in withServer { base =>
    tightFetch.get(s"$base/slow-body") shouldBe "<html><body>Kino</body></html>"
    tightFetch.getAsync(s"$base/slow-body").get() shouldBe "<html><body>Kino</body></html>"
  }

  // ── A blocking read on a ForkJoinPool worker ──────────────────────────────
  // CompletableFuture.get on a ForkJoinPool worker first RUNS async tasks queued on that
  // worker (ForkJoinPool.helpAsyncBlocker), on the waiting thread, inside the wait. The
  // identity capture fans its reads out with CompletableFuture.runAsync, each holding a
  // HostPacing slot while it reads: a waiting read picked up a queued one, which waited for
  // a slot its own thread's outer frames held — two IMDb POSTs deadlocked 23+ minutes at
  // 0% CPU (JDK 27, thread dump 2026-10-07). This gate is that slot: held across the read,
  // and wanted again by the task queued behind it.

  "a read on a ForkJoinPool worker" should "wait for its response without running the worker's queued tasks" in withServer { base =>
    val pool = new java.util.concurrent.ForkJoinPool(1)
    val gate = new java.util.concurrent.Semaphore(1)
    try {
      val reader = java.util.concurrent.CompletableFuture.supplyAsync(() => {
        gate.acquire()
        try {
          // Queued on this worker; it can only run once the read is done and the gate is free.
          java.util.concurrent.CompletableFuture.runAsync(() => { gate.acquire(); gate.release() }, pool)
          new RealHttpFetch().get(s"$base/slow-body")
        } finally gate.release()
      }, pool)
      reader.get(SpecBound.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe "<html><body>Kino</body></html>"
    } finally pool.shutdownNow()
  }

  // Far past the slow body's under-a-second answer: only a deadlocked read gets near it.
  private val SpecBound = java.time.Duration.ofSeconds(20)
}
