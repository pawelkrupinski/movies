package tools

import play.api.Logging

import java.net.URI
import java.net.http.{HttpClient, HttpConnectTimeoutException, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{CookieManager, CookiePolicy}
import java.time.Duration
import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutionException, TimeUnit, TimeoutException}
import scala.annotation.unused

class RealHttpFetch(
  proxy: Option[ProxyConfig] = None,
  // The TLS context every client below handshakes with (see TlsTrust). A wiring builds
  // one and hands it to each fetch it composes, so they share its session cache; a
  // fetch built without one gets its own.
  tls: javax.net.ssl.SSLContext = TlsTrust.newContext(),
  // The host's request budget — HostPolicies' table; a spec passes a short one.
  requestTimeoutFor: String => Duration = HostPolicies.requestTimeoutFor,
  // The host's connect budget (TCP and TLS handshake) — HostPolicies' table. A spec whose handshake is the thing under
  // test, in a cold JVM on a loaded machine, passes its own (IssuerCertificateFetchingSpec): the 5 s a live host needs
  // is no bound on a starved runner's class loading and path building.
  connectTimeoutFor: String => Duration = HostPolicies.connectTimeoutFor,
  // Which of a host's addresses the JVM connects to (AddressFailover): the JVM's resolver consults this one instance,
  // so a connect failure reported here moves the next connect onto the host's next address.
  addresses: AddressFailover = FailoverAddressResolverProvider.failover
) extends HttpFetch with Logging {
  // Connect + per-request timeouts so a hung upstream (MC search HTML
  // sometimes streams forever, Filmweb soft-blocks by holding the socket
  // open, …) can't pin a worker thread indefinitely. Production was
  // tolerating this — the daemon-thread pool would just have a stuck
  // thread that nobody noticed. The recording script can't: one stuck
  // request blocks `fullySyncOne`'s sequential loop and the rest of the
  // 200+ rows never run.
  // The connect timeout (for HTTPS this bounds the TCP handshake AND the TLS
  // handshake — the JVM throws HttpConnectTimeoutException if either runs over).
  // A live host completes both in well under a second, so 5s is generous
  // headroom. Keeping it low matters for the fan-out scrapers:
  // ParallelDetailFetch runs just 2 fetches at once, so a dead/refusing host
  // that hangs the connect pins HALF the budget for the whole timeout. At 10s a
  // couple of unreachable detail URLs stalled a wave for 10s each (most of
  // Kinoteka's ~57s scrape); at 5s the slot is freed twice as fast to fetch the
  // films that *are* reachable. The request (response-read) timeout is separate
  // and bounds reading the response, where a slow upstream legitimately needs
  // longer — so trimming connect doesn't cut off slow-but-alive servers. It is
  // per-host (see HostPolicies): the default 30s, but a stall-prone
  // host gets a tight budget so one stalling origin can't pin a fan-out slot for
  // the full default. The connect budget is per-host the same way.

  // `CookieManager` makes the client a well-behaved HTTP citizen: any
  // `Set-Cookie` header lands in the in-memory store and is sent back on
  // subsequent requests to the same domain. Multikino's direct path
  // depends on it — the homepage hands out a session cookie that the
  // API call must carry — and other clients are unaffected (cookies
  // are domain-scoped, and the APIs we talk to are otherwise stateless).
  private def buildClient(connectTimeout: Duration, version: HttpClient.Version): HttpClient = {
    val builder = HttpClient.newBuilder()
      .version(version)
      // Redirects are followed by `send`/`sendAsync` below, through RedirectGuard: the
      // JDK's NORMAL rules, minus any hop onto a local address.
      .followRedirects(HttpClient.Redirect.NEVER)
      .connectTimeout(connectTimeout)
      // Trust the JDK defaults PLUS the Certum root that OpenJDK's cacerts omits,
      // so the Certum-rooted cinema sites (Kinomuzeum/artmuseum.pl, Kino
      // Muranów/kinomuranow.pl, sdk.waw.pl) stop failing PKIX path building. See
      // TlsTrust. (AIA intermediate fetching is a JVM-wide switch each main sets once, through
      // IssuerCertificateFetching.applyToJvm.)
      .sslContext(tls)
      .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    // Route through an authenticated residential proxy when configured (the
    // Decodo static-ISP egress for the cinema sites that Cloudflare-block our
    // datacenter IP). Only the proxied instance carries this; everything else
    // fetches direct. No Authenticator: its credentials ride each request as a
    // preemptive Proxy-Authorization (see `withProxyCredentials`), because a client
    // carrying one turns an ORIGIN's 401 without WWW-Authenticate (Multikino's and
    // Vue's cold-session answer) into `IOException: WWW-Authenticate header missing`
    // instead of the status.
    proxy.foreach(p => builder.proxy(p.selector))
    builder.build()
  }

  // One HttpClient per distinct (connect budget, HTTP version) the host-policy table
  // can ask for (plus the defaults). Building a client isn't free and the distinct
  // pairs are few, so prebuild them all and pick per host; a shape an injected connect
  // budget asks for is built the first time it is asked.
  private val clientsByShape = new java.util.concurrent.ConcurrentHashMap[(Duration, HttpClient.Version), HttpClient]()
  ((HostPolicies.DefaultConnectTimeout, HostPolicies.DefaultHttpVersion) +:
    HostPolicies.all.map(policy => (policy.connectTimeout, policy.httpVersion)))
    .distinct
    .foreach { case shape @ (connectTimeout, version) => clientsByShape.put(shape, buildClient(connectTimeout, version)) }

  /** Pick the client whose connect budget and HTTP version match the host policy: the
   *  slow-TLS hosts get their long handshake budget, the HTTP/2 hosts an h2 client,
   *  everything else the tight default over HTTP/1.1. Package-private so the spec can
   *  assert the routing (the real slow-handshake behaviour needs a live server and
   *  can't be reached in a unit test). */
  private[tools] def clientFor(url: String): HttpClient =
    clientsByShape.computeIfAbsent((connectTimeoutFor(url), HostPolicies.httpVersionFor(url)), {
      case (connectTimeout, version) => buildClient(connectTimeout, version)
    })

  /** What an exchange's reply or failure becomes -- a body, a typed status failure -- and the line it is logged as,
   *  under this class's logger. */
  private[tools] val replies = new HttpReplies(logger, requestTimeoutFor, connectTimeoutFor)

  override def get(url: String): String = get(url, Map.empty)

  /** Raw wire bytes (gunzipped, NOT charset-decoded) so a caller can pick the
   *  right charset itself. See [[HttpFetch.getBytes]] — only the charset-quirky
   *  scrapers (Kino Charlie) use this; everyone else gets the UTF-8 `get`. */
  override def getBytes(url: String): Array[Byte] = {
    replies.bytes(url, send(buildRequest(url, Map.empty)))
  }

  override def get(url: String, headers: Map[String, String]): String =
    replies.body("GET", url, replies.sentLogged("GET", url, send(buildRequest(url, headers))))

  /** GET, also reporting the URL the redirects ended on — for a caller that
   *  must know whether the address it holds is the one the upstream serves (the
   *  roster audit's stale-slug check). Same status handling as `get`. */
  def getPage(url: String): FetchedPage = {
    val response = replies.sentLogged("GET", url, send(buildRequest(url)))
    FetchedPage(response.uri().toString, replies.body("GET", url, response))
  }

  override def getAsync(url: String): CompletableFuture[String] =
    sendAsync(buildRequest(url), hops = 0)
      .handle[String] { (response, throwable) =>
        if (throwable != null) {
          replies.failed("GET", url, unwrap(throwable))
          throw throwable
        }
        replies.body("GET", url, response)
      }

  override def post(url: String, body: String, contentType: String = "application/json"): String =
    replies.body("POST", url, replies.sentLogged("POST", url, send(postRequest(url, body, contentType))))

  /** [[sendAsync]], waited for. A failure is rethrown as its cause (an `HttpTimeoutException`,
   *  a `ConnectException`, …) — the shape `HttpClient.send` throws.
   *
   *  It waits on a latch, NOT in `CompletableFuture.get` (which `HttpClient.send` uses): on a
   *  ForkJoinPool worker, `get` first runs async tasks queued on that worker, on this thread,
   *  inside the wait. A caller that fans reads out with `CompletableFuture.runAsync` and holds a
   *  slot across each (HostPacing) then has a queued read wait for a slot this very thread's
   *  outer frames hold — a deadlock at 0% CPU, two IMDb POSTs hung 23+ minutes in an identity
   *  capture. A latch just parks; once it opens the result is in, and `get` returns at once. */
  private def send(request: HttpRequest): HttpResponse[Array[Byte]] = {
    val exchange = sendAsync(request, hops = 0)
    val done     = new CountDownLatch(1)
    exchange.whenComplete((_, _) => done.countDown())
    try { done.await(); exchange.get() }
    catch {
      case failure: ExecutionException if failure.getCause != null => throw failure.getCause
      case interrupted: InterruptedException => exchange.cancel(true); throw interrupted
    }
  }

  /** Send `request`, following its redirects through [[RedirectGuard]]; each hop goes
   *  out on the client whose connect budget its own host's policy names, and within
   *  that host's [[exchangeDeadline]]. */
  private def sendAsync(request: HttpRequest, hops: Int): CompletableFuture[HttpResponse[Array[Byte]]] =
    toNextAddress(request, tried = 1).thenCompose { response =>
        RedirectGuard.next(request, response, hops) match {
          case Some(next) => sendAsync(next, hops + 1)
          case None       => CompletableFuture.completedFuture(response)
        }
      }

  /** One hop's exchange; when its connect fails and the host has another address ([[AddressFailover]]: the JDK's
   *  client tries only the first), the same request again on the next, [[AddressFailover.MostAddressesTried]] at most.
   *  Through a proxy the proxy resolves the host, so a failed connect is the proxy's and is not retried here. */
  private def toNextAddress(request: HttpRequest, tried: Int): CompletableFuture[HttpResponse[Array[Byte]]] = {
    val url     = request.uri().toString
    val started = addresses.now()
    withinDeadline(url, clientFor(url).sendAsync(request, HttpResponse.BodyHandlers.ofByteArray()))
      .exceptionallyCompose { failure =>
        unwrap(failure) match {
          case connect @ (_: java.net.ConnectException | _: HttpConnectTimeoutException)
              if proxy.isEmpty && tried < AddressFailover.MostAddressesTried && addresses.connectFailed(request.uri().getHost, started) =>
            logger.info(s"${RedactedUrl(url)}: connect to one of its host's addresses failed (${connect.getClass.getSimpleName}), trying the next")
            toNextAddress(request, tried + 1)
          case other => CompletableFuture.failedFuture(other)
        }
      }
  }

  /** One hop's exchange — connect, headers AND body — bounded by its host's
   *  [[exchangeDeadline]]. Up to JDK 25 a request's own `timeout` stops at the
   *  response HEADERS, so a body that stalls after them held `HttpClient.send` for
   *  good. (JDK 26+ times the body too; the build targets Java 21, so this does it here.)
   *  Past the deadline the exchange is cancelled, freeing its connection, and the
   *  read fails with the `HttpTimeoutException` a header-phase timeout throws, so
   *  every caller's timeout handling — logging, retry, back-off — applies unchanged. */
  private def withinDeadline[A](url: String, exchange: CompletableFuture[A]): CompletableFuture[A] = {
    val deadline = exchangeDeadline(url)
    val bounded  = new CompletableFuture[A]
    exchange.whenComplete { (value, failure) =>
      if (failure == null) bounded.complete(value) else bounded.completeExceptionally(unwrap(failure))
    }
    bounded.orTimeout(deadline.toMillis, TimeUnit.MILLISECONDS).exceptionallyCompose { failure =>
      unwrap(failure) match {
        case _: TimeoutException =>
          exchange.cancel(true)
          CompletableFuture.failedFuture(new HttpTimeoutException(s"no complete response within ${deadline.toMillis}ms"))
        case other => CompletableFuture.failedFuture(other)
      }
    }
  }

  /** The longest one hop of an exchange with `url`'s host may take: its connect
   *  budget plus its request budget. */
  private def exchangeDeadline(url: String): Duration =
    connectTimeoutFor(url).plus(requestTimeoutFor(url))

  /** Package-private so the spec can assert what actually goes on the wire — the
   *  header a host policy demands is only observable on the built request. */
  private[tools] def postRequest(url: String, body: String, contentType: String): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(requestTimeoutFor(url))
      .header("Content-Type", contentType)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      // Decoded transparently ([[HttpReplies]]). See the matching
      // header on `buildRequest` for the GET path.
      .header("Accept-Encoding", "gzip")
      .POST(HttpRequest.BodyPublishers.ofString(body))
    // `setHeader` (overwrite), not `header` (append) — same reason as `buildRequest`.
    HostPolicies.headersFor(url).foreach { case (k, v) => builder.setHeader(k, v) }
    withProxyCredentials(builder).build()
  }

  // Async failures come wrapped in `CompletionException`; unwrap so the
  // log line names the real cause (`HttpTimeoutException`, …) rather than
  // the framework wrapper.
  private def unwrap(exception: Throwable): Throwable = exception match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }

  private[tools] def buildRequest(url: String, extraHeaders: Map[String, String] = Map.empty): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(requestTimeoutFor(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      // Advertise gzip — [[HttpReplies]] decompresses transparently. Without
      // this header some CDNs still gzip based on the Chrome-shaped UA we
      // send, so the decode path runs regardless; advertising it makes the
      // negotiation explicit and shrinks the wire payload.
      .header("Accept-Encoding", "gzip")
      .GET()
    // `setHeader`, NOT `header`: the latter APPENDS, so a caller overriding one of
    // the defaults above (most importantly `User-Agent`) got BOTH values sent —
    // our identity plus a Chrome string, on the very requests where the override
    // exists because the upstream cares what we claim to be — Wikimedia's UA
    // policy ("use your app and version") is exactly that case.
    // Host-policy headers first, caller's second: an explicit per-call override
    // is more specific than a blanket rule about the host, so it wins.
    HostPolicies.headersFor(url).foreach { case (k, v) => builder.setHeader(k, v) }
    extraHeaders.foreach { case (k, v) => builder.setHeader(k, v) }

    withProxyCredentials(decorateBuilder(builder, url)).build()
  }

  /** The proxy's Basic credentials, sent up front. java.net.http copies a request's
   *  proxy-* headers onto the HTTPS `CONNECT` (subject to the process's
   *  [[ProxyTunnelAuthentication]]) and strips them from what goes to the origin. */
  private def withProxyCredentials(builder: HttpRequest.Builder): HttpRequest.Builder =
    proxy.fold(builder)(p => builder.setHeader("Proxy-Authorization", p.basicCredentials))

  protected def decorateBuilder(builder: HttpRequest.Builder, @unused url: String): HttpRequest.Builder = builder
}

/** A page body and the URL it was finally served from, after redirects. */
final case class FetchedPage(finalUrl: String, body: String)
