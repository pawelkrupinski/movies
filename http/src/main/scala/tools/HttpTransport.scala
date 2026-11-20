package tools

import java.net.http.{HttpClient, HttpConnectTimeoutException, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{CookieManager, CookiePolicy}
import java.time.Duration
import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutionException, TimeUnit, TimeoutException}

import play.api.Logger

/** How [[RealHttpFetch]]'s requests go out: the JDK clients it sends on (one per connect budget and HTTP version the
 *  host policies ask for, through `proxy` when configured, handshaking with `tls`), and one exchange -- its redirects
 *  through [[RedirectGuard]], a failed connect moved onto the host's next address ([[AddressFailover]]), and each hop
 *  bounded by its host's deadline. What a reply becomes is [[HttpReplies]]'; what a request carries, the fetch's. */
private[tools] final class HttpTransport(proxy: Option[ProxyConfig], tls: javax.net.ssl.SSLContext,
                                        requestTimeoutFor: String => Duration, connectTimeoutFor: String => Duration,
                                        addresses: AddressFailover, logger: Logger) {
  import HttpTransport.unwrap

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
  def clientFor(url: String): HttpClient =
    clientsByShape.computeIfAbsent((connectTimeoutFor(url), HostPolicies.httpVersionFor(url)), {
      case (connectTimeout, version) => buildClient(connectTimeout, version)
    })

  /** [[sendAsync]], waited for. A failure is rethrown as its cause (an `HttpTimeoutException`,
   *  a `ConnectException`, …) — the shape `HttpClient.send` throws.
   *
   *  It waits on a latch, NOT in `CompletableFuture.get` (which `HttpClient.send` uses): on a
   *  ForkJoinPool worker, `get` first runs async tasks queued on that worker, on this thread,
   *  inside the wait. A caller that fans reads out with `CompletableFuture.runAsync` and holds a
   *  slot across each (HostPacing) then has a queued read wait for a slot this very thread's
   *  outer frames hold — a deadlock at 0% CPU, two IMDb POSTs hung 23+ minutes in an identity
   *  capture. A latch just parks; once it opens the result is in, and `get` returns at once. */
  def send(request: HttpRequest): HttpResponse[Array[Byte]] = {
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
  def sendAsync(request: HttpRequest): CompletableFuture[HttpResponse[Array[Byte]]] = sendAsync(request, hops = 0)

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

}

object HttpTransport {
  // Async failures come wrapped in `CompletionException`; unwrap so the
  // log line names the real cause (`HttpTimeoutException`, …) rather than
  // the framework wrapper.
  def unwrap(exception: Throwable): Throwable = exception match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }

}
