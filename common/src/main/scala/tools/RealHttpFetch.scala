package tools

import play.api.Logging

import java.io.IOException
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{Authenticator, CookieManager, CookiePolicy, InetSocketAddress, PasswordAuthentication, Proxy, ProxySelector, SocketAddress}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger

class RealHttpFetch(proxy: Option[RealHttpFetch.ProxyConfig] = None) extends HttpFetch with Logging {
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
  // films that *are* reachable. The request timeout below is separate — it
  // bounds reading the response, where a slow upstream legitimately needs longer
  // — so trimming connect doesn't cut off slow-but-alive servers.
  private val RequestTimeout = Duration.ofSeconds(30)

  // `CookieManager` makes the client a well-behaved HTTP citizen: any
  // `Set-Cookie` header lands in the in-memory store and is sent back on
  // subsequent requests to the same domain. Multikino's direct path
  // depends on it — the homepage hands out a session cookie that the
  // API call must carry — and other clients are unaffected (cookies
  // are domain-scoped, and the APIs we talk to are otherwise stateless).
  private def buildClient(connectTimeout: Duration): HttpClient = {
    val builder = HttpClient.newBuilder()
      .version(HttpClient.Version.HTTP_1_1)
      .followRedirects(HttpClient.Redirect.NORMAL)
      .connectTimeout(connectTimeout)
      // Trust the JDK defaults PLUS the Certum root that OpenJDK's cacerts omits,
      // so the Certum-rooted cinema sites (Kinomuzeum/artmuseum.pl, Kino
      // Muranów/kinomuranow.pl, sdk.waw.pl) stop failing PKIX path building. See
      // TlsTrust — touching it here also enables AIA intermediate fetching.
      .sslContext(TlsTrust.augmentedContext)
      .cookieHandler(new CookieManager(null, CookiePolicy.ACCEPT_ALL))
    // Route through an authenticated residential proxy when configured (the
    // Decodo static-ISP egress for the cinema sites that Cloudflare-block our
    // datacenter IP). Only the proxied instance carries this; everything else
    // fetches direct.
    proxy.foreach { p =>
      builder.proxy(p.selector).authenticator(p.authenticator)
    }
    builder.build()
  }

  private val underlying   = buildClient(RealHttpFetch.DefaultConnectTimeout)
  // A second client with a much longer connect budget for the handful of hosts
  // whose TLS handshake is pathologically slow (see RealHttpFetch.isSlowTlsHost).
  private val slowTlsClient = buildClient(RealHttpFetch.SlowTlsConnectTimeout)

  /** Pick the client by host: the slow-TLS hosts get the long connect budget,
   *  everything else keeps the tight 5s. Package-private so the spec can assert
   *  the routing (the actual slow-handshake behaviour needs a real server and
   *  can't be reached in a unit test). */
  private[tools] def clientFor(url: String): HttpClient =
    if (RealHttpFetch.isSlowTlsHost(url)) slowTlsClient else underlying

  override def get(url: String): String = get(url, Map.empty)

  /** Raw wire bytes (gunzipped, NOT charset-decoded) so a caller can pick the
   *  right charset itself. See [[HttpFetch.getBytes]] — only the charset-quirky
   *  scrapers (Kino Charlie) use this; everyone else gets the UTF-8 `get`. */
  override def getBytes(url: String): Array[Byte] = {
    val response = clientFor(url).send(buildRequest(url, Map.empty), HttpResponse.BodyHandlers.ofByteArray())
    val code = response.statusCode()
    if (code >= 200 && code < 300) Gunzip.decode(response.body())
    else throw new RuntimeException(s"HTTP $code for GET $url")
  }

  override def get(url: String, headers: Map[String, String]): String =
    sendLogged("GET", url, clientFor(url).send(buildRequest(url, headers), HttpResponse.BodyHandlers.ofByteArray()))

  override def getAsync(url: String): CompletableFuture[String] =
    clientFor(url).sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofByteArray())
      .handle[String] { (response, throwable) =>
        if (throwable != null) {
          logFailure("GET", url, unwrap(throwable))
          throw throwable
        }
        checkStatus("GET", url, response)
      }

  override def post(url: String, body: String, contentType: String = "application/json"): String = {
    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(RequestTimeout)
      .header("Content-Type", contentType)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      // Decoded transparently in `decodeBody` below. See the matching
      // header on `buildRequest` for the GET path.
      .header("Accept-Encoding", "gzip")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()
    sendLogged("POST", url, clientFor(url).send(request, HttpResponse.BodyHandlers.ofByteArray()))
  }

  // ── Logging hooks ─────────────────────────────────────────────────────────
  //
  // Every HTTP client in the app (TMDB, Filmweb, MC, RT, IMDb, every cinema
  // scraper) routes through this class, so failure visibility added here
  // surfaces in every caller at once. Two failure shapes worth seeing:
  //
  //   - **Network / timeout** — the service didn't respond at all (DNS,
  //     connection refused, socket timeout, request timeout). Always WARN —
  //     this is what masks the symptom users hit (e.g. Kino Muza's burst
  //     limiter stalling our detail fetches: no logs, just silently empty
  //     synopses).
  //   - **5xx response** — the service returned an error status. Also WARN.
  //   - **4xx response** — DEBUG only. Several callers (MetacriticClient's
  //     slug probe, RT's URL probe) deliberately use 404 as the "this
  //     slug doesn't exist" signal; WARN-ing every probe miss would flood
  //     the logs with hundreds of expected 404s per refresh tick.

  private def sendLogged(method: String, url: String, send: => HttpResponse[Array[Byte]]): String = {
    val response = try send catch {
      case exception: Throwable =>
        logFailure(method, url, exception)
        throw exception
    }
    checkStatus(method, url, response)
  }

  private def checkStatus(method: String, url: String, response: HttpResponse[Array[Byte]]): String = {
    val code = response.statusCode()
    if (code >= 200 && code < 300) decodeBody(response.body())
    else {
      if (code >= 500) logger.warn(s"HTTP $code from $method $url (server-side)")
      else             logger.debug(s"HTTP $code from $method $url")
      throw new RuntimeException(s"HTTP $code for $method $url")
    }
  }

  /** Decode the raw response bytes to a UTF-8 string. `Gunzip.decode`
   *  loops while the bytes still start with the gzip magic prefix
   *  `1F 8B` — TMDB (or its CDN) ships some responses as
   *  `gzip(gzip(json))` under a single `Content-Encoding: gzip` header,
   *  so one round of decompression isn't enough. Non-gzip and
   *  single-gzip bodies are handled by the same call. */
  private def decodeBody(bytes: Array[Byte]): String =
    new String(Gunzip.decode(bytes), StandardCharsets.UTF_8)

  private def logFailure(method: String, url: String, exception: Throwable): Unit = exception match {
    case _: HttpTimeoutException =>
      logger.warn(s"HTTP $method $url timed out after ${RequestTimeout.toSeconds}s (service not responding)")
    case _ =>
      logger.warn(s"HTTP $method $url failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
  }

  // Async failures come wrapped in `CompletionException`; unwrap so the
  // log line names the real cause (`HttpTimeoutException`, …) rather than
  // the framework wrapper.
  private def unwrap(exception: Throwable): Throwable = exception match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }

  private def buildRequest(url: String, extraHeaders: Map[String, String] = Map.empty): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(RequestTimeout)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      // Advertise gzip — `decodeBody` decompresses transparently. Without
      // this header some CDNs still gzip based on the Chrome-shaped UA we
      // send, so the decode path runs regardless; advertising it makes the
      // negotiation explicit and shrinks the wire payload.
      .header("Accept-Encoding", "gzip")
      .GET()
    extraHeaders.foreach { case (k, v) => builder.header(k, v) }

    decorateBuilder(builder, url).build()
  }

  protected def decorateBuilder(builder: HttpRequest.Builder, url: String): HttpRequest.Builder = builder
}

object RealHttpFetch {

  /** Routes a `RealHttpFetch`'s outbound requests through an authenticated HTTP
   *  proxy — the Decodo static-residential (ISP) egress used for cinema sites
   *  that Cloudflare-block our Fly datacenter IP at the ASN level (Multikino,
   *  biletyna). The proxy IPs are genuine PL ISP (Netia, AS12741), so the target
   *  sees a residential egress and returns 200. Rotates across `ports` (each =
   *  a distinct dedicated IP) for resilience; low volume so any one IP suffices.
   *  See the `reference_decodo_isp_proxy` memory. */
  case class ProxyConfig(host: String, ports: Seq[Int], user: String, password: String) {
    require(ports.nonEmpty, "ProxyConfig needs at least one port")

    // java.net.http disables Basic auth on HTTPS CONNECT tunnels by default
    // (`jdk.http.auth.tunneling.disabledSchemes` = "Basic"). Both our targets are
    // HTTPS, so without clearing it every proxied request 407s. Set before the
    // proxied HttpClient issues its first request. The worker also passes
    // `-Djdk.http.auth.tunneling.disabledSchemes=` as a belt-and-suspenders.
    System.setProperty("jdk.http.auth.tunneling.disabledSchemes", "")

    val selector: ProxySelector = new RotatingProxySelector(host, ports)

    val authenticator: Authenticator = new Authenticator {
      override protected def getPasswordAuthentication: PasswordAuthentication =
        if (getRequestorType == Authenticator.RequestorType.PROXY)
          new PasswordAuthentication(user, password.toCharArray)
        else null // server (non-proxy) auth is none of this proxy's business
    }
  }

  /** Round-robins a fixed proxy host across `ports` (one dedicated IP each), so
   *  load and any per-IP rate-limit risk spread across the pool. Thread-safe. */
  private class RotatingProxySelector(host: String, ports: Seq[Int]) extends ProxySelector {
    private val counter = new AtomicInteger(0)
    override def select(uri: URI): java.util.List[Proxy] = {
      val port = ports(Math.floorMod(counter.getAndIncrement(), ports.size))
      java.util.List.of(new Proxy(Proxy.Type.HTTP, new InetSocketAddress(host, port)))
    }
    override def connectFailed(uri: URI, sa: SocketAddress, e: IOException): Unit = ()
  }

  /** The tight default: a live host's TCP+TLS handshake finishes in well under a
   *  second, so 5s frees a stalled fan-out slot fast. See the comment on the
   *  client builder for why low matters. */
  val DefaultConnectTimeout: Duration = Duration.ofSeconds(5)

  /** The long budget for the slow-TLS hosts below. Sized above the worst
   *  handshake we measured (~27s) with headroom; the response read is bounded
   *  separately by RequestTimeout, so this only stretches the connect phase. */
  val SlowTlsConnectTimeout: Duration = Duration.ofSeconds(40)

  /** Hosts whose TLS handshake is pathologically slow — the TCP connect lands
   *  instantly but the handshake itself takes 20-30s (the server's own latency,
   *  reproducible with curl/openssl, not our cert path). Under the 5s default
   *  every fetch died with HttpConnectTimeoutException, leaving the cinema
   *  perpetually red on /uptime even though the page returns HTTP 200 when given
   *  enough time:
   *    - iluzjon.fn.org.pl — Kino Iluzjon (Filmoteka Narodowa, Warszawa).
   *  Matched by exact host or a dotted sub-domain (so www.iluzjon.fn.org.pl
   *  matches, an unrelated *.fn.org.pl does not). */
  private val SlowTlsHostSuffixes: Set[String] = Set("iluzjon.fn.org.pl")

  def isSlowTlsHost(url: String): Boolean =
    // Swallow a malformed URL here so routing never changes failure semantics —
    // the subsequent buildRequest's own URI.create throws it the same way.
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.exists { host =>
      val lowerHost = host.toLowerCase
      SlowTlsHostSuffixes.exists(suffix => lowerHost == suffix || lowerHost.endsWith("." + suffix))
    }
}
