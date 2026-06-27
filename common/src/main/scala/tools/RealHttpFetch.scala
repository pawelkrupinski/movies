package tools

import play.api.Logging

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse, HttpTimeoutException}
import java.net.{Authenticator, CookieManager, CookiePolicy, InetSocketAddress, PasswordAuthentication, ProxySelector}
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.util.concurrent.CompletableFuture
import scala.concurrent.duration.FiniteDuration

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
  // films that *are* reachable. The request (response-read) timeout is separate
  // and bounds reading the response, where a slow upstream legitimately needs
  // longer — so trimming connect doesn't cut off slow-but-alive servers. It is
  // per-host (see RealHttpFetch.requestTimeoutFor): the default 30s, but the
  // fast-fail hosts get a tight budget so one stalling enrichment origin can't
  // pin a fan-out slot for the full default.

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
    else throw new HttpStatusException(code, "GET", url, retryAfterOf(response))
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
      .timeout(RealHttpFetch.requestTimeoutFor(url))
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
      // Typed so a decorator (ThrottledHttpFetch) can react to 429 + Retry-After.
      // Message shape is unchanged, so MonitoringHttpFetch's 5xx classifier and
      // any message-matching caller keep working.
      throw new HttpStatusException(code, method, url, retryAfterOf(response))
    }
  }

  /** The server's `Retry-After` hint, if any (delta-seconds form). */
  private def retryAfterOf(response: HttpResponse[Array[Byte]]): Option[FiniteDuration] =
    HttpStatusException.parseRetryAfter(Option(response.headers().firstValue("Retry-After").orElse(null)))

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
      logger.warn(s"HTTP $method $url timed out after ${RealHttpFetch.requestTimeoutFor(url).toSeconds}s (service not responding)")
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
      .timeout(RealHttpFetch.requestTimeoutFor(url))
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
   *  sees a residential egress and returns 200.
   *
   *  Holds the full egress pool — `ports` is one Decodo IP each. [[perPort]]
   *  yields a config pinned to each ([[pinnedTo]]); `StickyShardHttpFetch` fans a
   *  client's venues across those per-IP shards, keyed by venue URL so each venue
   *  sticks to one IP. Stickiness matters because Multikino's session cookie is
   *  bound to the egress IP — a venue's homepage-warm + API retry must share an
   *  IP (rotation warmed on one IP, retried on another → still 401, verified
   *  2026-06-16) — and each IP warms its own session once, reused across the
   *  venues routed to it. Spreading matters because funnelling all proxied auth
   *  through one IP tripped Decodo's "too many authentication attempts. Limit: 3"
   *  cap and rolled all proxied traffic to Zyte on 2026-06-16. See the
   *  `reference_decodo_isp_proxy` memory. */
  case class ProxyConfig(host: String, ports: Seq[Int], user: String, password: String) {
    require(ports.nonEmpty, "ProxyConfig needs at least one port")

    // java.net.http disables Basic auth on HTTPS CONNECT tunnels by default
    // (`jdk.http.auth.tunneling.disabledSchemes` = "Basic"). Both our targets are
    // HTTPS, so without clearing it every proxied request 407s. Set before the
    // proxied HttpClient issues its first request. The worker also passes
    // `-Djdk.http.auth.tunneling.disabledSchemes=` as a belt-and-suspenders.
    System.setProperty("jdk.http.auth.tunneling.disabledSchemes", "")

    /** This config's sticky egress: the sole port of a [[pinnedTo]] config, or
     *  `ports.head` for the full pool (whose selector is unused — clients always
     *  pin first). */
    val port: Int = ports.head
    val selector: ProxySelector =
      ProxySelector.of(new InetSocketAddress(host, port))

    /** A copy pinned to a single egress IP from this pool — one shard's egress.
     *  Its own [[RealHttpFetch]] gives it its own cookie jar, so a sharded proxy
     *  can warm + reuse one Multikino session per IP while spreading venues across
     *  the pool rather than hammering one IP past Decodo's "Limit: 3" cap. */
    def pinnedTo(port: Int): ProxyConfig = {
      require(ports.contains(port), s"port $port is not one of the pool ports $ports")
      copy(ports = Seq(port))
    }

    /** One pinned config per pool port — the per-IP egresses a sharded proxy
     *  (see `StickyShardHttpFetch`) fans venues across. */
    def perPort: Seq[ProxyConfig] = ports.map(pinnedTo)

    val authenticator: Authenticator = new Authenticator {
      override protected def getPasswordAuthentication: PasswordAuthentication =
        if (getRequestorType == Authenticator.RequestorType.PROXY)
          new PasswordAuthentication(user, password.toCharArray)
        else null // server (non-proxy) auth is none of this proxy's business
    }
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

  /** The default per-request (response-read) budget: a slow-but-alive upstream
   *  legitimately needs longer than the connect phase, so this stays generous. */
  val DefaultRequestTimeout: Duration = Duration.ofSeconds(30)

  /** The tight per-request budget for the fast-fail hosts below — well above the
   *  ~1-3s a healthy response takes, but far under the 30s default so a stalling
   *  origin frees its fan-out slot fast instead of pinning it. */
  val FastFailRequestTimeout: Duration = Duration.ofSeconds(8)

  /** Hosts that must fast-fail when they stall rather than hold a ParallelDetail-
   *  Fetch slot for the full DefaultRequestTimeout. The TCP+TLS connect is fine;
   *  it's the response read that hangs.
   *    - restapi.helios.pl — Helios's per-screen/detail REST API. On 2026-06-23
   *      its detail endpoints started hanging ~30s for our datacenter egress;
   *      with the fan-out cap at 2 and many Helios venues × screens, each 30s
   *      hang ballooned Helios scrapes from ~2s to 100s+ and drained the worker's
   *      shared-cpu credit into a throttle spiral. These endpoints only ENRICH —
   *      HeliosClient degrades to its NUXT repertoire when REST is unavailable —
   *      so dropping a slow call costs at most some screen-name detail, never the
   *      listing.
   *  Matched by exact host or a dotted sub-domain, like SlowTlsHostSuffixes. */
  private val FastFailHostSuffixes: Set[String] = Set("restapi.helios.pl")

  /** A middle budget — above the fast-fail 8s but well under the 30s default —
   *  for a slow-but-alive Cloudflare-fronted origin. www.metacritic.com answers
   *  our datacenter egress in ~1-6s on a good day (≈35% of metascore fetches
   *  legitimately take >5s), but a Cloudflare challenge or hang can stretch a
   *  single page well past that and pin a rating-refresh slot for the full 30s.
   *  Capping at 15s frees the slot ~2× sooner while staying clear of the
   *  healthy tail. Metascore enrichment is best-effort — a dropped fetch just
   *  means no score this cycle, retried next — so an occasional cut costs
   *  nothing. The 8s fast-fail budget would be too tight (it'd cut legit slow
   *  pages); MC needs its own tier. */
  val MetacriticRequestTimeout: Duration = Duration.ofSeconds(15)

  private val MetacriticHostSuffixes: Set[String] = Set("metacritic.com")

  /** True when `url`'s host matches one of `suffixes` (exact host or a dotted
   *  sub-domain, so www.x matches x but an unrelated *.y does not). Swallows a
   *  malformed URL so routing never changes failure semantics — the subsequent
   *  buildRequest's own URI.create throws it the same way. */
  private def hostMatches(url: String, suffixes: Set[String]): Boolean =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.exists { host =>
      val lowerHost = host.toLowerCase
      suffixes.exists(suffix => lowerHost == suffix || lowerHost.endsWith("." + suffix))
    }

  def isSlowTlsHost(url: String): Boolean = hostMatches(url, SlowTlsHostSuffixes)

  def isFastFailHost(url: String): Boolean = hostMatches(url, FastFailHostSuffixes)

  def isMetacriticHost(url: String): Boolean = hostMatches(url, MetacriticHostSuffixes)

  /** The per-request (response-read) timeout for `url`: the tight fast-fail
   *  budget for a stall-prone enrichment host, the 15s Metacritic cap for MC's
   *  slow Cloudflare origin, the generous default otherwise. */
  def requestTimeoutFor(url: String): Duration =
    if (isFastFailHost(url)) FastFailRequestTimeout
    else if (isMetacriticHost(url)) MetacriticRequestTimeout
    else DefaultRequestTimeout
}
