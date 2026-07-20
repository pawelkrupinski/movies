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
  // per-host (see RealHttpFetch.HostPolicies): the default 30s, but a stall-prone
  // host gets a tight budget so one stalling origin can't pin a fan-out slot for
  // the full default. The connect budget is per-host the same way.

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

  // One HttpClient per distinct connect budget the host-policy table can ask for
  // (plus the default). Building a client isn't free and the distinct connect
  // timeouts are few, so prebuild them all and pick per host by connect budget.
  private val clientsByConnectTimeout: Map[Duration, HttpClient] =
    (RealHttpFetch.DefaultConnectTimeout +: RealHttpFetch.HostPolicies.map(_.connectTimeout))
      .distinct
      .map(connectTimeout => connectTimeout -> buildClient(connectTimeout))
      .toMap

  /** Pick the client whose connect budget matches the host policy: the slow-TLS
   *  hosts get their long handshake budget, everything else the tight default.
   *  Package-private so the spec can assert the routing (the real slow-handshake
   *  behaviour needs a live server and can't be reached in a unit test). */
  private[tools] def clientFor(url: String): HttpClient =
    clientsByConnectTimeout(RealHttpFetch.connectTimeoutFor(url))

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
  //   - **403 / 429 response** — WARN. A soft-block / rate-limit (a Cloudflare
  //     or datacenter-IP block) is a real outage the enrichment sources hit; at
  //     DEBUG it was INVISIBLE at the prod INFO level, hiding e.g. a Filmweb
  //     block for days. Also classified as a failure by MonitoringHttpFetch, so
  //     it now surfaces on /uptime instead of reading green.
  //   - **other 4xx (esp. 404)** — DEBUG only. Several callers (MetacriticClient's
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
      if (code >= 500)                     logger.warn(s"HTTP $code from $method $url (server-side)")
      else if (code == 403 || code == 429) logger.warn(s"HTTP $code from $method $url (blocked/throttled)")
      else                                 logger.debug(s"HTTP $code from $method $url")
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

  // ── Per-host network policy ────────────────────────────────────────────────
  // A host that needs anything other than the default connect/request timeouts
  // gets ONE row in `HostPolicies` below — no bespoke predicate, constant, or
  // if-branch per host. `connectTimeoutFor`, `requestTimeoutFor`, and the client
  // selection all read that one table (first matching row wins). Add a host =
  // append a row; nothing else changes.

  /** The tight default connect budget: a live host's TCP+TLS handshake finishes
   *  in well under a second, so 5s frees a stalled fan-out slot fast. See the
   *  comment on the client builder for why low matters. */
  val DefaultConnectTimeout: Duration = Duration.ofSeconds(5)

  /** The default per-request (response-read) budget: a slow-but-alive upstream
   *  legitimately needs longer than the connect phase, so this stays generous. */
  val DefaultRequestTimeout: Duration = Duration.ofSeconds(30)

  /** A per-host override of the network timeouts. `connectTimeout` bounds the
   *  TCP+TLS handshake (the JVM throws HttpConnectTimeoutException if either runs
   *  over); `requestTimeout` bounds the response read. Each defaults to the
   *  matching RealHttpFetch default, so a row overrides only what it names.
   *  `hostSuffixes` matches by exact host or a dotted sub-domain (see
   *  `hostMatches`), so `www.x` matches `x` but an unrelated `*.y` does not.
   *
   *  `minRequestInterval` is the OUTBOUND pace [[RateLimitedHttpFetch]] holds the
   *  whole fleet to for this host — the minimum gap between two requests to it,
   *  across every thread. `None` (the default) means unpaced: the host absorbs
   *  our natural concurrency, so only a host we structurally out-run names one.
   *
   *  `paceKnob` names an [[Env]] key that overrides `minRequestInterval` at
   *  RUNTIME. A host whose tolerance we don't know (nobody publishes one) can
   *  only be tuned empirically — push the pace until the 429s stop — and doing
   *  that through redeploys costs a worker restart and a cold JVM per attempt.
   *  Naming a knob makes the pace flippable from `/admin/config` mid-flight: the
   *  resolve below reads it per request, so the next request uses the new value.
   *  Still DATA, not an if-branch — any future host can name its own key. */
  final case class HostPolicy(
    hostSuffixes: Set[String],
    connectTimeout: Duration = DefaultConnectTimeout,
    requestTimeout: Duration = DefaultRequestTimeout,
    minRequestInterval: Option[Duration] = None,
    paceKnob: Option[String] = None,
    // The ceiling on how long a request may BLOCK on this host's pace gate
    // (`minRequestInterval` only sets the gap; a burst still queues behind it).
    // `None` (the default) means block for the whole backlog, however long — the
    // right behaviour for a synchronous sweep (Filmstarts). A QUEUED caller whose
    // work reschedules cheaply (a UK Flicks ScrapeChunk) sets one so the gate
    // sheds backlog past it via [[PaceGateBackpressureException]] instead of
    // pinning a worker — see RateLimitedHttpFetch's paced().
    maxGateWait: Option[Duration] = None,
  )

  /** The per-host policy table — the single place a host earns a non-default
   *  timeout. First matching row wins. */
  val HostPolicies: Seq[HostPolicy] = Seq(
    // Helios's REST API (restapi.helios.pl) — both the screening/event LIST that
    // HeliosClient fetches per cinema AND the per-screen/detail enrichment. On
    // 2026-07-05 the host degraded to answering in ~5-7s: UNDER the old 8s budget,
    // so calls SUCCEEDED (slow) instead of timing out — the HostCircuitBreaker only
    // trips on timeouts/5xx, so it never opened, and ~30 Helios venues each holding
    // a fetch ~5-7s pinned the worker's CPU and drained shared-cpu credit to the
    // floor for hours. 4s is above the ~1-3s a healthy call takes but BELOW the slow
    // tail, so a degraded host now TIMES OUT → the breaker opens → Helios is skipped
    // for the cooldown (it degrades to its NUXT repertoire; detail only enriches).
    HostPolicy(Set("restapi.helios.pl"), requestTimeout = Duration.ofSeconds(4)),

    // Metacritic's slow Cloudflare-fronted origin. It answers our datacenter
    // egress in ~1-6s on a good day (≈35% of metascore fetches legitimately take
    // >5s, so the 8s budget above would cut them), but a Cloudflare challenge or
    // hang can stretch a page past the 30s default and pin a rating-refresh slot.
    // 15s frees the slot ~2× sooner while clearing the healthy tail; metascore
    // enrichment is best-effort, retried next cycle, so an occasional cut is free.
    HostPolicy(Set("metacritic.com"), requestTimeout = Duration.ofSeconds(15)),

    // The Fly Prometheus CPU-credit poll (CpuCreditPoller, on its own thread). A
    // healthy query answers in ~1s; the 30s default let it hang the full budget
    // when the worker was starved at the credit floor. A dropped poll just skips
    // one balance sample, retried next tick — never load-bearing.
    HostPolicy(Set("api.fly.io"), requestTimeout = Duration.ofSeconds(5)),

    // Kino Iluzjon (Filmoteka Narodowa). Its TLS handshake is pathologically slow
    // — the TCP connect lands instantly but the handshake itself takes 20-30s (the
    // server's own latency, reproducible with openssl, not our cert path). Under
    // the 5s default connect budget every fetch died with HttpConnectTimeout-
    // Exception, leaving it perpetually red on /uptime though the page returns 200
    // given time. 40s covers the handshake; the read budget stays the default.
    HostPolicy(Set("iluzjon.fn.org.pl"), connectTimeout = Duration.ofSeconds(40)),

    // Filmstarts (Webedia DE). Germany's 1,533 venues × 7 days = ~10.7k requests
    // per sweep onto ONE origin, and with no pacing the worker's fan-out delivers
    // them in bursts the host answers with 429. That was our steady state, not an
    // anomaly: ThrottledHttpFetch's reactive 5s gate then parked the venue past
    // AdaptiveTimeoutScraper's budget, so the scrape was cut and DE went stale.
    // 250ms (~4 req/s) was that starting pace, and the 429s never stopped: one
    // live worker.log carried 3,118 of them, and 429 was the ONLY HTTP status in
    // it — no 403s, no 5xx, so this is purely self-inflicted, not a Fly-ASN block.
    // The cost is worse than the lost request: a 429 trips
    // HostCircuitBreakerHttpFetch, whose 60s open window fast-fails all 7 of a
    // venue's day requests at once ("all 7 showtime requests ... failed"), and
    // RetryWithBackoff then re-runs the whole cinema up to 3x. So the fast pace
    // spent its budget three times over on requests that could never land, and
    // only ~30% of scrapes were succeeding.
    // The pace-report logging (see ThrottledHttpFetch, surfaced at INFO in the
    // worker's logback-base.xml) turned that search into measurement. 500ms was
    // 100% clean overnight but only ~95% under German morning load — a steady
    // ~4-5% throttle with bursts to ~35%, i.e. Filmstarts' tolerance sits right
    // around our 2 req/s. Halving again to 1000ms (~1 req/s) drops under it for
    // 0 throttled. The cost is the sweep: 1,533 venues × 7 day-pages × 1000ms =
    // ~179min, which no longer fits a 2h cadence — so DE moved to a 180min cadence
    // in lockstep (fly.worker.de.toml). The two are coupled: pace sets sweep
    // length, cadence sets the budget, and WorkerScrapeCadenceConfigSpec asserts
    // sweep ≤ cadence so neither can drift alone. KINOWO_FILMSTARTS_PACE_MS still
    // overrides this live (per request) for re-tuning without a redeploy.
    HostPolicy(
      Set("filmstarts.de"),
      minRequestInterval = Some(Duration.ofMillis(1000)),
      paceKnob           = Some("KINOWO_FILMSTARTS_PACE_MS"),
    ),

    // Flicks (www.flicks.co.uk) — the UK's 843 venues, each fanning out one
    // sessions request per advertised day of a months-long horizon (~100k requests
    // per cycle) onto ONE origin. Like Filmstarts this was UNPACED, so the
    // 4-worker pool's fan-out delivered those in bursts Flicks answered with 429 —
    // panel-14 of kinowo-worker-diag showed a steady ~3-4% throttle spiking to
    // ~100% (pace-report: "8 requests, 8 throttled, pace=unpaced"). Worse than
    // Filmstarts', Flicks' limiter answers with Retry-After: 300-600s, so a burst
    // costs whole venue-days: ThrottledHttpFetch's 4 retries all fell inside that
    // window and the chunk was dropped (data loss + retry churn back onto the
    // queue). The 429s were purely self-inflicted (no 403s → not a Fly-ASN block),
    // so pacing the origin is the fix. 200ms (~5 req/s) is just under the ~6.4
    // req/s the unpaced pool averaged, but — the point — it SERIALISES the fan-out
    // so the concurrent bursts that trip the limiter never form. It lengthens the
    // ~100k-request sweep to ~333min, so fly.worker.uk.toml's cadence moved to
    // 420min in lockstep (the same pace↔cadence coupling DE has;
    // WorkerScrapeCadenceConfigSpec locks both). KINOWO_FLICKS_PACE_MS retunes it
    // live: if 200ms still throttles, step it down (and bump the cadence to match).
    HostPolicy(
      Set("flicks.co.uk"),
      minRequestInterval = Some(Duration.ofMillis(200)),
      paceKnob           = Some("KINOWO_FLICKS_PACE_MS"),
      // Cap the per-request gate wait. The reaper plans chunks in waves far
      // faster than the 5 req/s gate drains, so without this a worker claimed a
      // slot tens of seconds out and BLOCKED on it — the whole backlog counted
      // as the ScrapeChunk's wall-clock, pinning task-duration p99 at the
      // histogram ceiling in ~11-16min waves while completions stalled. 20s is
      // well above the sub-second steady-state wait but far under both the old
      // ~120s peaks and the 15min chunk-run stale timeout, so an over-backed gate
      // now sheds the overflow to a reschedule (ScrapeChunkHandler) that the
      // reaper's backoff spreads past the wave, instead of holding a worker.
      maxGateWait        = Some(Duration.ofSeconds(20)),
    ),
  )

  /** True when `url`'s host matches one of `suffixes` (exact host or a dotted
   *  sub-domain, so www.x matches x but an unrelated *.y does not). Swallows a
   *  malformed URL so routing never changes failure semantics — the subsequent
   *  buildRequest's own URI.create throws it the same way. */
  private def hostMatches(url: String, suffixes: Set[String]): Boolean =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.exists { host =>
      val lowerHost = host.toLowerCase
      suffixes.exists(suffix => lowerHost == suffix || lowerHost.endsWith("." + suffix))
    }

  /** The first host policy matching `url`, if any. */
  private def policyFor(url: String): Option[HostPolicy] =
    HostPolicies.find(policy => hostMatches(url, policy.hostSuffixes))

  /** The minimum gap between two outbound requests to `url`'s host, if that host
   *  is paced. `None` — the default for every host without a row naming one —
   *  means [[RateLimitedHttpFetch]] passes the call straight through.
   *
   *  Resolved PER REQUEST rather than baked into the table at class-init, so a
   *  `paceKnob` flip on `/admin/config` takes effect without a worker restart.
   *  The read is a map lookup against the override cache — cheap enough to sit
   *  on the request path, and it only runs for the few hosts that are paced. */
  def requestIntervalFor(url: String): Option[Duration] =
    policyFor(url).flatMap(tunedInterval)

  /** The cap on how long a request to `url`'s host may block on its pace gate
   *  before the fetch sheds the backlog with a [[PaceGateBackpressureException]].
   *  `None` for every host without a row naming one — the gate then blocks for
   *  the whole backlog, the correct behaviour for a synchronous sweep. */
  def maxGateWaitFor(url: String): Option[Duration] =
    policyFor(url).flatMap(_.maxGateWait)

  /** A policy's live pace: its knob's current value if it names one, else the
   *  compiled-in default. `Env.positiveLong` ignores a non-positive or
   *  unparseable override, so a fat-fingered `0` falls back to the default
   *  rather than silently unpacing a host we know we out-run. */
  private def tunedInterval(policy: HostPolicy): Option[Duration] =
    policy.paceKnob match {
      case None      => policy.minRequestInterval
      case Some(key) =>
        val compiledIn = policy.minRequestInterval.map(_.toMillis).getOrElse(0L)
        Some(Duration.ofMillis(Env.positiveLong(key, compiledIn)))
    }

  /** The connect (TCP+TLS handshake) budget for `url`: the matching host policy's,
   *  else the tight default. */
  def connectTimeoutFor(url: String): Duration =
    policyFor(url).map(_.connectTimeout).getOrElse(DefaultConnectTimeout)

  /** The per-request (response-read) budget for `url`: the matching host policy's,
   *  else the generous default. */
  def requestTimeoutFor(url: String): Duration =
    policyFor(url).map(_.requestTimeout).getOrElse(DefaultRequestTimeout)
}
