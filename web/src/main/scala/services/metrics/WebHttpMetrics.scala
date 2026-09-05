package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import play.api.mvc.RequestHeader
import play.api.routing.Router

/**
 * `kinowo_web_http_requests_total` + `kinowo_web_http_request_duration_seconds`
 * — the web tier's OWN request rate, error rate and latency, recorded by
 * [[modules.HttpMetricsFilter]] on every inbound request.
 *
 * WHY THIS EXISTS: these two panels used to be fed by Fly's PROXY metrics
 * (`fly_app_http_responses_count`, `fly_app_http_response_time_seconds_bucket`)
 * out of Fly's managed Prometheus. Those tokens are revoked and cannot be
 * reissued, so every request-rate / error-rate / latency panel on the web tier
 * went blank and stayed blank. `/metrics` is now scraped directly (port 9000
 * over the WireGuard peer) and already publishes ~46 families — all JVM,
 * process and business gauges, NOTHING about serving. This closes that: it is
 * the only signal that says the site is answering at all, and the only one that
 * separates "slow" from "erroring".
 *
 * Registered on the SAME registry as the JVM/process collectors
 * ([[WebJvmMetrics.registry]]) and served by the SAME `/metrics` endpoint —
 * a second registry or a second port would just be one more thing to scrape,
 * and the existing scrape config already points here.
 *
 * ── LABEL CARDINALITY — the load-bearing decision ──────────────────────────
 *
 * Every label here is drawn from a CLOSED set. That is not tidiness; the
 * monitoring box has 4 GB and a Prometheus series lives for the retention
 * window, so one unbounded label is a slow leak that only shows up as an OOM
 * weeks later.
 *
 *  - `route` is Play's matched ROUTE PATTERN (`/:city/movie/:slug`), never
 *    `request.path`. The raw path is unbounded twice over: one series per film
 *    slug (thousands, growing weekly) times one per city. The pattern set is
 *    exactly the number of lines in `web/src/main/resources/routes` — bounded
 *    by a file a human edits. Anything the router did NOT match (404s: crawler
 *    probes, `/wp-admin/…`, vulnerability scanners — attacker-controlled and
 *    therefore infinite) collapses into the single bucket `other`.
 *  - `method` is clamped to the nine real HTTP verbs. The request line is
 *    client-controlled, so an un-clamped `request.method` is the same
 *    unbounded-label hole as the raw path, just less obvious.
 *  - `status` is the CLASS (`2xx`…`5xx`), not the code. The two questions
 *    these series answer are "what fraction of requests failed" and "is the
 *    site up", and both read at class granularity — an alert on the code would
 *    be written `status=~"5.."` anyway. The class is also the taxonomy the
 *    worker's `kinowo_worker_http_total` already uses (`http_4xx` / `http_5xx`),
 *    so one Grafana panel shape reads either side of the fleet. THE COST,
 *    stated so nobody rediscovers it in an incident: you cannot tell 401 from
 *    404, or 429 from 400, off this metric — and `/api/me` answers 401 to every
 *    anonymous caller, so the 4xx line has a large, permanent, healthy floor.
 *    Do NOT "fix" that by widening this label to the raw code (it multiplies
 *    every route × method series by ~3); add a separate, single-purpose counter
 *    for the specific code that turned out to matter.
 *  - `country` is constant for the process (one web deployment serves one
 *    country) so it costs no cardinality, and it lets these series join the
 *    worker's per-country `kinowo_worker_*` series on the shared dashboards —
 *    the same reason `kinowo_web_movies_served` carries it.
 *
 * Deliberately NOT seeded to zero across the label grid, unlike
 * [[WorkerHttpMetrics]]: that grid is route × method × status ≈ 50 × 9 × 5,
 * and materialising ~2,000 permanently-zero series to avoid a few
 * appears-on-first-request Grafana lines is the wrong trade. The histogram
 * carries no `status` label for the same reason — latency is asked per route,
 * and a status dimension would multiply a 14-series-per-combination metric.
 */
class WebHttpMetrics(registry: PrometheusRegistry, country: String) {

  // The client auto-appends `_total`, so the name is declared without it.
  private val requests: Counter = Counter.builder()
    .name("kinowo_web_http_requests")
    .help("Inbound HTTP requests the web tier answered since boot, by country, method, matched " +
      "route pattern (`other` when no route matched — 404s and scanner noise) and response status " +
      "class. The `/metrics` scrape itself is excluded.")
    .labelNames("country", "method", "route", "status")
    .register(registry)

  private val duration: Histogram = Histogram.builder()
    .name("kinowo_web_http_request_duration_seconds")
    .help("Time from receiving a request to producing its response header, by country, method and " +
      "matched route pattern. Excludes body streaming, so an SSE stream records the time to first " +
      "byte rather than the life of the connection.")
    .labelNames("country", "method", "route")
    .classicOnly()
    .classicUpperBounds(WebHttpMetrics.Buckets*)
    .register(registry)

  private val responseBytes: Histogram = Histogram.builder()
    .name("kinowo_web_http_response_bytes")
    .help("Size of the response body actually put on the wire, by country, method and matched " +
      "route pattern. Measured AFTER gzip, so it is what the visitor downloads. Only recorded " +
      "when the response declares a Content-Length: a streamed body (SSE) has none, and guessing " +
      "one would be worse than the gap.")
    .labelNames("country", "method", "route")
    .classicOnly()
    .classicUpperBounds(WebHttpMetrics.ByteBuckets*)
    .register(registry)

  /** Record one answered request. Called once per request by
   *  [[modules.HttpMetricsFilter]]; all three metric objects are thread-safe.
   *
   *  `responseLength` is the wire size when the response declares one. It is
   *  separate from duration because THE TWO DO NOT MOVE TOGETHER, which is the
   *  whole reason this metric exists: `/us/los-angeles/movies` answers its
   *  header in ~50 ms and then hands the visitor 1.6 MB, so every latency
   *  percentile reads healthy while the page takes seconds to arrive over a
   *  real connection. Timing the body instead would have been the wrong fix --
   *  it would put the SSE streams' multi-minute lives into the latency tail. */
  def record(request: RequestHeader, status: Int, durationSeconds: Double,
             responseLength: Option[Long] = None): Unit = {
    val method = WebHttpMetrics.methodLabel(request.method)
    val route  = WebHttpMetrics.routeLabel(request)
    requests.labelValues(country, method, route, WebHttpMetrics.statusClass(status)).inc()
    duration.labelValues(country, method, route).observe(durationSeconds)
    responseLength.foreach(bytes => responseBytes.labelValues(country, method, route).observe(bytes.toDouble))
  }
}

object WebHttpMetrics {

  /** Histogram boundaries, in seconds, tuned for THIS workload rather than the
   *  client's generic defaults. The two populations are the server-rendered
   *  HTML pages (`/:city/` is 200+ cards and lands in the tens-to-hundreds of
   *  ms) and the JSON endpoints (`/api/catalog`, `/api/me/state` — single-digit
   *  ms), so the buckets are dense from 5 ms to 500 ms where every percentile
   *  worth alerting on lives. The tail out to 10 s exists because the slow
   *  cases we actually care about — a cold read model, a gzip of a 4 MB body —
   *  land there, and a histogram cannot tell you anything above its last
   *  finite bound. Changing these resets every stored bucket series: Prometheus
   *  cannot re-bucket history, so `histogram_quantile` over the boundary will
   *  read wrong until the old series age out. */
  val Buckets: Seq[Double] =
    Seq(0.005, 0.01, 0.025, 0.05, 0.1, 0.15, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0)

  /** Histogram boundaries for response size, in BYTES, and spread wide because
   *  the populations here are three orders of magnitude apart: a `/api/me`
   *  answer is a few hundred bytes, a typical city listing is ~200 KB gzipped
   *  (Poznan 209 KB, Norwich 188 KB), and the largest markets are past a
   *  megabyte -- measured 2026-09-04: London 1.27 MB, New York 1.31 MB,
   *  Chicago 1.17 MB, Los Angeles 1.66 MB. The bounds are dense either side of
   *  512 KB because that is where "large page" turns into "page a phone on a
   *  train will not finish", and the 8 MB top exists so a runaway render lands
   *  somewhere finite rather than in +Inf.
   *
   *  ⚠️ Changing these resets every stored bucket series, exactly as for
   *  [[Buckets]]. */
  val ByteBuckets: Seq[Double] =
    Seq(1024, 4096, 16384, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608)

  /** Label value for a request the router could not resolve. One bucket for
   *  every unmatched path — see the cardinality note on the class. */
  val UnmatchedRoute: String = "other"

  /** The route pattern of the exposition endpoint itself, excluded from
   *  measurement. */
  val MetricsRoute: String = "/metrics"

  /** The HTTP verbs allowed as a `method` label value. Anything else — and the
   *  request line is whatever the client typed — folds into [[UnmatchedRoute]]'s
   *  namesake bucket. */
  private val KnownMethods: Set[String] =
    Set("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "TRACE", "CONNECT")

  /** Rewrites Play's generated path syntax into the spelling used in the routes
   *  file: `/$city<[^/]+>/movie/$slug<[^/]+>` → `/:city/movie/:slug`. The label is
   *  read by a human in Grafana, and the raw form is unreadable; the regex body
   *  also carries no information the pattern name doesn't. */
  private val DynamicPart = """\$([^<]+)<[^>]*>""".r

  /** The route pattern Play matched, or [[UnmatchedRoute]].
   *
   *  The router attaches `HandlerDef` BEFORE the filter chain runs, which is
   *  what makes this readable from an `EssentialFilter` at all — and is the
   *  reason this is the only correct source for the label. If a future Play
   *  version stops populating the attribute, every request silently collapses
   *  into `other`; `HttpMetricsFilterSpec` is the tripwire for that. */
  def routeLabel(request: RequestHeader): String =
    request.attrs
      .get(Router.Attrs.HandlerDef)
      .map(handler => DynamicPart.replaceAllIn(handler.path, m => ":" + m.group(1)))
      .getOrElse(UnmatchedRoute)

  /** Clamp a client-supplied verb to the closed set above. */
  def methodLabel(method: String): String =
    if (KnownMethods.contains(method)) method else UnmatchedRoute

  /** `2xx`/`3xx`/`4xx`/`5xx` — see the class comment for why the class and not
   *  the code. */
  def statusClass(status: Int): String = status / 100 match {
    case 1 => "1xx"
    case 2 => "2xx"
    case 3 => "3xx"
    case 4 => "4xx"
    case 5 => "5xx"
    case _ => UnmatchedRoute
  }

  /** Whether a request should be counted at all.
   *
   *  Excludes the exposition endpoint: Prometheus scrapes it every few seconds
   *  forever, so counting it makes the request-rate panel mostly a picture of
   *  the monitoring system, and a scrape that gets slow inflates the very
   *  latency histogram you would be reading to diagnose it. Both the matched
   *  route AND the raw path are checked — the raw check is what still holds if
   *  the route is ever renamed or the request reaches us unrouted. */
  def isMeasured(request: RequestHeader): Boolean =
    routeLabel(request) != MetricsRoute && request.path != MetricsRoute
}
