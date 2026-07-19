package tools

/**
 * The category a single outbound HTTP attempt resolved to, as a stable metric
 * label value. One attempt → one outcome; retries are separate attempts (see
 * [[CountingHttpFetch]]), so a request throttled three times then succeeding
 * counts as three `Http429` + one `Success`.
 *
 * The values are the failure taxonomy the scraper actually hits — rate limiting,
 * 4xx/5xx, timeouts, connection-layer errors — plus `Success` so a consumer can
 * form the ratio (the denominator problem the pace-report already taught us).
 * Kept in `common` because the classification is generic; the worker supplies
 * the [[HttpOutcomeRecorder]] that turns an outcome into a Prometheus counter
 * increment, so `common` stays free of any metrics dependency.
 */
object HttpOutcome {
  val Success         = "success"
  val Http400         = "http_400"          // malformed request / rejected query
  val Http401         = "http_401"          // bad or missing API key (TMDB, OMDb)
  val Http403         = "http_403"          // BLOCKED — Cloudflare, datacentre-IP ban
  val Http404         = "http_404"          // not found — mostly slug probing, expected
  val Http410         = "http_410"          // listing withdrawn
  val Http429         = "http_429"          // rate limited
  val Http4xx         = "http_4xx"          // the residual 4xx bucket (405/409/422/451/…)
  val Http5xx         = "http_5xx"          // server errors
  val Timeout         = "timeout"           // connect or read/request timeout
  val ConnectionError = "connection_error"  // refused / unknown host / reset / TLS
  val Other           = "other"             // anything unclassified (incl. 3xx that surfaced as an error)

  /**
   * The 4xx codes this workload produces often enough to be worth their own
   * series; everything else in the 400-499 range folds into [[Http4xx]].
   *
   * The split exists because "4xx" lumps together two opposite meanings. A 404
   * is usually the system working as designed — the RT/Metacritic resolvers
   * probe up to 4 candidate slugs per title and the wrong ones 404 — so a large,
   * steady 404 line is normal. A 403 is the opposite: a host refusing us
   * (Cloudflare, a datacentre-IP ban), which is an outage in the making. Reading
   * one aggregate line, a rising 403 is invisible behind the 404 noise floor.
   *
   * 429 sits here too so the whole 4xx range classifies through one lookup;
   * it keeps its own long-standing label value.
   */
  private val Named4xx: Map[Int, String] =
    Map(400 -> Http400, 401 -> Http401, 403 -> Http403, 404 -> Http404, 410 -> Http410, 429 -> Http429)

  /** Every outcome, for seeding the metric at 0 so no Grafana gap opens before a
   *  category first fires. */
  val all: Seq[String] =
    Seq(Success, Http400, Http401, Http403, Http404, Http410, Http429, Http4xx, Http5xx, Timeout, ConnectionError, Other)

  /** Classify a thrown failure. Order matters: [[HttpStatusException]] carries a
   *  status; `HttpConnectTimeoutException` is a subtype of `HttpTimeoutException`
   *  (so the timeout case catches both); the connection-layer errors are all
   *  `IOException` subtypes, checked before the generic `IOException` fallback. */
  def classify(t: Throwable): String = t match {
    case e: HttpStatusException if e.code >= 400 && e.code < 500 => Named4xx.getOrElse(e.code, Http4xx)
    case e: HttpStatusException if e.code >= 500 && e.code < 600 => Http5xx
    case _: HttpStatusException                                => Other
    case _: java.net.http.HttpTimeoutException                => Timeout
    case _: java.net.ConnectException                         => ConnectionError
    case _: java.net.UnknownHostException                     => ConnectionError
    case _: java.net.SocketException                          => ConnectionError
    case _: javax.net.ssl.SSLException                        => ConnectionError
    case _: java.io.IOException                               => ConnectionError
    case _                                                    => Other
  }
}

/**
 * Sink for classified HTTP attempt outcomes. The worker binds one of these per
 * country (baking the `country` label) onto its Prometheus counter; `common`
 * sees only this narrow interface, so [[CountingHttpFetch]] carries no metrics
 * dependency and is trivially fakeable in tests.
 */
trait HttpOutcomeRecorder {
  def record(outcome: String): Unit
}

object HttpOutcomeRecorder {
  /** A no-op recorder — the default when nothing is wired (lone boots, tests that
   *  don't assert on outcomes), so [[CountingHttpFetch]] is safe to insert
   *  unconditionally. */
  val noop: HttpOutcomeRecorder = (_: String) => ()
}
