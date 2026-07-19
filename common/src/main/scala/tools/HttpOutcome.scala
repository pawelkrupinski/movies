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
  val Http429         = "http_429"          // rate limited
  val Http4xx         = "http_4xx"          // other client errors (400/403/404/…)
  val Http5xx         = "http_5xx"          // server errors
  val Timeout         = "timeout"           // connect or read/request timeout
  val ConnectionError = "connection_error"  // refused / unknown host / reset / TLS
  val Other           = "other"             // anything unclassified (incl. 3xx that surfaced as an error)

  /** Every outcome, for seeding the metric at 0 so no Grafana gap opens before a
   *  category first fires. */
  val all: Seq[String] = Seq(Success, Http429, Http4xx, Http5xx, Timeout, ConnectionError, Other)

  /** Classify a thrown failure. Order matters: [[HttpStatusException]] carries a
   *  status; `HttpConnectTimeoutException` is a subtype of `HttpTimeoutException`
   *  (so the timeout case catches both); the connection-layer errors are all
   *  `IOException` subtypes, checked before the generic `IOException` fallback. */
  def classify(t: Throwable): String = t match {
    case e: HttpStatusException if e.code == 429               => Http429
    case e: HttpStatusException if e.code >= 400 && e.code < 500 => Http4xx
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
