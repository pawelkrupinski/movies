package tools

import java.net.http.{HttpConnectTimeoutException, HttpResponse, HttpTimeoutException}
import java.nio.charset.StandardCharsets
import java.time.Duration

import scala.concurrent.duration.FiniteDuration

import play.api.Logger

/** What a [[RealHttpFetch]] exchange's reply or failure becomes -- a decoded body, or a typed status failure -- and how
 *  each is logged, on `logger` (the fetch's own). `requestTimeoutFor` / `connectTimeoutFor` are the fetch's budgets,
 *  named in a timeout's line. */
final class HttpReplies(logger: Logger, requestTimeoutFor: String => Duration, connectTimeoutFor: String => Duration) {

  // ── Logging hooks ─────────────────────────────────────────────────────────
  //
  // Every HTTP client in the app (TMDB, Filmweb, MC, RT, IMDb, every cinema
  // scraper) routes through RealHttpFetch, so through these replies: failure visibility added here
  // surfaces in every caller at once. Two failure shapes worth seeing:
  //
  //   - **Network / timeout** — the service didn't respond at all (DNS,
  //     connection refused, socket timeout, request timeout). WARN —
  //     this is what masks the symptom users hit (e.g. Kino Muza's burst
  //     limiter stalling our detail fetches: no logs, just silently empty
  //     synopses).
  //   - **5xx response** — the service returned an error status. Also WARN.
  //     Both are DEBUG under the retry layer ([[OutcomeLoggedAbove]]), which
  //     warns once for the whole call if it still fails, naming its attempts.
  //   - **403 / 429 response** — WARN. A soft-block / rate-limit (a Cloudflare
  //     or datacenter-IP block) is a real outage the enrichment sources hit; at
  //     DEBUG it was INVISIBLE at the prod INFO level, hiding e.g. a Filmweb
  //     block for days. Also classified as a failure by MonitoringHttpFetch, so
  //     it now surfaces on /uptime instead of reading green.
  //   - **other 4xx (esp. 404)** — DEBUG only. Several callers (MetacriticClient's
  //     slug probe, RT's URL probe) deliberately use 404 as the "this
  //     slug doesn't exist" signal; WARN-ing every probe miss would flood
  //     the logs with hundreds of expected 404s per refresh tick.

  /** `send`, logging a transport failure before it propagates. The response is
   *  returned unchecked — the caller runs [[body]]. */
  def sentLogged(method: String, url: String, send: => HttpResponse[Array[Byte]]): HttpResponse[Array[Byte]] =
    AnyThrowable.rethrownAfter(send)(failed(method, url, _))

  /** A 2xx reply's body, decoded; any other status thrown as an [[HttpStatusException]], logged as its kind is. */
  def body(method: String, url: String, response: HttpResponse[Array[Byte]]): String = {
    val code = response.statusCode()
    if (code >= 200 && code < 300) decodeBody(response.body())
    else {
      // Logged REDACTED: TMDB/OMDb authenticate in the query string, so the bare
      // url would write their key into the 14-day persistent log (see RedactedUrl).
      val safeUrl = RedactedUrl(url)
      // Typed so a decorator (ThrottledHttpFetch) can react to 429 + Retry-After.
      // Message shape is unchanged, so MonitoringHttpFetch's 5xx classifier and
      // any message-matching caller keep working.
      val failure = new HttpStatusException(code, method, url, retryAfterOf(response))
      if (code >= 500)                     warnUnlessLoggedAbove(failure, s"HTTP $code from $method $safeUrl (server-side)")
      else if (code == 403 || code == 429) logger.warn(s"HTTP $code from $method $safeUrl (blocked/throttled)")
      else                                 logger.debug(s"HTTP $code from $method $safeUrl")
      throw failure
    }
  }

  /** The server's `Retry-After` hint, if any (delta-seconds, or an HTTP-date read against its `Date`). */
  private def retryAfterOf(response: HttpResponse[Array[Byte]]): Option[FiniteDuration] = {
    def header(name: String) = Option(response.headers().firstValue(name).orElse(null))
    HttpStatusException.parseRetryAfter(header("Retry-After"), header("Date"))
  }

  /** Decode the raw response bytes to a UTF-8 string. `Gunzip.decode`
   *  loops while the bytes still start with the gzip magic prefix
   *  `1F 8B` — TMDB (or its CDN) ships some responses as
   *  `gzip(gzip(json))` under a single `Content-Encoding: gzip` header,
   *  so one round of decompression isn't enough. Non-gzip and
   *  single-gzip bodies are handled by the same call. */
  private def decodeBody(bytes: Array[Byte]): String =
    new String(Gunzip.decode(bytes), StandardCharsets.UTF_8)

  /** WARN for a transport failure. A read its caller abandoned ([[CallerCancelled]]) is not one: the caller logs why
   *  it cut the call (AdaptiveTimeoutScraper's "scrape cut" line), and a WARN here blamed the host for it. */
  def failed(method: String, url: String, exception: Throwable): Unit =
    if (CallerCancelled(exception)) logger.debug(s"HTTP $method ${RedactedUrl(url)} cancelled by its caller")
    else warnUnlessLoggedAbove(exception, failureLine(method, url, exception))

  /** WARN, unless a layer above logs this failure as its call's outcome ([[OutcomeLoggedAbove]]: the retry, which
   *  asks a recoverable failure again and warns once if the call still fails) -- then DEBUG, one attempt's detail. */
  private def warnUnlessLoggedAbove(failure: Throwable, line: => String): Unit =
    if (OutcomeLoggedAbove() && RetryingHttpFetch.recoverable(failure)) logger.debug(line) else logger.warn(line)

  /** The line a transport failure is logged as. A timeout names the budget that ran out: a connect timeout used to
   *  read "timed out after 30s" -- the REQUEST budget -- while the connect budget (5 s by default, bounding TCP and the
   *  TLS handshake together) was what expired, which sent a loopback flake hunting for a 30 s connect that never
   *  happened (IssuerCertificateFetchingSpec, run 4360). */
  def failureLine(method: String, url: String, exception: Throwable): String = {
    val safeUrl = RedactedUrl(url)
    exception match {
      case _: HttpConnectTimeoutException =>
        s"HTTP $method $safeUrl: connect (TCP + TLS handshake) timed out after ${shown(connectTimeoutFor(url))}"
      case _: HttpTimeoutException =>
        s"HTTP $method $safeUrl timed out after ${shown(requestTimeoutFor(url))} (service not responding)"
      case _ =>
        s"HTTP $method $safeUrl failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}"
    }
  }

  private def shown(budget: Duration): String =
    if (budget.toMillis % 1000 == 0) s"${budget.toSeconds}s" else s"${budget.toMillis}ms"

  /** A reply's raw wire bytes (gunzipped, NOT charset-decoded), or its status thrown as for [[body]] -- unlogged. */
  def bytes(url: String, response: HttpResponse[Array[Byte]]): Array[Byte] = {
    val code = response.statusCode()
    if (code >= 200 && code < 300) Gunzip.decode(response.body())
    else throw new HttpStatusException(code, "GET", url, retryAfterOf(response))
  }
}
