package services.cinemas

import play.api.Logging

import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Random

/**
 * Thin wrapper around ScrapingAnt's general-purpose proxy API. Encapsulates
 * the two patterns we lean on repeatedly:
 *
 *   1. **Cookie carryover** — hit a `cookieSourceUrl` (usually the cinema's
 *      homepage) to capture a set of session cookies, then GET the real
 *      `targetUrl` with `cookies=` carried over. The cinema API otherwise
 *      returns 401 without the JWT session cookie that the homepage hands out.
 *   2. **Retry on unusable proxy response** — ScrapingAnt occasionally returns
 *      200 with an empty body or an HTML anti-bot interstitial when the proxy
 *      upstream times out. Retrying with a fresh proxy session almost always
 *      succeeds (the failure is proxy-side, not target-side).
 *
 * `browser=false` keeps the response raw — the headless-browser mode wraps
 * JSON in `<pre>`, costs more credits, and gets caught by anti-bot detection.
 */
class ScrapingAntClient(httpClient: HttpClient, key: String, proxyCountry: () => String = () => "pl") {
  import ScrapingAntClient._

  /** GET `targetUrl` via ScrapingAnt, carrying cookies harvested from a first
   *  GET against `cookieSourceUrl`. Retries up to `maxAttempts` times while
   *  the proxy returns an unusable response, with exponential backoff between
   *  attempts. Throws after the last attempt.
   */
  def getWithCookies(
    targetUrl:       String,
    cookieSourceUrl: String,
    maxAttempts:     Int            = DefaultMaxAttempts,
    initialBackoff:  FiniteDuration = DefaultInitialBackoff
  ): String =
    retryWhileUnusable(maxAttempts, "ScrapingAnt", initialBackoff) {
      val cookies = readSetCookieHeaders(httpClient.send(
        request(cookieSourceUrl, ""),
        HttpResponse.BodyHandlers.discarding()
      ))
      val response = httpClient.send(
        request(targetUrl, s"&cookies=${urlEncode(cookies)}"),
        HttpResponse.BodyHandlers.ofString()
      )
      FetchResult(response.statusCode(), response.body())
    }.body

  private def request(targetUrl: String, extraParams: String) =
    HttpRequest.newBuilder()
      .uri(URI.create(s"$Endpoint?url=${urlEncode(targetUrl)}&proxy_country=${proxyCountry()}&browser=false$extraParams"))
      .header("x-api-key", key)
      .header("Accept", "application/json, text/plain, */*")
      .GET()
      .build()

  private def readSetCookieHeaders(response: HttpResponse[_]): String =
    response.headers().allValues("set-cookie").asScala
      .map(_.split(";", 2).head)
      .filter(_.nonEmpty)
      .mkString(";")

  private def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}

object ScrapingAntClient extends Logging {
  private val Endpoint              = "https://api.scrapingant.com/v2/general"

  /** ScrapingAnt-supported European proxy pools we cycle through to evade
   *  per-pool anti-bot blocks. When one country's datacenter range gets
   *  scored as bot-suspicious by a cinema's WAF, picking a different
   *  country gives us a fresh IP pool that hasn't been flagged yet.
   *
   *  The list is exactly the European entries from ScrapingAnt's supported
   *  country enum (see docs.scrapingant.com/proxy-settings). Anything
   *  outside that set returns 422 ("proxy_country is not a valid
   *  enumeration member") — so the previous, larger list (at/be/dk/fi/…)
   *  silently burned half the retry budget on 422s. Stick to the enum.
   *
   *  `pl` is deliberately omitted: it was the first pool that started
   *  returning 423 from multikino.pl, so until it cools off we don't
   *  spend retries hitting a known-bad pool. Easy to re-add later.
   */
  val EuropeanCountries: Vector[String] =
    Vector("cz", "de", "es", "fr", "gb", "it", "nl")

  /** Random pick from `EuropeanCountries`. Used as the `proxyCountry`
   *  thunk so each ScrapingAnt request rolls a fresh country and the
   *  in-client retry loop naturally tries different pools on failure.
   */
  def randomEuropean(): String =
    EuropeanCountries(Random.nextInt(EuropeanCountries.length))

  // Total worst-case wait with 5 attempts × 2s exponential backoff:
  //   2s + 4s + 8s + 16s = 30s of sleeping across 5 calls.
  // Long enough to let the free-tier concurrency counter (HTTP 409) flush
  // between attempts, short enough that an integration test doesn't time out.
  private val DefaultMaxAttempts    = 5
  private val DefaultInitialBackoff = 2.seconds

  /** Captured response from one ScrapingAnt attempt. A response is usable when
   *  it's 200, non-empty, and JSON-shaped (the APIs we route through here only
   *  return JSON; HTML bodies are anti-bot interstitials, 409/429/503 are
   *  proxy-side rate limits).
   */
  case class FetchResult(status: Int, body: String) {
    def isUsable: Boolean =
      status == 200 && body.nonEmpty && body.dropWhile(_.isWhitespace).startsWith("{")
    def describe: String =
      s"status=$status, body=${body.length}B, head='${body.take(120).replace('\n', ' ')}'"
  }

  /** Invoke `attempt` up to `maxAttempts` times, returning the first usable
   *  response. Logs each unusable attempt and exponentially backs off between
   *  retries (doubling) — important for free-tier concurrency limits (HTTP 409
   *  "Free user concurrency limit reached") where immediate retries land in
   *  the same metering window. Throws after the last attempt.
   *
   *  Tests pass `initialBackoff = Duration.Zero` to skip sleeping.
   */
  def retryWhileUnusable(
    maxAttempts:    Int,
    label:          String,
    initialBackoff: FiniteDuration = DefaultInitialBackoff
  )(attempt: => FetchResult): FetchResult = {
    var last: Option[FetchResult] = None
    var i = 1
    while (i <= maxAttempts) {
      val r = attempt
      if (r.isUsable) return r
      last = Some(r)
      logger.warn(s"$label attempt $i/$maxAttempts unusable: ${r.describe}")
      if (i < maxAttempts && initialBackoff > Duration.Zero) {
        val wait = initialBackoff * (1L << (i - 1))   // 1×, 2×, 4×, 8×, …
        logger.info(s"$label backing off ${wait.toMillis}ms before attempt ${i + 1}")
        Thread.sleep(wait.toMillis)
      }
      i += 1
    }
    throw new RuntimeException(
      s"$label API response unusable after $maxAttempts attempts: " +
      last.map(_.describe).getOrElse("(no response)")
    )
  }
}
