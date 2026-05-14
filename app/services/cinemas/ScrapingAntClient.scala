package services.cinemas

import play.api.Logging

import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

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
class ScrapingAntClient(httpClient: HttpClient, key: String) {
  import ScrapingAntClient._

  /** GET `targetUrl` via ScrapingAnt, carrying cookies harvested from a first
   *  GET against `cookieSourceUrl`. Retries up to `maxAttempts` times while
   *  the proxy returns an unusable response. Throws after the last attempt.
   */
  def getWithCookies(
    targetUrl:       String,
    cookieSourceUrl: String,
    maxAttempts:     Int = DefaultMaxAttempts
  ): String =
    retryWhileUnusable(maxAttempts, "ScrapingAnt") {
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
      .uri(URI.create(s"$Endpoint?url=${urlEncode(targetUrl)}&proxy_country=pl&browser=false$extraParams"))
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
  private val Endpoint           = "https://api.scrapingant.com/v2/general"
  private val DefaultMaxAttempts = 3

  /** Captured response from one ScrapingAnt attempt. A response is usable when
   *  it's 200, non-empty, and JSON-shaped (the APIs we route through here only
   *  return JSON; HTML bodies are anti-bot interstitials).
   */
  case class FetchResult(status: Int, body: String) {
    def isUsable: Boolean =
      status == 200 && body.nonEmpty && body.dropWhile(_.isWhitespace).startsWith("{")
    def describe: String =
      s"status=$status, body=${body.length}B, head='${body.take(120).replace('\n', ' ')}'"
  }

  /** Invoke `attempt` up to `maxAttempts` times, returning the first usable
   *  response. Logs each unusable attempt and throws after the last one fails.
   *  Pure function of its arguments — exposed for tests.
   */
  def retryWhileUnusable(maxAttempts: Int, label: String)
                        (attempt: => FetchResult): FetchResult = {
    var last: Option[FetchResult] = None
    var i = 1
    while (i <= maxAttempts) {
      val r = attempt
      if (r.isUsable) return r
      last = Some(r)
      logger.warn(s"$label attempt $i/$maxAttempts unusable: ${r.describe}")
      i += 1
    }
    throw new RuntimeException(
      s"$label API response unusable after $maxAttempts attempts: " +
      last.map(_.describe).getOrElse("(no response)")
    )
  }
}
