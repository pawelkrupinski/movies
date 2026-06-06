package services.cinemas

import play.api.Logging
import play.api.libs.json.{JsString, Json}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}

/**
 * Wrapper around Zyte API's `/v1/extract` endpoint, used as the primary
 * Multikino proxy backend. Zyte's residential ASNs avoid the WAF blocks
 * that datacenter IPs hit.
 *
 * Cookie-carryover pattern:
 *
 *   1. POST `cookieSourceUrl` (homepage) with a fresh `session.id` UUID.
 *      Zyte fetches the homepage, parks the upstream's Set-Cookie in a
 *      server-side session keyed by that id.
 *   2. POST `targetUrl` with the same `session.id`. Zyte reuses the
 *      session — same egress IP, same cookie jar — and the upstream's
 *      session-cookie wall is already past.
 *
 * Default mode is `httpResponseBody: true` (raw HTTP, no headless
 * browser). It's the cheapest tier (~1 credit/request) and it's what
 * Multikino needs — the API returns JSON, no JS rendering is required.
 *
 * Errors bubble as `RuntimeException`. Callers (`ZyteFetch` →
 * `FallbackHttpFetch`) catch and fall through to the next backend.
 */
class ZyteClient(httpClient: HttpClient, apiKey: String) extends Logging {
  import ZyteClient._

  /** GET `targetUrl` via Zyte, harvesting a session from
   *  `cookieSourceUrl` first. Returns the upstream response body as
   *  UTF-8. Throws on any non-2xx upstream status or non-2xx Zyte
   *  status, with diagnostics in the exception message.
   */
  def getWithCookies(targetUrl: String, cookieSourceUrl: String): String = {
    val sessionId = UUID.randomUUID().toString

    // Warm-up: fetch the cookie source. We don't read its body — only
    // care that Zyte successfully fetched it and parked cookies in the
    // session. A non-2xx upstream here usually means the homepage itself
    // is blocked, which is a strong signal the target's hostile; bail
    // early so the caller falls back to direct without burning a
    // second credit on a doomed API call.
    val warmupStatus = extractStatus(post(cookieSourceUrl, sessionId))
    if (warmupStatus < 200 || warmupStatus >= 300)
      throw new RuntimeException(s"Zyte warm-up returned upstream status=$warmupStatus for $cookieSourceUrl")

    val apiPayload = post(targetUrl, sessionId)
    val apiStatus  = extractStatus(apiPayload)
    if (apiStatus < 200 || apiStatus >= 300)
      throw new RuntimeException(s"Zyte API call returned upstream status=$apiStatus for $targetUrl")

    extractBody(apiPayload).getOrElse(
      throw new RuntimeException(s"Zyte response missing httpResponseBody for $targetUrl")
    )
  }

  /** Single POST to Zyte's /extract. Returns the raw JSON body or throws
   *  if Zyte itself failed (network error, 4xx/5xx from Zyte).
   */
  private def post(targetUrl: String, sessionId: String): String = {
    val body = Json.obj(
      "url"              -> targetUrl,
      "httpResponseBody" -> true,
      "session"          -> Json.obj("id" -> JsString(sessionId))
    ).toString

    val request = HttpRequest.newBuilder()
      .uri(URI.create(Endpoint))
      .header("Authorization", basicAuth(apiKey))
      .header("Content-Type",  "application/json")
      .header("Accept",        "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
      .build()

    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(
        s"Zyte http=${response.statusCode()} for $targetUrl: ${response.body().take(200)}"
      )
    response.body()
  }
}

object ZyteClient {
  private val Endpoint = "https://api.zyte.com/v1/extract"

  /** Pull the upstream HTTP status code from a Zyte extract response.
   *  Defaults to -1 when absent (treated by callers as an error).
   */
  def extractStatus(zyteJson: String): Int =
    (Json.parse(zyteJson) \ "statusCode").asOpt[Int].getOrElse(-1)

  /** Decode the base64 `httpResponseBody` from a Zyte extract response,
   *  returning the upstream payload as UTF-8. `None` when the field is
   *  absent (e.g. Zyte returned only metadata, or the call asked for
   *  `browserHtml` instead).
   */
  def extractBody(zyteJson: String): Option[String] =
    (Json.parse(zyteJson) \ "httpResponseBody")
      .asOpt[String]
      .map(b64 => new String(Base64.getDecoder.decode(b64), StandardCharsets.UTF_8))

  /** Zyte authenticates with Basic auth where the API key is the username
   *  and the password is empty — see docs.zyte.com/zyte-api/usage.
   */
  def basicAuth(apiKey: String): String =
    "Basic " + Base64.getEncoder.encodeToString(s"$apiKey:".getBytes(StandardCharsets.UTF_8))
}
