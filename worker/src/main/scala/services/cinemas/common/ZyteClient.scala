package services.cinemas.common

import play.api.Logging
import play.api.libs.json.{JsString, Json}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Wrapper around Zyte API's `/v1/extract` endpoint, used as a proxy backend
 * for cinema sites whose WAF blocks our datacenter IP — Zyte's residential
 * ASNs avoid those blocks. Two fetch shapes:
 *
 *   - [[get]] — a single extract call. For stateless pages that just need the
 *     residential egress to clear an IP block (biletyna's server-rendered
 *     venue page).
 *   - [[warm]] + [[fetchWithSession]] — the cookie-carryover pattern below, for
 *     upstreams with a session-cookie wall (Multikino's API). Split into two
 *     primitives so [[SharedZyteSession]] can warm ONCE and reuse the session
 *     across many fetches (every warm is a paid request).
 *
 * Cookie-carryover pattern:
 *
 *   1. [[warm]]: POST `cookieSourceUrl` (homepage) with a `session.id` UUID.
 *      Zyte fetches the homepage, parks the upstream's Set-Cookie in a
 *      server-side session keyed by that id.
 *   2. [[fetchWithSession]]: POST `targetUrl` with the same `session.id`. Zyte
 *      reuses the session — same egress IP, same cookie jar — and the
 *      upstream's session-cookie wall is already past.
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

  /** GET `targetUrl` via Zyte in a single extract call — no cookie warm-up.
   *  For stateless pages where the only thing standing between us and the
   *  HTML is a datacenter-IP block (biletyna's venue page). One credit.
   *  Throws on non-2xx upstream status or a missing body.
   */
  def get(targetUrl: String): String = get(targetUrl, Map.empty)

  /** [[get]] carrying request `headers` to the upstream (Zyte's
   *  `customHttpRequestHeaders`) — for an origin that authenticates in a header
   *  rather than a cookie (Odeon's ocapi `Authorization: Bearer`). */
  def get(targetUrl: String, headers: Map[String, String]): String =
    new String(getBytes(targetUrl, headers), StandardCharsets.UTF_8)

  /** [[get]]'s raw upstream bytes, undecoded — for a legacy single-byte page
   *  whose parser picks its own charset. */
  def getBytes(targetUrl: String, headers: Map[String, String] = Map.empty): Array[Byte] =
    bodyBytesOrThrow(post(targetUrl, sessionId = None, headers), targetUrl)

  /** Warm a Zyte session: fetch `cookieSourceUrl` (the homepage) under
   *  `sessionId` so Zyte parks the upstream's Set-Cookie in the server-side
   *  session keyed by that id. We don't read the body — only that the fetch
   *  succeeded. A non-2xx upstream here usually means the homepage itself is
   *  blocked (a strong signal the target's hostile), so throw and let the
   *  caller fall back to direct without burning a credit on a doomed API call.
   */
  def warm(cookieSourceUrl: String, sessionId: String): Unit = {
    val warmupStatus = extractStatus(post(cookieSourceUrl, Some(sessionId)))
    if (warmupStatus < 200 || warmupStatus >= 300)
      throw new RuntimeException(s"Zyte warm-up returned upstream status=$warmupStatus for $cookieSourceUrl")
  }

  /** GET `targetUrl` under an already-[[warm]]ed `sessionId` — a single extract
   *  call reusing the session's parked cookies + sticky egress. Returns the
   *  upstream body as UTF-8. Throws on a non-2xx upstream status or missing body
   *  (e.g. the session expired and the API answered 401), which the caller
   *  ([[SharedZyteSession]]) treats as "re-warm and retry once".
   */
  def fetchWithSession(targetUrl: String, sessionId: String): String =
    new String(fetchBytesWithSession(targetUrl, sessionId), StandardCharsets.UTF_8)

  /** [[fetchWithSession]]'s raw upstream bytes, undecoded. */
  def fetchBytesWithSession(targetUrl: String, sessionId: String): Array[Byte] =
    bodyBytesOrThrow(post(targetUrl, Some(sessionId)), targetUrl)

  /** Single POST to Zyte's /extract. Returns the raw JSON body or throws
   *  if Zyte itself failed (network error, 4xx/5xx from Zyte).
   */
  private def post(targetUrl: String, sessionId: Option[String], headers: Map[String, String] = Map.empty): String = {
    val body = requestBody(targetUrl, sessionId, headers)

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

  /** The Zyte `/extract` request body. The `session` field is included ONLY for
   *  the cookie-carryover path ([[ZyteClient.warm]] + [[ZyteClient.fetchWithSession]]),
   *  where it pins a sticky egress IP so the upstream's Set-Cookie survives
   *  across calls. The stateless [[ZyteClient.get]] path passes `None`: a one-shot
   *  session carries no cookies (nothing to gain), and Zyte's sticky-session IP
   *  is ban-prone on some hosts — bilety.ck105.koszalin.pl answers `520
   *  /download/website-ban` WITH a session but `200` (full programme) WITHOUT,
   *  so a stray session id was exactly what left Kino Kryterium a permanent white
   *  /uptime bar.
   *
   *  `headers` ride as `customHttpRequestHeaders`, and only when there are any. */
  def requestBody(targetUrl: String, sessionId: Option[String], headers: Map[String, String] = Map.empty): String = {
    val base        = Json.obj("url" -> targetUrl, "httpResponseBody" -> true)
    val withSession = sessionId.fold(base)(id => base + ("session" -> Json.obj("id" -> JsString(id))))
    val withHeaders =
      if (headers.isEmpty) withSession
      else withSession + ("customHttpRequestHeaders" ->
        Json.toJson(headers.toSeq.map { case (name, value) => Json.obj("name" -> name, "value" -> value) }))
    withHeaders.toString
  }

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
    extractBodyBytes(zyteJson).map(new String(_, StandardCharsets.UTF_8))

  /** The base64 `httpResponseBody` decoded to the upstream's exact bytes. */
  def extractBodyBytes(zyteJson: String): Option[Array[Byte]] =
    (Json.parse(zyteJson) \ "httpResponseBody").asOpt[String].map(Base64.getDecoder.decode)

  /** Decode the upstream body from one Zyte extract response, or throw with
   *  diagnostics: a non-2xx upstream status, or a response that carried no
   *  `httpResponseBody`. Shared by `get` and `fetchWithSession` so both fetch
   *  shapes agree on what counts as a usable result. */
  def bodyOrThrow(zyteJson: String, targetUrl: String): String =
    new String(bodyBytesOrThrow(zyteJson, targetUrl), StandardCharsets.UTF_8)

  /** [[bodyOrThrow]] without the UTF-8 decode — the upstream's exact bytes. */
  def bodyBytesOrThrow(zyteJson: String, targetUrl: String): Array[Byte] = {
    val status = extractStatus(zyteJson)
    if (status < 200 || status >= 300)
      throw new RuntimeException(s"Zyte API call returned upstream status=$status for $targetUrl")
    extractBodyBytes(zyteJson).getOrElse(
      throw new RuntimeException(s"Zyte response missing httpResponseBody for $targetUrl")
    )
  }

  /** Zyte authenticates with Basic auth where the API key is the username
   *  and the password is empty — see documents.zyte.com/zyte-api/usage.
   */
  def basicAuth(apiKey: String): String =
    "Basic " + Base64.getEncoder.encodeToString(s"$apiKey:".getBytes(StandardCharsets.UTF_8))
}
