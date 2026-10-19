package services.cinemas.common

import play.api.Logging
import play.api.libs.json.Json
import tools.{EgressProviderException, HttpStatusException}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Wrapper around Zyte API's `/v1/extract` endpoint, used as a proxy backend
 * for a cinema site whose firewall blocks both our datacenter IP and the Decodo
 * proxy — Zyte's residential ASNs get through. One stateless extract call per
 * fetch ([[get]]).
 *
 * Mode is `httpResponseBody: true` (raw HTTP, no headless browser) — the
 * cheapest tier (~1 credit/request).
 *
 * Errors bubble as `RuntimeException`. Callers (`ZyteFetch` →
 * `FallbackHttpFetch`) catch and fall through to the next backend.
 */
class ZyteClient(httpClient: HttpClient, apiKey: settings.ZyteApiKey) extends Logging {
  import ZyteClient._

  /** GET `targetUrl` via Zyte in a single extract call. One credit.
   *  Throws on non-2xx upstream status or a missing body.
   */
  def get(targetUrl: String): String = get(targetUrl, Map.empty)

  /** [[get]] carrying request `headers` to the upstream (Zyte's
   *  `customHttpRequestHeaders`). */
  def get(targetUrl: String, headers: Map[String, String]): String =
    new String(getBytes(targetUrl, headers), StandardCharsets.UTF_8)

  /** [[get]]'s raw upstream bytes, undecoded — for a legacy single-byte page
   *  whose parser picks its own charset. */
  def getBytes(targetUrl: String, headers: Map[String, String] = Map.empty): Array[Byte] =
    bodyBytesOrThrow(post(targetUrl, headers), targetUrl)

  /** Single POST to Zyte's /extract. Returns the raw JSON body or throws
   *  if Zyte itself failed (network error, 4xx/5xx from Zyte).
   */
  private def post(targetUrl: String, headers: Map[String, String]): String = {
    val body = requestBody(targetUrl, headers)

    val request = HttpRequest.newBuilder()
      .uri(URI.create(Endpoint))
      .header("Authorization", basicAuth(apiKey))
      .header("Content-Type",  "application/json")
      .header("Accept",        "application/json")
      .timeout(RequestTimeout)
      .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
      .build()

    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    ZyteClient.apiBodyOrThrow(response.statusCode(), response.body(), targetUrl)
  }
}

object ZyteClient {
  private val Endpoint = "https://api.zyte.com/v1/extract"

  /** The longest one extract call may take. Zyte retries a hard upstream inside the
   *  call, so a slow answer is normal — but with no bound, an API that accepted the
   *  connection and never answered parked the calling scrape thread forever (the
   *  client sets only a connect timeout). */
  private val RequestTimeout = java.time.Duration.ofMinutes(3)

  /** The Zyte `/extract` request body. Never carries a `session`: Zyte's
   *  sticky-session IP is ban-prone on some hosts — bilety.ck105.koszalin.pl
   *  answers `520 /download/website-ban` WITH a session but `200` (full
   *  programme) WITHOUT, so a stray session id was exactly what left Kino
   *  Kryterium a permanent white /uptime bar.
   *
   *  `headers` ride as `customHttpRequestHeaders`, and only when there are any. */
  def requestBody(targetUrl: String, headers: Map[String, String] = Map.empty): String = {
    val base = Json.obj("url" -> targetUrl, "httpResponseBody" -> true)
    if (headers.isEmpty) base.toString
    else (base + ("customHttpRequestHeaders" ->
      Json.toJson(headers.toSeq.map { case (name, value) => Json.obj("name" -> name, "value" -> value) }))).toString
  }

  /** Zyte's own answer to an `/extract` POST: its JSON body on 200, or a throw
   *  naming Zyte's status (401 a bad key, 429 its throttle, 520 a ban). */
  def apiBodyOrThrow(zyteStatus: Int, zyteBody: String, targetUrl: String): String =
    if (zyteStatus == 200) zyteBody
    else throw new ZyteApiException(zyteStatus, s"Zyte http=$zyteStatus for $targetUrl: ${zyteBody.take(200)}")

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
   *  `httpResponseBody`. */
  def bodyOrThrow(zyteJson: String, targetUrl: String): String =
    new String(bodyBytesOrThrow(zyteJson, targetUrl), StandardCharsets.UTF_8)

  /** [[bodyOrThrow]] without the UTF-8 decode — the upstream's exact bytes. */
  def bodyBytesOrThrow(zyteJson: String, targetUrl: String): Array[Byte] = {
    val status = extractStatus(zyteJson)
    // The origin's own status, relayed: the origin's verdict, so an HttpStatusException.
    // No status at all (-1) is Zyte failing to say, which is Zyte's failure.
    if (status < 0)
      throw new ZyteApiException(status, s"Zyte API call returned upstream status=$status for $targetUrl")
    if (status < 200 || status >= 300)
      throw new ZyteOriginStatusException(status, targetUrl, s"Zyte API call returned upstream status=$status for $targetUrl")
    extractBodyBytes(zyteJson).getOrElse(
      throw new RuntimeException(s"Zyte response missing httpResponseBody for $targetUrl")
    )
  }

  /** Zyte authenticates with Basic auth where the API key is the username
   *  and the password is empty — see documents.zyte.com/zyte-api/usage.
   */
  def basicAuth(apiKey: settings.ZyteApiKey): String =
    "Basic " + Base64.getEncoder.encodeToString(s"${apiKey.value}:".getBytes(StandardCharsets.UTF_8))
}

/** The ORIGIN's non-2xx status, relayed by Zyte — the origin's verdict, so an
 *  [[HttpStatusException]] every status-aware caller may act on (a 404/410 is as
 *  durable through Zyte as fetched directly). The message is the one these
 *  failures always had: it is what /uptime shows and what people search for. */
class ZyteOriginStatusException(code: Int, targetUrl: String, message: String)
    extends HttpStatusException(code, "GET", targetUrl, None) {
  override def getMessage: String = message
}

/** Zyte ITSELF failing — its API answering non-200 (401 a bad key, 429 its
 *  throttle, 520 a ban), or naming no origin status. Zyte's status, never the
 *  origin's: see [[tools.EgressProviderException]]. */
class ZyteApiException(zyteStatus: Int, message: String) extends EgressProviderException(zyteStatus, message)
