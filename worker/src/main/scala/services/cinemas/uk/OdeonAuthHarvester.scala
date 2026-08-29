package services.cinemas.uk

import play.api.libs.json.Json
import play.api.Logging
import services.cinemas.common.ZyteClient

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Duration
import java.util.Base64
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.matching.Regex

/**
 * Supplies [[OdeonClient]] with a short-lived Vista Bearer JWT.
 *
 * Odeon's www site is Cloudflare-gated but embeds the token in its page HTML as
 * `window.initialData.api.authToken`; the Vista `ocapi` backend it authorises
 * (`vwc.odeon.co.uk`) needs no browser — only the bearer (it is Cloudflare-gated on
 * egress-IP reputation, which the residential proxy clears; see [[OdeonClient]]).
 * So we harvest the token by loading ONE Odeon page through Zyte's `browserHtml`
 * mode (the only Zyte mode that clears the challenge and runs the page's JS), read
 * the JWT, and hand it to the client — the client then pulls all showtimes over
 * plain HTTP. The token is global to the
 * estate (one page covers all 102 venues) and lasts ~12h.
 *
 * Lazy TTL cache, mirroring [[services.cinemas.common.SharedZyteSession]]: `token()`
 * serves the cached JWT until it is within `refreshMargin` of its own `exp`, then
 * re-harvests once under the lock. So one background scrape ~every 10h pays the
 * ~20s browser fetch; the rest read the cache. A harvest failure yields `None`, and
 * `OdeonClient` then throws → the venue rides its flicks fallback rather than
 * showing a false "nothing on". `fetchPage` is injected so the token logic is
 * tested without live Zyte (the fake just returns a captured page).
 *
 * Cheap by construction: ~2 browser fetches/day/machine — single-digit dollars/mo —
 * versus fetching the ocapi DATA through a browser (~150k calls/mo), which is
 * pointless since ocapi needs only the Bearer, not a browser.
 */
class OdeonAuthHarvester(
  fetchPage:     () => Option[String],
  now:           () => Long = () => System.currentTimeMillis(),
  refreshMargin: FiniteDuration = 2.hours
) extends Logging {
  import OdeonAuthHarvester._

  private val lock = new Object
  private var cached: Option[(String, Long)] = None // (token, exp epoch millis)

  /** The current Bearer, harvesting a fresh one when the cache is empty or within
   *  `refreshMargin` of expiry. `None` when a harvest fails — the client treats
   *  that as a scrape failure and falls back to flicks. */
  def token(): Option[String] = lock.synchronized {
    cached match {
      case Some((t, exp)) if now() < exp - refreshMargin.toMillis => Some(t)
      case _                                                      => harvest()
    }
  }

  /** Drop the cached token so the next `token()` re-harvests — for a mid-life 401
   *  (token revoked before its `exp`). */
  def invalidate(): Unit = lock.synchronized { cached = None }

  private def harvest(): Option[String] = {
    val fresh = for {
      html  <- fetchPage()
      token <- extractToken(html)
      exp   <- jwtExpiryMillis(token)
    } yield (token, exp)
    fresh match {
      case Some((t, exp)) => cached = Some((t, exp)); Some(t)
      case None           => logger.warn("Odeon JWT harvest failed; venues will ride the flicks fallback."); None
    }
  }
}

object OdeonAuthHarvester {
  /** Any Odeon page carries the estate-wide token; the cinema index is small. */
  val OdeonPageUrl = "https://www.odeon.co.uk/cinemas/"

  private val TokenRegex: Regex = """"authToken"\s*:\s*"([^"]+)"""".r

  /** Pull `window.initialData.api.authToken` out of the rendered page HTML. */
  def extractToken(html: String): Option[String] =
    TokenRegex.findFirstMatchIn(html).map(_.group(1)).filter(_.nonEmpty)

  /** The JWT's `exp` claim (seconds) → epoch millis. Base64URL-decodes the payload
   *  segment and reads `exp`; no signature check (we only need the lifetime). */
  def jwtExpiryMillis(jwt: String): Option[Long] =
    jwt.split('.') match {
      case Array(_, payload, _*) =>
        try {
          val json = Json.parse(new String(Base64.getUrlDecoder.decode(payload), UTF_8))
          (json \ "exp").asOpt[Long].map(_ * 1000L)
        } catch { case NonFatal(_) => None }
      case _ => None
    }

  /** Production `fetchPage`: one Zyte `browserHtml` POST. Unlike `httpResponseBody`
   *  (which [[ZyteClient]] uses and base64-encodes), `browserHtml` comes back as a
   *  plain UTF-8 string. `None` when no key is set or the call fails. */
  def zyteFetchPage(apiKey: Option[String], pageUrl: String = OdeonPageUrl): Option[String] =
    apiKey.filter(_.nonEmpty).flatMap { key =>
      try {
        val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(30)).build()
        val body   = Json.obj("url" -> pageUrl, "browserHtml" -> true).toString
        val request = HttpRequest.newBuilder()
          .uri(URI.create(ZyteEndpoint))
          .header("Authorization", ZyteClient.basicAuth(key))
          .header("Content-Type", "application/json")
          .timeout(Duration.ofSeconds(120))
          .POST(HttpRequest.BodyPublishers.ofString(body, UTF_8))
          .build()
        val response = client.send(request, HttpResponse.BodyHandlers.ofString(UTF_8))
        if (response.statusCode() == 200) (Json.parse(response.body()) \ "browserHtml").asOpt[String]
        else None
      } catch { case NonFatal(_) => None }
    }

  private val ZyteEndpoint = "https://api.zyte.com/v1/extract"
}
