package services.cinemas.uk

import play.api.libs.json.Json
import play.api.Logging
import services.cinemas.common.ZyteClient
import tools.{EgressProviderException, HttpOutcome, HttpOutcomeRecorder}

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
 * Lazy TTL cache: `token()`
 * serves the cached JWT until it is within `refreshMargin` of its own `exp`, then
 * re-harvests once under the lock. So one background scrape ~every 10h pays the
 * ~20s browser fetch; the rest read the cache. A harvest failure is not tried again for
 * `FailureCooldown`, serves the cached token while it is unexpired, and otherwise yields `None`, and
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
  now:           () => Long,
  refreshMargin: FiniteDuration = 2.hours
) extends Logging {
  import OdeonAuthHarvester._

  private val lock = new Object
  private var cached: Option[(String, Long)] = None // (token, exp epoch millis)
  // Until when a failed harvest is not tried again (epoch millis): every harvest is a paid Zyte call.
  private var failedUntil: Long = Long.MinValue

  /** The current Bearer, harvesting a fresh one when the cache is empty or within
   *  `refreshMargin` of expiry. A failed harvest is not retried for [[FailureCooldown]]; meanwhile a token
   *  still short of its `exp` is served, and without one the answer is `None` — the client treats that as a
   *  scrape failure and falls back to flicks. */
  def token(): Option[String] = lock.synchronized {
    val at    = now()
    val valid = cached.filter { case (_, exp) => at < exp }.map(_._1)
    cached match {
      case Some((t, exp)) if at < exp - refreshMargin.toMillis => Some(t)
      case _ if at < failedUntil                               => valid
      case _                                                   => harvest(at).orElse(valid)
    }
  }

  /** Drop the cached token so the next `token()` re-harvests — for a mid-life 401
   *  (token revoked before its `exp`). */
  def invalidate(): Unit = lock.synchronized { cached = None; failedUntil = Long.MinValue }

  private def harvest(at: Long): Option[String] = {
    val fresh = for {
      html  <- fetchPage()
      token <- extractToken(html)
      exp   <- jwtExpiryMillis(token)
    } yield (token, exp)
    fresh match {
      case Some((t, exp)) =>
        cached = Some((t, exp)); failedUntil = Long.MinValue
        logger.info(s"Odeon JWT harvested; it expires at ${java.time.Instant.ofEpochMilli(exp)}.")
        Some(t)
      case None =>
        failedUntil = at + FailureCooldown.toMillis
        logger.warn(s"Odeon JWT harvest failed; venues ride the flicks fallback (or the unexpired token) " +
          s"until the next try in ${FailureCooldown.toMinutes} min.")
        None
    }
  }
}

object OdeonAuthHarvester {
  /** How long a failed harvest waits before the next: each try is a paid Zyte browser fetch, and the failure on
   *  2026-11-04 was retried by every Odeon chunk that asked for a token — 131 tries in 16 minutes. */
  val FailureCooldown: FiniteDuration = 10.minutes

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
   *  plain UTF-8 string. `None` when no key is set or the call fails. Each call is a
   *  paid Zyte request, so its outcome goes to `meter` — the paid-egress counter. */
  def zyteFetchPage(apiKey: Option[settings.ZyteApiKey], pageUrl: String = OdeonPageUrl,
                    meter: HttpOutcomeRecorder = HttpOutcomeRecorder.noop): Option[String] =
    apiKey.flatMap { key =>
      meteredBrowserHtml(meter) {
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
        (response.statusCode(), response.body())
      }
    }

  /** Run one Zyte `browserHtml` `call` (status, body), record its outcome on
   *  `meter` — a non-200 classed by its status, a throw by its type — and answer
   *  the page, or `None`. Split out so the metering is testable offline. */
  def meteredBrowserHtml(meter: HttpOutcomeRecorder)(call: => (Int, String)): Option[String] =
    try {
      val (status, body) = call
      if (status == 200) {
        meter.record(HttpOutcome.Success)
        (Json.parse(body) \ "browserHtml").asOpt[String]
      } else {
        meter.record(HttpOutcome.classify(new EgressProviderException(status, s"Zyte http=$status for $ZyteEndpoint")))
        None
      }
    } catch {
      case NonFatal(e) =>
        meter.record(HttpOutcome.classify(e))
        None
    }

  private val ZyteEndpoint = "https://api.zyte.com/v1/extract"
}
