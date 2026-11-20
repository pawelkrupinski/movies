package services.cinemas.uk

import tools.{HttpFetch, HttpRead, HttpStatusException}
import models._
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ScrapeHorizon, ListingFeed}

import java.time.LocalDate

/**
 * ODEON (odeon.co.uk) — the UK's largest chain (100+ venues), scraped through
 * its Vista booking backend `vwc.odeon.co.uk/WSVistaWebClient/ocapi/v1`.
 *
 * HYBRID auth by design. Odeon's www site sits behind Cloudflare (a plain fetch
 * 403s) and needs a real browser; the ocapi DATA backend needs only a bearer JWT.
 * That JWT is global (one token covers every venue), lives in the Cloudflare-served
 * page HTML as `window.initialData.api.authToken`, and expires in ~12h. So the
 * token is harvested OUT OF BAND (production: a Zyte `browserHtml` fetch of a
 * www page, ~2×/day) and injected here as its `bearer` — this client never
 * touches the www page or Zyte, keeping the harvest seam wired separately
 * (see the composition root). Every ocapi call is plain HTTP over the injected
 * `http`, carrying `Authorization: Bearer <token>`. A 401 says the token was revoked
 * before its expiry: the call tells the bearer ([[OdeonBearer.refused]]) and asks ONCE
 * more with the fresh token it then hands out, if it hands out another.
 *
 * The bearer is not sufficient on its own: ocapi is ALSO behind Cloudflare, which
 * gates on egress-IP reputation. It answered our Fly datacenter IP, but 403s the
 * Hetzner one the worker moved to on 2026-08-29, so production injects the
 * residential-proxy `odeonFetch` here rather than the shared direct `http`.
 *
 * chain=true: Odeon is a large, reliably-fed network whose own API is the
 * source of truth, so it's excluded from the Filmweb-style fallback.
 *
 * One instance serves one venue (its ocapi `siteId` + the [[Cinema]] it feeds).
 * Per-day chunked, mirroring the Vista/quickbook shape: the horizon comes from
 * `film-screening-dates` (the exact business dates the venue has on), and each
 * date is one `showtimes/by-business-date/<date>` call — one chunk, one queued
 * `ScrapeChunk` task, so the days spread across the task queue instead of
 * bursting from a single task.
 */
class OdeonClient(
  http:                HttpFetch,
  // Exposed so `OdeonVenueMapSpec` can hold the wired ids against the checked-in
  // roster in `docs/venue-maps/ODEON-VENUE-MAP.tsv` — a site Odeon retires from
  // its estate then fails the build rather than 400ing in production unnoticed.
  val siteId:          String,
  override val cinema: Cinema,
  bearer:              OdeonBearer,
  today:               => LocalDate
) extends ChunkedCinemaScraper {

  import OdeonClient._

  override def listingFeed: ListingFeed = ListingFeed.Chain

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiBase)

  // The ocapi `sites` roster carries no public slug, and the www venue page is
  // Cloudflare-protected anyway, so there's no stable public URL to derive here.
  override def sourceUrl: Option[String] = None
  override def sourceKey: Option[String] = Some(s"${CinemaScraper.urlKey(ApiBase)}/sites/$siteId")

  /** The venue's business dates, bounded to `[today, today + ScrapeHorizon.MaxDays]`. One
   *  `film-screening-dates` call names exactly the days the venue has a
   *  programme on (Odeon advertises event/advance dates months out), so we fetch
   *  only those — no request per empty day. A fetch failure (or a missing token)
   *  THROWS, failing the whole scrape as a normal recorded outcome, which keeps
   *  the venue's last-known listing rather than narrowing it to a guess. */
  def planChunks(): Seq[String] =
    OdeonParser.parseScreeningDates(authorisedPage(datesUrl))
      .filter(ScrapeHorizon.includes(today))
      .map(_.toString)

  /** Fetch + parse ONE business date's showtimes into that day's films. Throws
   *  on failure so only this date's chunk task reschedules (the per-day retry). */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] =
    OdeonParser.parseShowtimes(authorisedPage(showtimesUrl(dateKey)), cinema)

  private def datesUrl: String =
    s"$ApiBase/ocapi/v1/film-screening-dates?siteIds=$siteId"

  private def showtimesUrl(dateKey: String): String =
    s"$ApiBase/ocapi/v1/showtimes/by-business-date/$dateKey?siteIds=$siteId"

  /** The bearer the ocapi calls carry. When no token has been harvested yet,
   *  THROW rather than fetch — an un-authenticated ocapi call would 401, and
   *  swallowing that to return empty would look like "this venue has nothing on"
   *  (silently dropping the whole listing). Throwing makes it a normal scrape
   *  FAILURE instead, so `MovieCache.recordCinemaScrape` keeps the last-known
   *  listing and /uptime shows a red row (a real problem) rather than a
   *  white "empty" one (a lie). */
  private def currentToken(): String =
    bearer.token().filter(_.nonEmpty).getOrElse(throw new IllegalStateException(
      s"Odeon bearer token not available for siteId=$siteId (harvest not yet run)"))

  private def page(url: String, token: String): String =
    HttpRead.page(http, url, Map("Authorization" -> s"Bearer $token", "Accept" -> "application/json"))

  /** The page under the current bearer; on a 401, once more under the fresh one the refusal makes the bearer hand
   *  out — never more, and not at all when it hands out the refused token again (the 401 then stands). */
  private def authorisedPage(url: String): String = {
    val token = currentToken()
    try page(url, token)
    catch {
      case refusal: HttpStatusException if refusal.code == Unauthorised =>
        bearer.refused(token)
        val fresh = currentToken()
        if (fresh == token) throw refusal
        page(url, fresh)
    }
  }
}

object OdeonClient {

  /** The Vista WebClient base the ocapi endpoints hang off — no browser challenge,
   *  but Cloudflare-gated on egress IP, so production reaches it over `odeonFetch`. */
  val ApiBase = "https://vwc.odeon.co.uk/WSVistaWebClient"

  /** ocapi's answer to a bearer it does not accept. Only this status, not a 403: through the proxy a 403 is
   *  Cloudflare refusing the egress IP (see `odeonFetch`), which a fresh token cannot cure but a paid harvest
   *  would be spent on. */
  private val Unauthorised = 401

  /** Vista's media CDN on the same host — the films' posters ([[OdeonParser.posterUrl]]). */
  val MediaCdn = "https://vwc.odeon.co.uk/CDN"
}
