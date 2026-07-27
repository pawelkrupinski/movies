package services.cinemas.common

import models.{Cinema, CinemaMovie}
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, ZoneId}

/**
 * Scraper for Webedia's Gatsby-hosted "box office" cinema platform — one
 * implementation serving TWO UK chains that run the identical backend on their
 * own hosts:
 *
 *   - Showcase Cinemas  `https://www.showcasecinemas.co.uk`  (16 venues)
 *   - Everyman          `https://www.everymancinema.com`     (50 venues)
 *
 * Verified 2026-07-27: both hosts answer unauthenticated, with no Cloudflare
 * challenge, and — because Gatsby derives the filename from the query text —
 * the SAME static-query hashes on both. One class parameterised by `baseUrl` is
 * therefore the whole story; there is no per-brand behaviour to model, which is
 * why this lives in `services.cinemas.common` rather than under `uk`.
 *
 * Two requests per venue per scrape:
 *
 *   1. `GET {base}/page-data/sq/d/3836549025.json`
 *      → `data.allMovie.nodes[]` — the chain-wide film catalogue keyed by the
 *        same numeric id the schedule uses. The ONLY source of titles: the
 *        schedule carries none. (The sibling `…/scheduledMovies` endpoint was
 *        investigated and rejected — it returns nothing but id ORDERINGS
 *        (`movieIds.titleAsc`) and a per-id day list, no metadata at all.)
 *   2. `GET {base}/api/gatsby-source-boxofficeapi/schedule?theaters=…&from=…&to=…`
 *      → `{theaterId: {schedule: {movieId: {date: [session, …]}}}}`, where
 *        `theaters` is a URL-encoded JSON object and each session carries
 *        `startsAt`, the dotted `tags[]`, `isExpired`, `screen.name` and the
 *        `data.ticketing[]` booking links.
 *
 * Parsing is [[GatsbyBoxOfficeParser]]'s; this class is only transport +
 * the horizon decision.
 */
class GatsbyBoxOfficeClient(
  http:      HttpFetch,
  baseUrl:   String,        // e.g. "https://www.showcasecinemas.co.uk"
  theaterId: String,        // e.g. "X06JR" — the platform's own venue id
  override val cinema: Cinema,
  // Every venue on both brands reports `Europe/London`; parameterised anyway
  // because the query needs it verbatim and the platform is multi-country.
  timeZone:  String         = GatsbyBoxOfficeClient.UkTimeZone,
  // The venue's public page path from the roster query
  // (`/theaters/x06jr-showcase-cinema-de-lux-bluewater`). Not derivable from
  // `theaterId` alone — the slug carries the venue name, and `/theaters/x06jr`
  // 404s — so the composition root supplies it or /uptime shows no source link.
  venuePath: Option[String] = None,
  today:     LocalDate      = LocalDate.now(ZoneId.of(GatsbyBoxOfficeClient.UkTimeZone))
) extends CinemaScraper {

  import GatsbyBoxOfficeClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  /** Both brands are national chains fed by one central box-office backend, so
   *  the Filmweb per-cinema fallback shouldn't shadow them (see
   *  `FallbackEligibility`). */
  override def chain: Boolean = true

  override def sourceUrl: Option[String] = venuePath.map(p => s"$baseUrl$p")

  /** ONE schedule request covers the entire horizon.
   *
   *  This is the endpoint's distinguishing feature and the reason this scraper
   *  is a plain `CinemaScraper` rather than a `ChunkedCinemaScraper` like the
   *  German and Flicks clients: `from`/`to` take an arbitrary range and the
   *  platform returns every populated day inside it in a single response, with
   *  no server-side cap (probed to a full year). Days with nothing on simply
   *  don't appear, so there is no per-day fan-out to plan, no index/nav fetch
   *  to discover which days exist, and no gap days wasted on empty requests —
   *  the two calls in `fetch()` are the venue's whole scrape.
   *
   *  The catalogue call is the same URL for every venue of a brand, so an HTTP
   *  cache in front collapses it across the chain's venues.
   */
  def fetch(): Seq[CinemaMovie] = {
    val catalogue = http.get(catalogueUrl(baseUrl))
    val schedule  = http.get(scheduleUrl(baseUrl, theaterId, timeZone, today, today.plusDays(MaxHorizonDays.toLong)))
    GatsbyBoxOfficeParser.parse(schedule, catalogue, theaterId, cinema, baseUrl)
  }
}

object GatsbyBoxOfficeClient {

  val ShowcaseBaseUrl = "https://www.showcasecinemas.co.uk"
  val EverymanBaseUrl = "https://www.everymancinema.com"

  val UkTimeZone = "Europe/London"

  /** How far ahead one `fetch()` asks for.
   *
   *  Matched to [[services.cinemas.uk.FlicksClient]]'s 210 rather than the
   *  German client's 34, because the constraint those two answer is different.
   *  There, the horizon is a per-day request budget — every extra day is
   *  another round-trip — so it is tuned down to the site's own booking window.
   *  Here the whole range is ONE request, and the marginal cost of a wider one
   *  is only the extra bytes: measured 2026-07-27 against Showcase's busiest
   *  venue (Bluewater), 90 days returned 1.03 MB and a FULL YEAR returned
   *  1.05 MB — ~2% more for 4× the window.
   *
   *  So the cap isn't protecting a budget, it is bounding the payload we parse
   *  and keeping the far tail out. Advance bookings do trickle to ~10 months
   *  out (a handful of dates reaching 2027-05-30), but those are single
   *  event-cinema dates that roll into any horizon long before they screen.
   *  210 days captures the real programme, agrees with the sibling UK chain, and
   *  keeps one brand-wide convention rather than a per-venue guess. */
  val MaxHorizonDays = 210

  /** The chain-wide film catalogue (Gatsby static query `allMovie`). The hash is
   *  Gatsby's digest of the query TEXT, so it is identical on both brands'
   *  hosts — confirmed live on each 2026-07-27 — and changes only if the site
   *  rewrites the query. */
  def catalogueUrl(baseUrl: String): String =
    s"$baseUrl/page-data/sq/d/$CatalogueQueryHash.json"

  private val CatalogueQueryHash = "3836549025"

  /** The venue's schedule over `[from, to)`. `theaters` is a JSON object passed
   *  as a query parameter, so it must be URL-encoded; the timestamps are the
   *  venue's own wall-clock and are sent unencoded, exactly as the site's own
   *  client does. */
  def scheduleUrl(baseUrl: String, theaterId: String, timeZone: String, from: LocalDate, to: LocalDate): String = {
    val theaters = URLEncoder.encode(s"""{"id":"$theaterId","timeZone":"$timeZone"}""", StandardCharsets.UTF_8)
    s"$baseUrl/api/gatsby-source-boxofficeapi/schedule?theaters=$theaters&from=${from}T00:00:00&to=${to}T00:00:00"
  }
}
