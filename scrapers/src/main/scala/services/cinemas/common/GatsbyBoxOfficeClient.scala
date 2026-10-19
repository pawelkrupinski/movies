package services.cinemas.common

import models.{Cinema, CinemaMovie, TimeZones}
import tools.{HttpFetch, HttpRead}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import scala.util.Try
import scala.util.control.NonFatal

/**
 * Scraper for Webedia's Gatsby-hosted "box office" cinema platform — one
 * implementation serving FIVE chains, on two continents, that run the identical
 * backend on their own hosts:
 *
 *   - Cineworld            `https://www.cineworld.co.uk`        (87 venues, via
 *                          [[services.cinemas.uk.CineworldClient]], since its
 *                          2026-09-17 relaunch)
 *   - Showcase Cinemas UK  `https://www.showcasecinemas.co.uk`  (16 venues)
 *   - Everyman             `https://www.everymancinema.com`     (50 venues)
 *   - Showcase Cinemas US  `https://www.showcasecinemas.com`    (13 venues)
 *   - Landmark Theatres    `https://www.landmarktheatres.com`   (26 venues)
 *
 * Verified 2026-07-27 (UK) and 2026-08-30 (US): every host answers
 * unauthenticated, with no Cloudflare challenge, and — because Gatsby derives
 * the filename from the query text — the SAME static-query hashes on all of them.
 * One class parameterised by `baseUrl` is therefore the whole story; there is no
 * per-brand behaviour to model, which is why this lives in
 * `services.cinemas.common` rather than under `uk`.
 *
 * The US brands cost NOTHING but their base URL and their venue ids: looking for
 * a shared platform before writing a client turned two of the seven US mid-tier
 * chains into a wiring change. `timeZone` was already a parameter (the platform
 * is multi-country and the query needs it verbatim), which is what let Landmark
 * — five zones, `America/Phoenix` and `America/Indiana/Indianapolis` among them —
 * arrive without touching this class at all.
 *
 * Three requests per venue per scrape (the third per 50 films):
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
 *   3. `GET {base}/api/gatsby-source-boxofficeapi/movies?ids=…&ids=…`
 *      → the scheduled films' credits and running times, which the catalogue's
 *        static query leaves null and the film page loads client-side.
 *
 * Parsing is [[GatsbyBoxOfficeParser]]'s; this class is only transport +
 * the horizon decision.
 */
class GatsbyBoxOfficeClient(
  http:      HttpFetch,
  baseUrl:   String,        // e.g. "https://www.showcasecinemas.co.uk"
  theaterId: String,        // e.g. "X06JR" — the platform's own venue id
  override val cinema: Cinema,
  // Every UK venue reports `Europe/London`; parameterised anyway
  // because the query needs it verbatim and the platform is multi-country.
  timeZone:  String         = GatsbyBoxOfficeClient.UkTimeZone,
  // The venue's public page path from the roster query
  // (`/theaters/x06jr-showcase-cinema-de-lux-bluewater`). Not derivable from
  // `theaterId` alone — the slug carries the venue name, and `/theaters/x06jr`
  // 404s — so the composition root supplies it or /uptime shows no source link.
  venuePath: Option[String] = None,
  today:     => LocalDate,
  // The rating system the brand's `certificate` field speaks: BBFC for the UK brands, MPA for the US.
  ageRatings: Set[String]   = GatsbyBoxOfficeParser.BbfcCertificates
) extends CinemaScraper with play.api.Logging {

  import GatsbyBoxOfficeClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  /** Both brands are national chains fed by one central box-office backend, so
   *  the Filmweb per-cinema fallback shouldn't shadow them (see
   *  `FallbackEligibility`). */
  override def chain: Boolean = true

  override def sourceUrl: Option[String] = venuePath.map(p => s"$baseUrl$p")
  // The venue path is optional; the platform's own theatre id always identifies it.
  override def sourceKey: Option[String] = Some(s"${CinemaScraper.urlKey(baseUrl)}/theaters/$theaterId")

  /** ONE schedule request covers the entire horizon.
   *
   *  This is the endpoint's distinguishing feature and the reason this scraper
   *  is a plain `CinemaScraper` rather than a `ChunkedCinemaScraper` like the
   *  German and Flicks clients: `from`/`to` take an arbitrary range and the
   *  platform returns every populated day inside it in a single response, with
   *  no server-side cap (probed to a full year). Days with nothing on simply
   *  don't appear, so there is no per-day fan-out to plan, no index/nav fetch
   *  to discover which days exist, and no gap days wasted on empty requests —
   *  the catalogue, the schedule and one details call per 50 films are the
   *  venue's whole scrape.
   *
   *  The catalogue call is the same URL for every venue of a brand, so an HTTP
   *  cache in front collapses it across the chain's venues.
   */
  def fetch(): Seq[CinemaMovie] = {
    val catalogue = HttpRead.page(http, catalogueUrl(baseUrl))
    val schedule  = HttpRead.page(http, scheduleUrl(baseUrl, theaterId, timeZone, today, today.plusDays(MaxHorizonDays.toLong)))
    GatsbyBoxOfficeParser.parse(schedule, catalogue, theaterId, cinema, baseUrl,
      details(GatsbyBoxOfficeParser.scheduledMovieIds(schedule, theaterId)), ageRatings)
  }

  /** The scheduled films' credits and running times, [[DetailsBatch]] ids per request. Optional: a
   *  batch that fails leaves its films without them — the schedule is the scrape, and a listing
   *  without a credit is what every scrape published before this request existed. */
  private def details(ids: Seq[String]): Map[String, GatsbyBoxOfficeParser.FilmDetails] =
    ids.distinct.sorted.grouped(DetailsBatch).flatMap { batch =>
      // A 200 naming none of the batch's films is a failed read too, asked once more: three Landmark
      // venues' whole batches came back unparseable on 2026-09-29, and every film there was listed
      // bare ("Nosferatu", Eggers' 132 minutes, then resolved as the 1922 film).
      def ask(): Map[String, GatsbyBoxOfficeParser.FilmDetails] = {
        val answered = GatsbyBoxOfficeParser.parseDetails(HttpRead.page(http, detailsUrl(baseUrl, batch)))
        if (answered.isEmpty) throw new IllegalStateException(s"the response named none of the ${batch.size} film(s) asked")
        answered
      }
      try (1 until DetailsAttempts).foldLeft(Try(ask()))((tried, _) => tried.orElse(Try(ask()))).get
      catch {
        case NonFatal(e) =>
          logger.warn(s"${cinema.displayName}: film details for ${batch.size} film(s) unavailable, listing them without credits: ${e.getMessage}")
          Map.empty
      }
    }.toMap
}

object GatsbyBoxOfficeClient {

  val ShowcaseBaseUrl = "https://www.showcasecinemas.co.uk"
  val EverymanBaseUrl = "https://www.everymancinema.com"

  /** Showcase's US sibling — the same National Amusements brand on the same
   *  platform, a SEPARATE host (`.com`, not `.co.uk`) and therefore a separate
   *  pace bucket and `HostPolicy` row. Its 13 venues are the chain's whole US
   *  roster; see `docs/venue-maps/US-WEBEDIA-VENUE-MAP.tsv`. */
  val ShowcaseUsBaseUrl = "https://www.showcasecinemas.com"

  /** Landmark Theatres — 26 US arthouse venues on the same platform. The chain
   *  most exposed to a short horizon (repertory and one-off event stock), and
   *  measured against flicks.us before being wired primary: see the horizon note
   *  in `AlamoDrafthouseClient` for why that check gates every US chain here. */
  val LandmarkBaseUrl = "https://www.landmarktheatres.com"

  val UkTimeZone: String = TimeZones.UnitedKingdom.getId

  /** The shared scrape horizon — see [[ScrapeHorizon]]. This one bounds the PAYLOAD we
   *  parse rather than a request count, but the consequence of cutting it was the same:
   *  advance bookings trickle to ~10 months out (a handful of dates reaching 2027-05-30)
   *  and the old 210-day cap kept them out of the listing, so scrape-prune deleted them. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  /** The chain-wide film catalogue (Gatsby static query `allMovie`). The hash is
   *  Gatsby's digest of the query TEXT, so it is identical on every brand's
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

  /** How many films one details request asks about. */
  val DetailsBatch = 50

  /** How many times one details batch is asked before its films are listed without credits. */
  val DetailsAttempts = 2

  /** The films' credits and running times, as the film page loads them: one repeated `ids` per film. */
  def detailsUrl(baseUrl: String, ids: Seq[String]): String =
    s"$baseUrl/api/gatsby-source-boxofficeapi/movies?${ids.map(id => s"ids=${URLEncoder.encode(id, StandardCharsets.UTF_8)}").mkString("&")}"
}
