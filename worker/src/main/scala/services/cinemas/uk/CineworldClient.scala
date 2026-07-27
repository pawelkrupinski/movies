package services.cinemas.uk

import models.{Cinema, CinemaMovie, CineworldChain, Source}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, FilmDetail, ScrapeHorizon}
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}
import scala.util.Try

/**
 * Cineworld — the UK's second-largest chain (87 sites), served off the same
 * Regal-built `quickbook` data API Cinema City Poland uses, under the UK chain
 * code 10108. Plain GETs, no auth, JSON:
 *
 *   GET /uk/data-api-service/v1/quickbook/10108/dates/in-cinema/<id>/until/<date>
 *     → `body.dates[]` — the days this venue has a programme on
 *   GET /uk/data-api-service/v1/quickbook/10108/film-events/in-cinema/<id>/at-date/<date>
 *     → `body.films[]` (title, length, poster, film page, trailer) +
 *       `body.events[]` (one screening each: `filmId`, `eventDateTime`,
 *       `bookingLink`, `auditorium`, `attributeIds`)
 *
 * One instance serves one venue — its numeric Cineworld site code (`cinemaId`,
 * e.g. "031" = Sheffield) plus the [[Cinema]] it feeds, mirroring
 * [[FlicksClient]]. `CINEWORLD-VENUE-MAP.tsv` at the repository root maps every
 * API venue id to its case object. Parsing lives in [[CineworldParser]].
 *
 * The venue roster itself (`cinemas/with-event/until/<date>` → `body.cinemas[]`)
 * is NOT fetched at scrape time — the id↔`Cinema` pairing is wired statically in
 * `CinemaScraperCatalog`. `CineworldParser.parseVenues` reads it for the venue
 * map, pinned against a recorded roster by the spec.
 */
class CineworldClient(
  http:     HttpFetch,
  cinemaId: String,
  override val cinema: Cinema,
  today:    LocalDate = LocalDate.now(ZoneId.of("Europe/London"))
) extends ChunkedCinemaScraper with DetailEnricher {

  import CineworldClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def chain: Boolean = true

  /** Cineworld's listing feed omits synopsis/cast/director — they live on each
   *  film's detail page (`/films/<slug>/<id>`, the `filmUrl` the listing leaves
   *  on the movie), so this scraper opts into deferred detail enrichment.
   *
   *  `detailGroup` is chain-wide ("cineworld"): one fetch per film per freshness
   *  window serves the whole chain. Detail here is DISPLAY enrichment only —
   *  Cineworld rows already resolve from their listing title (there is no
   *  detail-only TMDB hint we hold them for; the listing has no director/year),
   *  so `defersTmdbResolution` is false: rows reach the read model straight off
   *  the listing and the synopsis/cast/director merge in asynchronously.
   *
   *  `detailTarget` routes the fetched detail into the single `CineworldChain`
   *  network slot (as Cinema City does with `CinemaCityChain`) rather than an
   *  arbitrary venue's — the 87 venues share `detailGroup "cineworld"`, so one
   *  fetch per film serves the chain and lands in one shared slot. */
  override val detailGroup: String = "cineworld"
  override def detailTarget: Source = CineworldChain
  override def enrichmentServiceOverride: Option[String] = Some("Cineworld Enrichment")
  override def defersTmdbResolution: Boolean = false

  /** Fetch + parse one film's detail page by the `filmUrl` the listing left on
   *  the movie. Same `http` the listing scrape uses. None on fetch failure so
   *  the task stays stale and is retried rather than recording an empty result
   *  as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.get(ref)).toOption.map(CineworldParser.parseDetail)

  /** The venue's public page. Cineworld's canonical URL is
   *  `/cinemas/<slug>/<siteCode>` and the numeric site code alone resolves it —
   *  `/cinemas/031/031` serves the Sheffield page, the leading segment being a
   *  decorative slug the router ignores (verified 2026-07-27). That keeps the
   *  /uptime link derivable from the one id the scraper is wired with, with no
   *  second per-venue slug table to drift from the roster. */
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/cinemas/$cinemaId/$cinemaId")

  /** The days to scrape, read off the venue's own `dates` endpoint rather than
   *  guessed as a fixed grid — one cheap call names precisely which days have a
   *  programme, so no request is spent on an empty day and no advertised day is
   *  missed. Cineworld's list is dense day-by-day for the near term and then
   *  sparse (Sheffield, 2026-07-27: every day for five weeks, then 13 scattered
   *  advance-sale days out to 2026-12-01).
   *
   *  Bounded to `[today, today + MaxHorizonDays]`, and the bound is pushed into
   *  the request's own `until` parameter so the API does the trimming. Each day
   *  is one `film-events` call, and this chain is 87 venues, so the horizon is
   *  the whole per-cycle request budget: 36 days × 87 venues ≈ 3.1k calls per
   *  pass, which at the UK worker's cadence paces out to a handful a minute.
   *  Reaching for the sparse tail would push past 4k for a dozen single
   *  screenings a venue — the near-term dense block is what the listing is for.
   *
   *  A day-list failure (`http.get` throwing) propagates and fails the whole
   *  scrape, which keeps the venue's last-known listing rather than narrowing it
   *  to a guess; an EMPTY list is expected data for a venue with nothing on. */
  def planChunks(): Seq[String] =
    CineworldParser.parseDates(http.get(datesUrl(cinemaId, today.plusDays(MaxHorizonDays.toLong))))
      .filter(d => !d.isBefore(today) && !d.isAfter(today.plusDays(MaxHorizonDays.toLong)))
      .map(_.toString)

  /** Fetch + parse ONE day's `film-events` response. A throw reschedules only
   *  this day's chunk task, leaving the venue's other days alone. */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] =
    CineworldParser.parseDay(http.get(filmEventsUrl(cinemaId, LocalDate.parse(dateKey))), cinema)
}

object CineworldClient {

  val BaseUrl = "https://www.cineworld.co.uk"

  /** Cineworld's `quickbook` chain code (Cinema City Poland is 10103 on the
   *  identical API). */
  private val BaseApiUrl = s"$BaseUrl/uk/data-api-service/v1/quickbook/10108"

  /** Every quickbook URL carries the same two query parameters: `attr=` (no
   *  attribute filter — we want every screening) and the response locale. */
  private val Query = "?attr=&lang=en_GB"

  /** The shared scrape horizon — see [[services.cinemas.common.ScrapeHorizon]] for why
   *  it is a sanity bound rather than a budget, and what the old 35-day budget cost. The
   *  venue's own `dates` endpoint names exactly which days exist, so no day is fetched
   *  blind: sampled 2026-07-27 the tail is 3-28 extra days per venue (Sheffield: 55 days,
   *  19 past the old cap, ending 2027-04-22). */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  def datesUrl(cinemaId: String, until: LocalDate): String =
    s"$BaseApiUrl/dates/in-cinema/$cinemaId/until/$until$Query"

  def filmEventsUrl(cinemaId: String, date: LocalDate): String =
    s"$BaseApiUrl/film-events/in-cinema/$cinemaId/at-date/$date$Query"

  /** The chain roster — every venue with a screening between now and `until`.
   *  Read by `CineworldParser.parseVenues` to build `CINEWORLD-VENUE-MAP.tsv`. */
  def venuesUrl(until: LocalDate): String =
    s"$BaseApiUrl/cinemas/with-event/until/$until$Query"
}
