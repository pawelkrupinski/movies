package services.cinemas.common

import services.titles.NonMovieEventClassifier
import tools.{HttpFetch, HttpRead}
import models._

import java.time.LocalDate
import scala.util.Try

/**
 * Webedia website-JSON showtimes scraper — the surviving reach into the
 * AlloCiné / Filmstarts / SensaCine / Beyazperde family after the old
 * `api.allocine.fr` mobile REST API (partner key + SHA-1 `sig`) was
 * decommissioned: its whole `api.*` subdomain family (incl. `api.filmstarts.de`,
 * `api.screenrush.co.uk`) was removed from DNS in 2026, and the replacement
 * GraphQL backend (`graph.allocine.fr`) sits behind a per-session JWT.
 *
 * What still answers unauthenticated is the WEBSITE's own internal JSON, hit by
 * every Webedia country site with an identical path + schema — only the host,
 * the theater-id LETTER prefix and the (unused-here) localized city-list path
 * differ:
 *
 *   GET https://www.<host>/_/showtimes/theater-<id>/d-<YYYY-MM-DD>/p-<page>/
 *     → { results:[ { movie, showtimes }, … ],
 *         pagination:{ page, totalPages, itemsPerPage, totalItems }, nextDate, … }
 *
 * `results[].movie` carries `title` + `originalTitle` (a strong TMDB-resolution
 * hint for the German market, where the local title is a translation),
 * `data.productionYear`, `runtime` ("1 Std. 56 Min."), `genres`, the director
 * (`credits[]` where `position.name == "DIRECTOR"`) and a `poster`.
 * `results[].showtimes` is an object keyed by language-version bucket
 * (`original` / `dubbed` / `local` / …); each bucket is an array of screenings
 * whose `startsAt` is a local ISO `LocalDateTime` (no zone — it is already the
 * venue's wall-clock) and whose booking deep-link is `data.ticketing[].urls`.
 *
 * One instance serves one venue — its `theaterId` (Germany: "A0263", Spain:
 * "E0123") + the [[Cinema]] it feeds, mirroring [[FilmwebShowtimesClient]].
 * Everything that differs between the family's national sites is carried by
 * [[WebediaMarket]], so the same client serves Germany and Spain (and would
 * serve FR/TR/BR/MX by adding a market). TMDB enriches synopsis/cast downstream,
 * so this client only carries what the JSON actually provides.
 */
class WebediaShowtimesClient(
  http:      HttpFetch,
  market:    WebediaMarket,
  theaterId: String,              // e.g. "A0263" — the letter prefix is per-country
  override val cinema: Cinema,
  /** The VENUE's calendar day the horizon is measured from, asked per plan —
   *  built through [[WebediaShowtimesClient.forVenue]], which reads the venue's
   *  city zone. Same shape as [[FlicksClient]]. */
  today:     => LocalDate
) extends ChunkedCinemaScraper {

  import WebediaShowtimesClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(s"https://${market.host}")

  // The public, browser-renderable venue page. This ONE path is localized per
  // market (`/kinoprogramm/kino/` in Germany, `/cines/cine-` in Spain); the JSON
  // endpoint above is uniform across the family, so it is the only piece of the
  // URL shape that moved onto WebediaMarket when Spain landed.
  override def sourceUrl: Option[String] = Some(market.venuePageUrl(theaterId))

  // Each populated day is one chunk, run as its own `ScrapeChunk` task (see
  // ChunkedCinemaScraper / ScrapeChunkHandler). The days spread across the task
  // queue and the market's shared pace gate instead of bursting from a single
  // task that parks a worker thread for days×1s. The in-process `fetch()` the
  // trait composes (planChunks → fetchChunk → reduceChunks) is used only by the
  // deterministic fixture harness + unit tests.

  /** The days to scrape, read off the venue page's (`sourceUrl`)
   *  `data-showtimes-dates` attribute — the exact days that have screenings
   *  inside the site's own booking window, gap days excluded (verified empty in
   *  the per-day API) — ~28 days on Filmstarts, ~21 on SensaCine. Reading it once lifts the horizon from
   *  the old fixed 7-day grid to the site's full window WITHOUT firing a request
   *  per empty day: the page names precisely which days to fetch. Data does exist
   *  sparsely beyond that window, but it is unadvertised (no signal names those
   *  days) and it rolls into the window — and so into this list — well before its
   *  showtime, so a ~28-day horizon captures it in time without blind probing.
   *
   *  No fallback: this IS the nav/index fetch the `ChunkedCinemaScraper` contract
   *  allows, and its failure fails the whole scrape (recorded as a normal
   *  outcome). A fetch error propagates; a 200 that lacks the attribute entirely
   *  — the market's markup changed, or a block/error page came back — is a
   *  discovery failure too, so `parseShowtimeDates` returns `None` and we throw
   *  rather than silently scraping nothing. An attribute that IS present but
   *  lists no days is a legitimately empty venue, not a failure — the page itself
   *  saying so ([[planSchedule]]). Bounded to `[today, today+MaxHorizonDays]` so a
   *  stray attribute date can't balloon the chunk fan-out. */
  def planChunks(): Seq[String] = planSchedule().keys

  /** [[planChunks]], plus whether an empty plan is the PAGE saying the venue has no
   *  schedule: the attribute present and naming no day at all, which is what a summer
   *  cinema closed for the season renders (2026-10-04..06: four Spanish ones held
   *  CinemaContentStaleVenuesGrowing for a day without it). Days advertised only past the
   *  horizon vouch for nothing and stay a plain empty; a missing attribute still throws. */
  override def planSchedule(): ChunkPlan = {
    val url  = sourceUrl.getOrElse(throw new IllegalStateException("WebediaShowtimesClient has no sourceUrl"))
    val html = HttpRead.page(http, url)
    val day  = today
    val all  = parseShowtimeDates(html).getOrElse(throw new IllegalStateException(
      s"$url carries no data-showtimes-dates attribute — ${market.host} markup changed?"))
    if (all.isEmpty) ChunkPlan.NoScheduleListed
    else ChunkPlan.of(all.filter(d => !d.isBefore(day) && !d.isAfter(day.plusDays(MaxHorizonDays.toLong))).map(_.toString))
  }

  /** Fetch + parse ONE day into that day's films. A page-1 fetch failure THROWS
   *  so ONLY that day's chunk task reschedules (the per-day retry); the other
   *  days are unaffected. A day that ANSWERS with no films is a valid empty
   *  result, not a failure — Webedia serves an empty `results` for far-future
   *  days. Spillover pages (>20 films/day, rare): page 1 already answered, so a lost
   *  page 2 leaves its films out and marks the listing incomplete rather than failing the day.
   *
   *  (The old monolithic fetch swallowed a day's failure to None and threw only
   *  if ALL days failed, to avoid feeding AdaptiveTimeoutScraper a fast-empty
   *  "success". Chunked scrapers skip that wrapper, and the total-failure case is
   *  now the empty reduce — no chunk lands → recordCinemaScrape's empty-guard
   *  keeps last-known data.) */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] = {
    val date  = LocalDate.parse(dateKey)
    ListingPages.readNumbered("webedia-spillover", showtimesUrl(market.host, theaterId, date, _), (_: WebediaShowtimesPage.Page).totalPages,
        (_: WebediaShowtimesPage.Page).films)(url => WebediaShowtimesPage.parsePage(HttpRead.page(http, url), market))
      .filterNot(raw => NonMovieEventClassifier.isNonScreening(raw.title))
      .map(raw => toCinemaMovie(raw, raw.showtimes))
  }

  /** Merge every day's films into the venue's listing: one row per Webedia film
   *  id, showtimes unioned, deduped by (time, booking) and time-ordered — the same
   *  grouping the monolithic scrape used, so `reduceChunks ∘ fetchChunk ∘
   *  planChunks` equals the old `fetch()`. Overrides the identity default because
   *  Webedia rows carry no `filmUrl` (that default would collapse films to their
   *  title); the Webedia id (`externalIds("webedia")`) is the stable key. A film
   *  with no showtimes on any day drops out. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    SlotsToMovies.mergeMovies(chunks.toSeq.sortBy(_._1).flatMap(_._2), m => m.externalIds.getOrElse("webedia", m.movie.title),
      order = SlotsToMovies.MovieOrder.byKey)

  /** Build the venue-agnostic film row from one parsed Webedia result. The
   *  showtimes to attach are supplied by the caller — one day's in `fetchChunk`,
   *  the cross-day union in `reduceChunks`. */
  private def toCinemaMovie(raw: WebediaShowtimesPage.RawWebediaFilm, showtimes: Seq[Showtime]): CinemaMovie =
    CinemaMovie(
      movie = Movie(
        title          = raw.title,
        runtimeMinutes = raw.runtimeMinutes,
        releaseYear    = raw.year,
        genres         = raw.genres,
        // Carry the international title only when it differs from the German one —
        // for German films the site echoes the same string there.
        originalTitle  = raw.originalTitle.filter(_ != raw.title)
      ),
      cinema      = cinema,
      ageRating   = raw.ageRating,
      posterUrl   = raw.posterUrl,
      // no filmUrl: no stable public film-page URL is derivable from the JSON
      synopsis    = raw.synopsis,
      director    = raw.director,
      showtimes   = showtimes,
      externalIds = Map("webedia" -> raw.internalId.toString)
    )
}

object WebediaShowtimesClient {

  /** A venue's client, planning from the venue's OWN calendar day (its city's zone) per
   *  scrape. Spain has two zones: on the peninsula's "today" a Canary venue (an hour
   *  behind) lost its still-running evening's day for the last hour before its midnight. */
  def forVenue(http: HttpFetch, market: WebediaMarket, theaterId: String, cinema: Cinema,
               venueClock: VenueClock): WebediaShowtimesClient =
    new WebediaShowtimesClient(http, market, theaterId, cinema, today = venueClock.todayAt(cinema, market.zoneId))

  /** The shared scrape horizon — see [[services.cinemas.common.ScrapeHorizon]].
   *
   *  Was 34, chosen as "a touch above" Filmstarts' own ~28-day booking calendar.
   *  That reasoning is right about what Filmstarts advertises TODAY (a venue page
   *  probed 2026-08-30 offered 7 dates reaching +21d, and the German corpus's
   *  furthest screening sat at +28d) and wrong as a design: it is a per-client cap
   *  of exactly the kind `ScrapeHorizon` exists to abolish. The day list still
   *  comes from the venue page's own `data-showtimes-dates`, so this bound costs
   *  NOTHING while the source stays short — it only stops mattering the day the
   *  source's window grows, which the old comment itself anticipated ("or a future
   *  window bump").
   *
   *  That day is the dangerous one, because the failure is silent and total:
   *  `MovieCache`'s scrape-prune reads a film's absence from a listing as "it
   *  stopped screening", so any film living only beyond the cap is deleted on
   *  every COMPLETE scrape — no error, no failed fetch. That is precisely how the
   *  UK lost its whole advance-sale programme on 2026-07-27, and Germany has no
   *  aggregator fallback to have caught it. Sharing the one number is the cheap
   *  insurance. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  def showtimesUrl(host: String, theaterId: String, date: LocalDate, page: Int): String =
    s"https://$host/_/showtimes/theater-$theaterId/d-$date/p-$page/"

  private val ShowtimesDatesAttr = """data-showtimes-dates="([^"]*)"""".r
  private val IsoDate            = """\d{4}-\d{2}-\d{2}""".r

  /** The days a venue has showtimes on, read off the venue page's
   *  `data-showtimes-dates="[&quot;2026-07-19&quot;,…]"` — an HTML-entity-escaped
   *  JSON array of ISO dates spanning the site's own booking window with gap
   *  days omitted (~28 days on Filmstarts, ~21 on SensaCine). The entity-escaping is irrelevant to a date regex, so pull the
   *  ISO dates straight out of the attribute value; deduped and sorted.
   *
   *  `None` when the attribute is ABSENT — unexpected markup, so the caller fails
   *  the scrape rather than scrape nothing. `Some(dates)` when present, where
   *  `Some(Nil)` (an empty `[]`) is a legitimately empty venue, not a failure.
   *  Pure + public so a spec feeds it a recorded page directly. */
  def parseShowtimeDates(html: String): Option[Seq[LocalDate]] =
    ShowtimesDatesAttr.findFirstMatchIn(html).map { m =>
      IsoDate.findAllIn(m.group(1)).toSeq.flatMap(s => Try(LocalDate.parse(s)).toOption).distinct.sortBy(_.toString)
    }
}
