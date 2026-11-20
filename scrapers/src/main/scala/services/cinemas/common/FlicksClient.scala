package services.cinemas.common

import tools.{HttpFetch, HttpRead}
import models._

import java.time.LocalDate
import scala.util.Try

/**
 * Flicks — a nationwide cinema-listings aggregator (chains + independents) that
 * runs the SAME site on several ccTLDs. One instance serves one venue of one
 * [[FlicksMarket]]: `flicks.co.uk` for the UK — the chosen source there after
 * Webedia retired its British sibling Screenrush (`screenrush.co.uk` is gone
 * from DNS, so [[services.cinemas.common.WebediaShowtimesClient]] can't reach the
 * UK) — and `flicks.us` for the United States, which is why this client lives in
 * `services.cinemas.common` rather than a per-country package.
 *
 * Flicks renders a cinema's programme server-side but loads each day's sessions
 * on demand: the cinema page carries empty `<div data-date=…>` day tabs, and the
 * real showtimes come from an AJAX fragment (the request needs an
 * `is-ajax-call: yes` header):
 *
 *   GET <market base>/cinema/sessions/<slug>/<YYYY-MM-DD>/
 *     → an HTML fragment of `<article class="cinema-times__article">` per film:
 *        `h3.cinema-times__movie-title`, a `/movie/<slug>/` link, the runtime
 *        (`.cinema__movie-duration` "90 mins"), the FIRST director only
 *        (`.cinema__director span`), and a `ul.times-calendar-times` of session
 *        buttons — carrying the 24h time in `data-optlabel` (falling back to the
 *        visible "h:mm am/pm") and a premium/format label (IMAX, LuxeSuite,
 *        iSense…) on variant screenings. A button is an `<a>` to the cinema
 *        chain's booking deep-link only where the venue has online booking wired
 *        in; venues without it render the identical button as a `<span>`, so the
 *        session is matched by CLASS, not tag, and its booking link is simply
 *        absent.
 *
 * One instance serves one venue — its Flicks `cinemaSlug` + the [[Cinema]] it
 * feeds, mirroring [[FilmwebShowtimesClient]]. One AJAX call per day. Each
 * session button carries a `data-eventjson` blob from which we lift the film's
 * numeric Flicks id (`content_id`, an `externalId`), its `content_cast` and
 * `content_director` (comma-separated, and LOWERCASED by Flicks — see `PersonName`;
 * the card shows only the first director, the blob all of them) and
 * `content_genre` (comma-separated); the film card also
 * carries a `/trailer/` link.
 *
 * The fragment carries no year, country or synopsis — those are only on the film's
 * `/movie/<slug>/` page ([[FlicksFilmPage]]), read as this client's deferred
 * [[DetailEnricher]] detail. That page is the same at every venue Flicks lists the
 * film at, so the detail group spans the MARKET (`flicks|UK`, `flicks|US`), not the
 * venue: one `EnrichDetails` task per film per market, its read landing on every
 * venue slot that names the page (`pagesSharedAcrossVenues`). The fetch itself goes
 * through `detailHttp`, the catalogue's shared Mongo `detail_cache` (chain
 * `flicks`, keyed by the page URL — market host + slug, since Flicks reuses slugs
 * across its ccTLDs), so a fallback client or a second worker asking within the TTL
 * is answered from the cache.
 */
class FlicksClient(
  http:       HttpFetch,
  cinemaSlug: String,
  override val cinema: Cinema,
  market:     FlicksMarket,
  // The film pages' fetch: the catalogue's shared detail cache, so a film's page is fetched once
  // per TTL for the whole market, whichever venue (or fallback) asks.
  detailHttp: HttpFetch,
  // The VENUE's calendar day, asked per plan — see `forVenue`: a worker in Europe planning
  // US venues must not start from a date those venues have not reached.
  today:      => LocalDate
) extends PagedChunkScraper with DetailEnricher {

  import FlicksClient._

  private val baseUrl = market.baseUrl
  private val programmeUrl = s"$baseUrl/cinema/$cinemaSlug/"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(programmeUrl)

  // Each populated day is one chunk, run as its own `ScrapeChunk` task (see
  // ChunkedCinemaScraper / ScrapeChunkHandler). The days spread across the task
  // queue and the shared Flicks pace gate instead of bursting from a single task
  // that parks a worker thread for that many back-to-back AJAX calls. The
  // in-process `fetch()` the trait composes (planChunks → fetchChunk →
  // reduceChunks) serves the fixture harness, the unit tests and — sequentially,
  // in the caller's task — a chain venue's Flicks fallback (SourceFallbackScraper).

  /** The days to scrape, read off the venue programme page's
   *  `<div class="timetable__day" data-date="YYYY-MM-DD">` day tabs — the exact
   *  days with sessions, gap days excluded (a date absent from the tab list
   *  returns an empty sessions fragment). The list is sparse and reaches months
   *  out (Flicks advertises a venue's whole booking horizon, not a fixed window),
   *  so reading it once gives the site's full advertised horizon WITHOUT firing a
   *  request per empty day: the page names precisely which days to fetch. Bounded
   *  to `[today, today+MaxHorizonDays]` so a stray attribute date can't balloon
   *  the per-venue chunk fan-out.
   *
   *  The programme page is the ONLY source of days — there is no fixed-grid
   *  fallback. When the page can't be fetched (its `http.get` throws, propagating
   *  here) this throws: an index-page failure fails the whole scrape (recorded as
   *  a normal scrape outcome by the planner), which keeps the venue's last-known
   *  listing rather than silently narrowing it to a guessed 7-day window that
   *  would drop every advertised far-out day.
   *
   *  A page that fetched fine but names no day is split in two, the same line
   *  [[WebediaShowtimesClient]] draws for Germany:
   *   - NO timetable block at all → we aren't parsing the page we think we are
   *     (markup drift, an error page, a redirect). A FAILURE, so it throws.
   *   - timetable block present, no day tabs in it → the venue simply has
   *     nothing on. EXPECTED DATA, so it returns empty: the planner records that
   *     as a successful scrape of an empty repertoire, and
   *     `MovieCache.recordCinemaScrape` bails on an empty result so the venue
   *     keeps its last-known listing regardless. Throwing here instead left five
   *     UK venues permanently red on /uptime (2026-07-26). */
  def planChunks(): Seq[String] = planSchedule().keys

  /** [[planChunks]], plus whether an empty plan is the PAGE saying the venue has no
   *  schedule: no day tab anywhere on it, and Flicks' own "no times" notice
   *  (`no-streaming-sessions`) inside the timetable block — what a drive-in closed for
   *  the season renders. An empty plan without that notice (only far-out tabs, or a
   *  bare block) vouches for nothing and stays a plain empty. */
  override def planSchedule(): ChunkPlan = {
    val html  = HttpRead.page(http, programmeUrl)
    val all   = parseProgrammeDates(html)
    val day   = today
    val dates = all.filter(d => !d.isBefore(day) && !d.isAfter(day.plusDays(MaxHorizonDays.toLong)))
    if (dates.isEmpty && !hasTimetable(html))
      throw new IllegalStateException(
        s"Flicks programme page for '$cinemaSlug' carried no timetable block")
    if (all.isEmpty && saysNoSessions(html)) ChunkPlan.NoScheduleListed
    else ChunkPlan.of(dates.map(_.toString))
  }

  /** Fetch + parse ONE day's sessions fragment into that day's films. The fetch
   *  THROWS on failure so ONLY that day's chunk task reschedules (the per-day
   *  retry); the other days are unaffected. A day that ANSWERS with an empty
   *  fragment (no programme) is a valid empty result, not a failure. */
  def fetchChunkPage(dateKey: String): String =
    HttpRead.page(http, sessionsUrl(market, cinemaSlug, LocalDate.parse(dateKey)), AjaxHeaders)

  def parseChunkPage(dateKey: String, page: String): Seq[CinemaMovie] = {
    val date = LocalDate.parse(dateKey)
    moviesFor(FlicksDayPage.parseDay(page, date, market))
  }

  def pageParser: String = FlicksDayPage.PageParser

  /** Merge every day's films into the venue's listing: one row per film (grouped
   *  by its stable `/movie/<slug>` `filmUrl`), showtimes unioned, deduped by
   *  (time, booking) and time-ordered — the same grouping the monolithic scrape
   *  used, so `reduceChunks ∘ fetchChunk ∘ planChunks` equals the old `fetch()`.
   *  Overrides the identity default only to keep the exact (time, booking) dedup
   *  key and the by-title final ordering. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    ChunkedCinemaScraper.mergeByIdentity(chunks.toSeq.sortBy(_._1).flatMap(_._2), s => (s.dateTime, s.bookingUrl))
      .filter(_.showtimes.nonEmpty)
      .sortBy(_.movie.title)

  // ── deferred detail: the film page's year, runtime, directors, countries, cast, synopsis ──

  /** One group per MARKET: a film page is the same page at every venue of it (see the class doc). */
  override val detailGroup: String = detailGroupOf(market)
  override def pagesSharedAcrossVenues: Boolean = true
  /** One /uptime row for the market's film pages, not one per venue. */
  override def enrichmentServiceOverride: Option[String] = Some(s"Flicks ${market.label} Enrichment")

  /** `ref` is the listing's `filmUrl`, the film page itself. A page Flicks no longer has is a 404, let
   *  escape as gone. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(detailHttp, ref).map(FlicksFilmPage.parse(_, slugOf(ref), today))

  /** Build one film row per stable `/movie/<slug>` from a day's session slots,
   *  showtimes deduped by (time, booking) and time-ordered. */
  private def moviesFor(slots: Seq[FlicksDayPage.RawFlicksSlot]): Seq[CinemaMovie] =
    SlotsToMovies.fold(slots, _.slug, s => Showtime(s.dateTime, s.booking, None, s.format), order = SlotsToMovies.MovieOrder.asGrouped) { (_, group, showtimes) =>
        val head = group.head
        CinemaMovie(
          movie       = Movie(title = head.title, runtimeMinutes = head.runtimeMinutes, genres = head.genres),
          cinema      = cinema,
          posterUrl   = head.posterUrl,
          filmUrl     = Some(s"$baseUrl/movie/${head.slug}/"),
          cast        = head.cast,
          director    = head.directors,
          showtimes   = showtimes,
          externalIds = head.contentId.map("flicks" -> _).toMap,
          trailerUrl  = head.trailerUrl,
          ageRating   = head.ageRating
        )
    }
}

object FlicksClient {

  /** A venue's client, planning from the venue's OWN calendar day (its city's zone) per
   *  scrape — the catalogue's primary and a chain venue's fallback both build it here, so
   *  neither can fall back to the market-wide zone a multi-zone country gets wrong. */
  def forVenue(http: HttpFetch, detailHttp: HttpFetch, cinemaSlug: String, cinema: Cinema, market: FlicksMarket,
               venueClock: VenueClock): FlicksClient =
    new FlicksClient(http, cinemaSlug, cinema, market, detailHttp, today = venueClock.todayAt(cinema, market.zoneId))

  /** The detail group of `market`'s film pages: every venue of the market shares it. */
  def detailGroupOf(market: FlicksMarket): String = s"flicks|${market.label}"

  /** How long the shared detail cache keeps a film page: inside the `DetailEnrich` refresh window, like
   *  every chain's (`CinemaScraperCatalogSpec`). */
  val DetailTtl: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(2).hours

  /** The slug off a film page URL (`https://www.flicks.us/movie/<slug>/`). */
  def slugOf(filmUrl: String): String = FlicksDayPage.SlugPat.findFirstMatchIn(filmUrl).fold("")(_.group(1))

  /** The shared scrape horizon — see [[services.cinemas.common.ScrapeHorizon]]. Flicks
   *  advertises a venue's whole booking horizon as day tabs and we fetch every advertised
   *  day; this only bounds a stray far-future tab. Deliberately the SAME number the chain
   *  clients use, so a venue's primary and its Flicks fallback cover one window and
   *  neither prunes the other's tail. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  /** A Flicks film page (`/movie/<slug>/`) links the film's Letterboxd and Rotten Tomatoes pages in its ratings block
   *  (about 70% of pages, measured 2026-10-05) — exact ids the identity's catalogue take maps to TMDB. Those links are
   *  not detail fields: the agreement reads them for a cluster nothing else took, apart from the detail read. */
  val CatalogueLinkPages: services.identity.CatalogueLinkPages =
    services.identity.CatalogueLinkPages(FlicksMarket.all.map(_.host).toSet, Set("letterboxd", "rt"))

  private val DataDate = """data-date="(\d{4}-\d{2}-\d{2})"""".r
  // The venue programme page's day-tab container. Rendered on EVERY venue page,
  // including one with nothing on (verified 2026-07-27 across venues both with
  // and without listings), so its absence — not the absence of day tabs — is what
  // marks a page we failed to parse. See `planChunks`.
  private val TimetableBlock = """class="timetable timetable--cinema"""".r

  /** The days a venue has a programme on, read off the programme page's
   *  `<div class="timetable__day" data-date="YYYY-MM-DD">` day tabs — a sparse,
   *  months-long list with gap days omitted (a date absent from it returns an
   *  empty sessions fragment). Every `data-date` on the page is a day tab, so
   *  pull the ISO dates straight out; deduped and sorted. Empty when no tab is
   *  present (older / changed markup) → the caller falls back to the fixed grid.
   *  Pure + public so a spec feeds it a recorded page directly. */
  def parseProgrammeDates(html: String): Seq[LocalDate] =
    DataDate.findAllMatchIn(html).map(_.group(1)).toSeq
      .flatMap(s => Try(LocalDate.parse(s)).toOption)
      .distinct.sortBy(_.toString)

  /** Whether the page carries the venue day-tab container — i.e. it really is a
   *  Flicks programme page, whether or not the venue has anything on. */
  def hasTimetable(html: String): Boolean = TimetableBlock.findFirstIn(html).isDefined

  // Flicks' notice inside the timetable block for a venue it holds no times for ("Sorry, we
  // haven't received movie times for this cinema yet"), seen on closed-for-season US drive-ins
  // and dormant UK venues alike.
  private val NoSessionsNotice = """class="no-streaming-sessions\b""".r

  /** Whether the page carries Flicks' own "no times for this cinema" notice. */
  def saysNoSessions(html: String): Boolean = NoSessionsNotice.findFirstIn(html).isDefined

  // Flicks 403s a non-browser fetch and only serves the sessions fragment (rather
  // than the full page) when this header is set; RealHttpFetch already sends a
  // browser User-Agent, so this is the one extra header the endpoint needs.
  private val AjaxHeaders = Map("is-ajax-call" -> "yes")

  def sessionsUrl(market: FlicksMarket, cinemaSlug: String, date: LocalDate): String =
    s"${market.baseUrl}/cinema/sessions/$cinemaSlug/$date/"
}
