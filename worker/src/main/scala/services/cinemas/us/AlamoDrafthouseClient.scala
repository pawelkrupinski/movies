package services.cinemas.us

import models.{Cinema, CinemaMovie}
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScrapeHorizon}
import tools.{HttpFetch, HttpRead}

import java.time.LocalDate
import scala.concurrent.duration._

/**
 * Alamo Drafthouse — 40 US venues, served off the chain's own public JSON API
 * (the "mother" service its website is built on). Plain GETs, no auth, no
 * cookie, no browser challenge from our datacenter egress (verified 2026-08-30):
 *
 *   GET /s/mother/v2/schedule/venue/<venueSlug>
 *     → `data.sessions[]`   — one screening each: `cinemaId`, `sessionId`,
 *                             `presentationSlug`, `showTimeClt` (the venue's own
 *                             wall-clock), `screenNumber`, `formatSlug`,
 *                             `sessionAttributeSlugs`, `isHidden`
 *       `data.presentations[]` — the film side: `slug`, `show.title`,
 *                             `show.posterImages[]`, `show.certification`
 *       `data.formats[]`    — the venue's format slug → display-title vocabulary
 *       `data.market[]`     — the market roster, from which our slug resolves to
 *                             the numeric `cinemaId` the sessions carry
 *
 * ONE REQUEST PER VENUE PER SWEEP. This is the endpoint's distinguishing
 * feature and the reason this is a plain [[CinemaScraper]] rather than a
 * `ChunkedCinemaScraper` like the Flicks and Cineworld clients: the venue's
 * WHOLE advertised programme comes back in one response. There is no date
 * parameter to page over, no day-tab index to read first, and no request spent
 * on an empty day — so the 36-requests-per-venue that the same venue costs on
 * `flicks.us` becomes 1.
 *
 * THE HORIZON IS WHY THIS CAN GO PRIMARY AT ALL. A chain client that saw less
 * of the programme than the aggregator it replaces would not merely miss the
 * tail — `MovieCache`'s scrape-prune reads a film's absence from a complete
 * listing as "it stopped screening", so every complete scrape would DELETE the
 * advance-sale stock (see [[ScrapeHorizon]] for the day this cost the UK its
 * whole event programme). Measured on the same three venues on 2026-08-30,
 * before wiring:
 *
 *   Lakeline           own 44 days → 2026-12-22   flicks.us 38 days → 2026-12-22
 *   Downtown Brooklyn  own 49 days → 2027-01-01   flicks.us 47 days → 2027-01-01
 *   New Mission        own 35 days → 2026-12-20   flicks.us 35 days → 2026-12-20
 *
 * Same furthest date at every venue, and strictly MORE populated days at two of
 * the three — Alamo's own feed is a superset, which is the bar a primary has to
 * clear. Alamo is exactly the programmer most exposed here (repertory, 70mm
 * revivals, one-off event screenings all live in that tail), so the check was
 * the precondition for this client existing rather than an afterthought.
 *
 * THE FILM'S FACTS ARE A SECOND, DEFERRED READ. The venue payload names a film
 * by title, poster and certificate only; its runtime, release year, director,
 * cast and synopsis come from `v2/schedule/presentation/<show slug>` — what the
 * show page renders from — fetched as this venue's [[DetailEnricher]] detail
 * through the chain's shared detail cache, so a show playing at 30 venues costs
 * one request per cache TTL, not 30. Without them a title-only listing cannot
 * tell Andy Palmer's 2026 "Last Shot" from the 2020 film of that name.
 *
 * One instance serves one venue — its Alamo `venueSlug` (e.g. "lakeline") plus
 * the [[Cinema]] it feeds, mirroring [[services.cinemas.common.FlicksClient]].
 * `docs/venue-maps/ALAMO-DRAFTHOUSE-VENUE-MAP.tsv` maps every API venue to its
 * roster display name. Parsing lives in [[AlamoDrafthouseParser]].
 */
class AlamoDrafthouseClient(
  http:      HttpFetch,
  // The per-show detail fetch: the chain's SHARED detail cache. The same film plays
  // at most of the 40 venues and its show detail is identical at every one, so the
  // cache — not the task scheduler — is what makes it one request per show per TTL
  // fleet-wide rather than one per venue.
  detailHttp: HttpFetch,
  venueSlug: String,
  override val cinema: Cinema,
  // The VENUE's calendar day, asked per scrape, for the far-date sanity bound. The US
  // spans six zones, so the catalog asks it in the venue's own rather than letting a
  // worker in Europe decide.
  today:     => LocalDate
) extends CinemaScraper with DetailEnricher {

  import AlamoDrafthouseClient._


  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  /** A national chain fed by one central schedule service, so the per-cinema
   *  aggregator fallback shouldn't shadow it (see `FallbackEligibility`). */
  override def chain: Boolean = true

  override def sourceUrl: Option[String] = Some(s"$BaseUrl/theater/$venueSlug")

  /** The venue's whole programme in one call.
   *
   *  A fetch failure PROPAGATES rather than folding to an empty listing: the
   *  scrape is then recorded as failed and the venue keeps its last-known
   *  showtimes. An empty-but-well-formed response is the opposite case and
   *  returns EMPTY — a venue with nothing on is data, not an outage, and
   *  throwing there is what left five UK venues permanently red on /uptime.
   */
  def fetch(): Seq[CinemaMovie] =
    AlamoDrafthouseParser.parse(
      HttpRead.page(http, scheduleUrl(venueSlug)),
      venueSlug,
      cinema,
      notAfter = today.plusDays(MaxHorizonDays.toLong)
    )

  // ── deferred detail: the show's runtime, year, director, cast, synopsis ──

  /** PER VENUE, so the detail lands on this venue's own slot and counts as what
   *  the venue states about its listing — Alamo is the programmer itself, not an
   *  aggregator. The fetch is still once per show: `detailHttp` is chain-wide. */
  override val detailGroup: String = s"alamo-drafthouse|$venueSlug"

  /** The listing's `filmUrl` (`/show/<show slug>`) names the show; the endpoint
   *  answers to that slug. A show it doesn't know is a 404, let escape as gone. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(detailHttp, presentationUrl(showSlugOf(ref))).map(AlamoDrafthouseParser.parseDetail)
}

object AlamoDrafthouseClient {

  val BaseUrl = "https://drafthouse.com"

  /** Alamo's own website API. `v2/schedule/venue/<slug>` is the venue-scoped
   *  view; the sibling `v2/schedule/market/<slug>` returns the same shape for a
   *  whole market and is what `RecordAlamoDrafthouseVenues` reads to build the
   *  venue map. */
  def scheduleUrl(venueSlug: String): String = s"$BaseUrl/s/mother/v2/schedule/venue/$venueSlug"

  /** The chain's market roster — every market with its `cinemas[]`. Read only
   *  when regenerating `docs/venue-maps/ALAMO-DRAFTHOUSE-VENUE-MAP.tsv`, never
   *  at scrape time: the slug↔`Cinema` pairing is wired statically in
   *  `CinemaScraperCatalog`. */
  def marketScheduleUrl(marketSlug: String): String = s"$BaseUrl/s/mother/v2/schedule/market/$marketSlug"

  /** One show's detail — what `drafthouse.com/show/<slug>` displays once its
   *  JavaScript loads (the page itself is a 4.5 KB shell). The site also asks it
   *  market-scoped (`presentation/<market>/<slug>`); the show's facts are the same
   *  unscoped, and one URL per show is what lets every venue share a cached body. */
  def presentationUrl(showSlug: String): String = s"$BaseUrl/s/mother/v2/schedule/presentation/$showSlug"

  /** The show slug off a listing's film page (`https://drafthouse.com/show/<slug>`). */
  def showSlugOf(filmUrl: String): String = filmUrl.trim.stripSuffix("/").split('/').last

  /** How long the chain's shared detail cache keeps a show's body: inside the
   *  `DetailEnrich` refresh window, like every chain's (`CinemaScraperCatalogSpec`). */
  val DetailTtl: FiniteDuration = 2.hours

  /** A show's detail states the film's IMDb id on some shows (6 of 107 across six
   *  venues, 2026-10-06) — the venue's own exact naming of the film, which the
   *  identity's catalogue take maps to TMDB. The id sits in the JSON the show page
   *  reads, not in the page's HTML, so the links are read off [[presentationUrl]]. */
  val CatalogueLinkPages: services.identity.CatalogueLinkPages =
    services.identity.CatalogueLinkPages(CinemaScraper.hostsOf(BaseUrl), Set("imdb"),
      readAt = page => presentationUrl(showSlugOf(page)))

  /** The shared scrape horizon — see [[ScrapeHorizon]]. Here it bounds the
   *  PAYLOAD we keep rather than a request count (the whole programme arrives
   *  in one response either way), so it is purely a guard against a garbage
   *  far-future date, never a coverage window. Deliberately the SAME number the
   *  other chain clients and the Flicks fallback use, so a venue's primary and
   *  its fallback cover one window and neither prunes the other's tail. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays
}
