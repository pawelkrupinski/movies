package services.cinemas.uk

import models.{Cinema, CinemaMovie, CineworldChain, Source}
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, GatsbyBoxOfficeClient}
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}

/**
 * Cineworld — the UK's second-largest chain (87 sites). Cineworld relaunched
 * its entire website 2026-09-17, retiring the `quickbook` data API this class
 * used to speak (chain code 10108, the same Regal-built API family Cinema City
 * Poland and Regal US still run — the unwired [[services.cinemas.us.RegalClient]]
 * speaks the latter — both unaffected; only Cineworld UK's own deployment moved). The whole
 * `/uk/` path prefix now 404s.
 *
 * The new site turns out to run the IDENTICAL Webedia "box office" Gatsby
 * platform already serving Showcase/Everyman/Landmark — same static-query
 * hash for the chain-wide film catalogue, same
 * `/api/gatsby-source-boxofficeapi/schedule` shape, verified live 2026-09-18
 * (`curl` against both, no cookies, identical response shape). So the listing
 * scrape is a plain [[GatsbyBoxOfficeClient]] this class COMPOSES rather than
 * re-implements — see that class's doc for the two-request shape (chain
 * catalogue + one schedule call spanning the whole horizon).
 *
 * What's different from Showcase/Everyman, and why Cineworld still needs its
 * own class: the listing/catalogue query never carries synopsis, cast,
 * director or certificate for ANY brand on this platform (every node comes
 * back null — confirmed on Cineworld's own 157-film catalogue same as the UK
 * Webedia siblings). Showcase/Everyman simply don't have that data anywhere
 * and lean on TMDB for it. Cineworld's site DOES still expose it, just off a
 * separate runtime endpoint the static catalogue query doesn't reach —
 * `/api/gatsby-source-boxofficeapi/movies?ids=<id>` — so this class keeps the
 * [[DetailEnricher]] wiring its old client had (one chain-wide fetch per film,
 * deduped across all 87 venues) pointed at that endpoint instead of the
 * retired film page's `<noscript>` block. That block is GONE on the
 * relaunched site — verified 2026-09-18, no `<noscript>` element at all, the
 * page is client-rendered React with no server-side fallback content — so the
 * old jsoup-based [[CineworldParser]] detail scrape could not have carried
 * forward even in spirit; the movies endpoint is a wholesale replacement, not
 * a patch.
 *
 * `docs/venue-maps/CINEWORLD-VENUE-MAP.tsv` maps every venue's new slug (its
 * platform id is simply the slug's leading segment, UPPERCASED —
 * `g01hn-cineworld-cinema-barnsley` → `G01HN` — verified across all 87 via the
 * site's own `/cinemas/` roster, which gives both in one response) to its
 * [[Cinema]] case object.
 */
class CineworldClient(
  http: HttpFetch,
  // The venue's slug from `/cinemas/` (e.g. "g01hn-cineworld-cinema-barnsley").
  // The platform id the schedule/catalogue calls need is derived from it
  // (see `theaterId` below) rather than carried as a second constructor
  // parameter, so the venue map has one column to drift, not two.
  slug: String,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(ZoneId.of(GatsbyBoxOfficeClient.UkTimeZone))
) extends CinemaScraper with DetailEnricher {

  import CineworldClient._

  /** The platform's own venue id — the slug's leading dash-delimited segment,
   *  uppercased. Verified against all 87 venues in the site's `/cinemas/`
   *  roster 2026-09-18: `relatedEntity.id` matched this derivation on every
   *  one, so there is no case this can silently get wrong for a venue the
   *  roster already agrees with. */
  private val theaterId: String = slug.takeWhile(_ != '-').toUpperCase

  /** The listing scrape, unchanged from how Showcase/Everyman already do it —
   *  see class doc. Composed rather than duplicated: this class adds only what
   *  is unique to Cineworld (the detail enrichment below). */
  private val listing = new GatsbyBoxOfficeClient(
    http, BaseUrl, theaterId, cinema, venuePath = Some(s"/cinemas/$slug/"), today = today)

  def scrapeHosts: Set[String] = listing.scrapeHosts
  override def sourceUrl: Option[String] = listing.sourceUrl

  /** A national chain fed by one central platform, so the Filmweb per-cinema
   *  fallback shouldn't shadow it (see `FallbackEligibility`) — unchanged from
   *  the old client. */
  override def chain: Boolean = true

  def fetch(): Seq[CinemaMovie] = listing.fetch()

  // ── deferred detail: synopsis/cast/director/certificate off the runtime
  //    `movies?ids=` endpoint — see class doc for why this chain still needs
  //    its own DetailEnricher wiring despite composing the shared listing. ──

  /** Chain-wide, exactly as before: one fetch per film per freshness window
   *  serves all 87 venues instead of 87 copies. */
  override val detailGroup: String = "cineworld"
  override def detailTarget: Source = CineworldChain
  override def enrichmentServiceOverride: Option[String] = Some("Cineworld Enrichment")
  /** The listing already carries a title (off the catalogue), so a row
   *  resolves against TMDB immediately; the detail fetch's synopsis/cast/
   *  director/certificate merge in asynchronously, same reasoning as before. */
  override def defersTmdbResolution: Boolean = false

  /** Fetch + parse one film's detail by the `filmUrl` the listing scrape left
   *  on the movie (`.../films/<id>-<slug>`) — `movieIdOf` reads the id off it.
   *  `None` only on fetch failure or an unparseable body. An id the platform
   *  doesn't recognise answers 200 with an empty array — a LOADED, well-formed
   *  response — so `CineworldParser.parseMovieDetail` returns `Some(FilmDetail())`
   *  for it: the film is stamped `Fetched` (an empty `FilmDetail` merges as a
   *  no-op) instead of retried every tick forever. There is no durable-vs-
   *  transient HTTP signal on this endpoint the way the old detail-page fetch
   *  had, so a durable HTTP status (404/410 on the endpoint itself, not an
   *  empty body) still escapes via `transientToNone` exactly as every other
   *  deferred-detail client does — that is the one case this client still
   *  reports `Failed`/retries. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(movieDetailUrl(BaseUrl, movieIdOf(ref))))
      .flatMap(CineworldParser.parseMovieDetail)
}

object CineworldClient {

  val BaseUrl = "https://www.cineworld.co.uk"

  /** Cast members the `movies` endpoint returns per film. The old detail
   *  page's `<noscript>` block carried the whole billed cast with no cap; the
   *  new endpoint makes us name a limit, and 10 is a generous stand-in. */
  private val CastingLimit = 10

  def movieDetailUrl(baseUrl: String, movieId: String): String =
    s"$baseUrl/api/gatsby-source-boxofficeapi/movies?basic=false&castingLimit=$CastingLimit&ids=$movieId"

  /** The film id off a listing's `filmUrl` (`.../films/<id>-<title-slug>`).
   *  Lenient on anything not matching that exact shape — takes the last path
   *  segment and its leading dash-delimited token regardless — so a ref this
   *  client didn't itself produce (a test double, a future URL shape change)
   *  still yields SOME id and the fetch is attempted, rather than every
   *  unexpected ref silently short-circuiting to `None` before ever reaching
   *  the network. A real HTTP failure then surfaces through `fetchFilmDetail`
   *  normally instead of being masked as "nothing to fetch". */
  def movieIdOf(filmUrl: String): String =
    filmUrl.split("/").lastOption.getOrElse(filmUrl).split("-").headOption.getOrElse(filmUrl)
}
