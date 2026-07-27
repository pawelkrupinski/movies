package services.cinemas.common

import models.{Cinema, CinemaMovie}
import tools.HttpFetch

/**
 * One client for the whole **Vue Cinemas platform** — the showings backend Vue
 * runs for its UK/IE estate (`myvue.com`) and, byte-for-byte the same shape,
 * for CinemaxX Germany (`cinemaxx.de`). Only the host differs, so the brand is a
 * constructor parameter rather than a second class:
 *
 * {{{
 *   new VueCinemasPlatformClient(http, VueCinemasPlatformClient.MyVueBaseUrl,    "10032", VueCinemasIslington)
 *   new VueCinemasPlatformClient(http, VueCinemasPlatformClient.CinemaxXBaseUrl, "1107",  someCinemaxxVenue)
 * }}}
 *
 * Two endpoints, both under `/api/microservice`:
 *
 *   - `GET {base}/api/microservice/showings/cinemas` — the venue roster
 *     (`cinemaId` + `fullName` + `whatsOnUrl`). Unauthenticated; used offline to
 *     build the id↔`Cinema` map (`VUE-VENUE-MAP.tsv` at the repository root),
 *     NOT at scrape time — the venue id is wired in per client.
 *   - `GET {base}/api/microservice/showings/cinemas/{cinemaId}/films` — every
 *     film with its `showingGroups[].sessions[]`, months out in one call. This
 *     one is **401 without a token cookie**.
 *
 * ==Token bootstrap==
 * `POST {base}/api/microservice/auth/token` (empty JSON body) sets a
 * `microservicesToken` cookie that the films GET must carry. The bootstrap runs
 * OPTIMISTICALLY, the same shape as `services.cinemas.pl.MultikinoClient`'s
 * session warm: try the films GET first, and only on failure POST the token
 * endpoint and retry the GET once. That matters because production wires every
 * venue of a brand onto ONE cookie-jar-bearing `tools.RealHttpFetch` — so the
 * first venue scraped pays the POST and the other ~90 find the cookie already in
 * the jar. A blind POST-then-GET would cost ~91 pointless round-trips per scrape
 * cycle. Either way the bootstrap happens at most once per `fetch()` — never per
 * film — because a single GET returns the venue's whole schedule.
 *
 * `tools.SessionWarmingHttpFetch` is the generic wrapper for exactly this shape,
 * but it warms with a **GET**; the token endpoint 404s on GET and only mints the
 * cookie for a POST (verified live 2026-07-27), so the retry stays in the client.
 *
 * Parsing lives in the pure [[VueCinemasPlatformParser]] — no I/O, so it is
 * unit-tested straight off a recorded fixture.
 */
class VueCinemasPlatformClient(
  http:                HttpFetch,
  baseUrl:             String,
  cinemaId:            String,
  override val cinema: Cinema
) extends CinemaScraper {
  import VueCinemasPlatformClient._

  private val filmsUrl = filmsUrlFor(baseUrl, cinemaId)
  private val tokenUrl = tokenUrlFor(baseUrl)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  /** Vue and CinemaxX are chains fed by their own API — excluded from the
   *  Filmweb per-cinema fallback like Multikino / Cinema City / Helios. */
  override def chain: Boolean = true

  /** The venue's public page is `{base}/cinema/<slug>/whats-on`, and the slug
   *  lives only in the roster endpoint we deliberately don't fetch at scrape
   *  time — an API-only source whose venue id maps to no page we can derive.
   *  That is precisely the `None` case [[CinemaScraper.sourceUrl]] documents;
   *  guessing a slug off the display name would produce dead /uptime links. */
  override def sourceUrl: Option[String] = None

  def fetch(): Seq[CinemaMovie] =
    VueCinemasPlatformParser.parse(filmsJsonWithTokenBootstrap(), cinema, baseUrl)

  /** Optimistic films GET; on any failure mint a token cookie and retry once.
   *  See the token-bootstrap note in the class doc for why this is optimistic
   *  rather than an unconditional POST-then-GET. */
  private def filmsJsonWithTokenBootstrap(): String =
    try http.get(filmsUrl)
    catch { case _: Exception =>
      try http.post(tokenUrl, EmptyTokenRequestBody) catch { case _: Exception => () } // best-effort
      http.get(filmsUrl)
    }
}

object VueCinemasPlatformClient {

  /** Vue UK/IE — 91 venues as of 2026-07-27 (88 GB + 3 Republic of Ireland). */
  val MyVueBaseUrl = "https://www.myvue.com"

  /** CinemaxX Germany — 30 venues on the identical backend. */
  val CinemaxXBaseUrl = "https://www.cinemaxx.de"

  /** The token endpoint wants a POST; the body is irrelevant (the cookie is
   *  minted from the anonymous-session path) but it must be valid JSON. */
  private val EmptyTokenRequestBody = "{}"

  def filmsUrlFor(baseUrl: String, cinemaId: String): String =
    s"${baseUrl.stripSuffix("/")}/api/microservice/showings/cinemas/$cinemaId/films"

  def tokenUrlFor(baseUrl: String): String =
    s"${baseUrl.stripSuffix("/")}/api/microservice/auth/token"

  /** The unauthenticated venue roster. Not fetched by `fetch()` — kept here so
   *  the offline `VUE-VENUE-MAP.tsv` refresh and the client agree on one URL. */
  def cinemasUrlFor(baseUrl: String): String =
    s"${baseUrl.stripSuffix("/")}/api/microservice/showings/cinemas"
}
