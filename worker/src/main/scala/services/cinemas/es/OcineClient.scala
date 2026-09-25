package services.cinemas.es

import models.{Cinema, CinemaMovie}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, ScrapeHorizon}
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}

/**
 * Ocine (Spain) — each venue off its OWN ticketing server,
 * `tickets.ocine<slug>.es`: a self-hosted "webtpv21" box-office app whose JSON
 * API the venue's public site embeds as its cartelera. Plain HTTP, no auth, no
 * cookie, no captcha on these two reads (the app's altcha challenge guards only
 * purchase recovery), no Cloudflare in front (verified 2026-09-25):
 *
 *   POST /api/v1/sessions   {"token":"","idioma":"es","urlWeb":…}
 *     → `pelicules[]` / `estrenes[]` / `anticipades[]` — the running programme,
 *       announced releases and advance sales, each a film id + clean title. The
 *       `sessions` on a grouped film are EMPTY here: the screenings are only on
 *       the detail endpoint.
 *   GET  /api/v1/pelicula/<id>?lang=es
 *     → the film (Castilian synopsis, cast, director, certificate) and its
 *       `subPelicules[]` variants, each with `sessions[]` (date, time, room,
 *       `planificacio` id) — the venue's WHOLE advertised run of that film, with
 *       no date parameter. See [[OcineParser]] for the grouping.
 *
 * So a scrape is ONE listing call plus ONE call per film, and it is a
 * [[ChunkedCinemaScraper]] for the reason that shape suggests: each film is an
 * independent chunk, so a film whose detail call times out is retried on its
 * own instead of failing the venue. That matters here — every venue runs its
 * own on-premises server, the listing call alone takes 2-9s, and roughly one
 * detail call in a hundred timed out during reconnaissance.
 *
 * No horizon to choose: both calls return everything the venue has on sale,
 * which reached 2026-12-15 when captured, against SensaCine's 7-10 days for
 * the same venues. [[ScrapeHorizon.MaxDays]] only bounds a garbage far date.
 *
 * One instance serves one venue, named by its ticketing host's `<slug>` — see
 * [[OcineVenues]] for the map and which roster venues are (not) on it.
 */
class OcineClient(
  http:        HttpFetch,
  ticketingSlug: String,
  override val cinema: Cinema,
  today:       Option[LocalDate] = None
) extends ChunkedCinemaScraper {

  import OcineClient._

  private val baseUrl      = OcineClient.baseUrl(ticketingSlug)
  private val referenceDay = today.getOrElse(LocalDate.now(Zone))

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  /** A chain fed by its own box-office system, not a single independent venue. */
  override def chain: Boolean = true

  /** The venue's cartelera as the public sees it — the same app the API serves. */
  override def sourceUrl: Option[String] = Some(s"$baseUrl/")

  override def chainVenueId: Option[String] = Some(ticketingSlug)

  /** The venue's film ids, off its cartelera. A fetch failure propagates (the
   *  scrape is recorded as failed and the venue keeps its last-known listing);
   *  a 200 that is not a cartelera throws too, rather than reading an error
   *  page as an empty venue. An empty cartelera is a legitimately empty venue. */
  def planChunks(): Seq[String] =
    OcineParser.filmIds(http.post(s"$baseUrl/api/v1/sessions", listingBody(baseUrl)))
      .getOrElse(throw new IllegalStateException(s"$baseUrl/api/v1/sessions answered without a cartelera"))

  /** One film's detail → its row. A throw reschedules only this film's chunk. */
  def fetchChunk(filmId: String): Seq[CinemaMovie] =
    OcineParser.film(
      http.get(s"$baseUrl/api/v1/pelicula/$filmId?lang=es"),
      baseUrl, cinema, notAfter = referenceDay.plusDays(ScrapeHorizon.MaxDays.toLong)
    ).toSeq
}

object OcineClient {

  def baseUrl(ticketingSlug: String): String = s"https://tickets.ocine$ticketingSlug.es"

  /** Every Ocine venue is on the peninsula or the Balearics — one zone. Used only
   *  to resolve "today" for the far-date bound when none is injected. */
  val Zone: ZoneId = ZoneId.of("Europe/Madrid")

  /** The body the site's own cartelera posts. An empty `token` asks the server
   *  for a fresh anonymous one (returned alongside the listing, unused by us);
   *  `urlWeb` is fixed rather than the caller's URL so the request — and with it
   *  the recorded fixture's key — is the same on every scrape. */
  def listingBody(baseUrl: String): String =
    s"""{"token":"","idioma":"es","urlWeb":"$baseUrl/"}"""
}
