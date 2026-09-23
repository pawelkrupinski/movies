package services.cinemas.pl

import tools.{HttpFetch, ParallelDetailFetch}
import models._
import play.api.libs.json._
import services.cinemas.common.{AgeRating, CinemaScraper}

import java.time.{LocalDate, LocalDateTime, OffsetDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.util.Try

/**
 * Cinema1 (Gdańsk Morena) — a single-venue booking site built on the
 * "POSitive Cinema" Angular SPA platform (`bilety.cinemaone.pl`). The venue
 * used to run the legacy MSI portal (still modelled as an `MsiVenue` in
 * `CinemaScraperCatalog` until this client replaced it); a 2026 platform
 * migration replaced that server-rendered site with a client-rendered SPA
 * shell that returns no HTML to scrape (confirmed via the 2026-09
 * Filmweb-diff investigation — every scrape came back empty).
 *
 * The SPA fetches its own runtime config from `/assets/config.json`, which
 * exposes the real REST backend (`restapi.cinemaone.pl`) and the venue's own
 * `cinemaId` — so this client talks to that JSON API directly instead of
 * rendering the SPA. Three calls:
 *   - `GET /cinema/{cinemaId}/screening?dateTimeFrom=…&dateTimeTo=…` — every
 *     showtime in the window, grouped by `movieId`. The window runs a year
 *     out from `today`; the API only ever returns what the venue has actually
 *     published (in practice ~3-4 weeks), so this is not a horizon cap (see
 *     feedback_never_limit_scrape_horizon.md) — a real future showtime is
 *     never excluded by the query range.
 *   - `GET /movie/{movieId}` — one call per DISTINCT film in the window
 *     (fetched in parallel), for title/synopsis/cast/director/poster/rating.
 *   - `GET /cinema/{cinemaId}/screenhead` — once, for the screenId → room
 *     name map ("Sala 1"…"Sala 6").
 *
 * The public booking link for a showtime is the SPA's own deep link:
 * `https://bilety.cinemaone.pl/pl/screen?screeningId={id}&cinemaId={id}`.
 */
class Cinema1Client(
  http:     HttpFetch,
  override val cinema: Cinema,
  cinemaId: String,
  today:    LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import Cinema1Client._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ApiUrl, BookingBase)
  override def sourceUrl: Option[String] = Some(s"$BookingBase/pl/cinema?cinemaId=$cinemaId")

  def fetch(): Seq[CinemaMovie] = {
    // Not wrapped in a Try: the listing is the whole scrape, so a failed fetch has
    // to surface as a failed (red) scrape with its cause, not an empty (white) one.
    // The per-film and room lookups below stay tolerant — they only enrich.
    val screenings = parseScreenings(http.get(screeningsUrl(cinemaId, today.atStartOfDay, today.plusYears(1).atStartOfDay)))
    if (screenings.isEmpty) return Seq.empty

    val roomByScreen = Try(http.get(screenHeadUrl(cinemaId))).toOption.map(parseScreenHeads).getOrElse(Map.empty)

    val movieIds  = screenings.map(_.movieId).distinct
    val movieById = ParallelDetailFetch.keyed("cinema1-movies", movieIds, 1.minute)(identity) { id =>
      Try(http.get(movieUrl(id))).toOption.flatMap(parseMovie)
    }

    screenings.groupBy(_.movieId).toSeq.flatMap { case (movieId, slots) =>
      movieById.getOrElse(movieId, None).map { m =>
        CinemaMovie(
          movie     = Movie(m.title, m.runtimeMinutes, m.releaseYear, countries = m.countries, genres = m.genres),
          cinema    = cinema,
          posterUrl = m.posters.headOption,
          filmUrl   = None,
          synopsis  = m.synopsis,
          cast      = m.cast,
          director  = m.director,
          showtimes = slots.map(s => Showtime(s.dateTime, Some(bookingUrl(cinemaId, s.screeningId)), roomByScreen.get(s.screenId), s.format))
            .sortBy(_.dateTime),
          ageRating = m.ageRating
        )
      }
    }
  }
}

object Cinema1Client {

  val BookingBase = "https://bilety.cinemaone.pl"
  val ApiUrl      = "https://restapi.cinemaone.pl/api"

  private val Iso = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  def screeningsUrl(cinemaId: String, from: LocalDateTime, to: LocalDateTime): String =
    s"$ApiUrl/cinema/$cinemaId/screening?dateTimeFrom=${Iso.format(from)}&dateTimeTo=${Iso.format(to)}"
  def screenHeadUrl(cinemaId: String): String = s"$ApiUrl/cinema/$cinemaId/screenhead"
  def movieUrl(movieId: String): String       = s"$ApiUrl/movie/$movieId"
  def bookingUrl(cinemaId: String, screeningId: String): String =
    s"$BookingBase/pl/screen?screeningId=$screeningId&cinemaId=$cinemaId"

  private[cinemas] case class RawScreening(
    screeningId: String, movieId: String, screenId: String, dateTime: LocalDateTime, format: List[String])

  private[cinemas] case class MovieInfo(
    title: String, synopsis: Option[String], cast: Seq[String], director: Seq[String],
    runtimeMinutes: Option[Int], releaseYear: Option[Int], countries: Seq[String],
    genres: Seq[String], posters: Seq[String], ageRating: Option[String])

  /** One screening → a `RawScreening`, or `None` on a malformed entry (missing
   *  id/movieId/screenId, or an unparseable `screeningTimeFrom`) — dropped
   *  individually rather than failing the whole response. */
  private[cinemas] def parseScreenings(raw: String): Seq[RawScreening] = Try {
    Json.parse(raw).as[JsArray].value.toSeq.flatMap { s =>
      for {
        id       <- (s \ "id").asOpt[String]
        movieId  <- (s \ "movieId").asOpt[String]
        screenId <- (s \ "screenId").asOpt[String]
        dt       <- (s \ "screeningTimeFrom").asOpt[String].flatMap(t => Try(OffsetDateTime.parse(t).toLocalDateTime).toOption)
      } yield RawScreening(id, movieId, screenId, dt, formatOf(s))
    }
  }.getOrElse(Seq.empty)

  // `printType` ("2D"/"3D"/…) is almost always the unremarkable "2D" default,
  // so only surface it when it's something else (matches CinemaCityClient's
  // convention of calling out the exception, not the common case).
  // `speakingType` DUB/NAPISY/ORG maps to the same DUB/NAP badge vocabulary
  // every other Polish scraper uses; "ORG" (original version) gets no badge.
  private def formatOf(s: JsValue): List[String] = List(
    (s \ "printType").asOpt[String].filter(_.nonEmpty).filterNot(_ == "2D"),
    (s \ "speakingType").asOpt[String] match {
      case Some("DUB")    => Some("DUB")
      case Some("NAPISY") => Some("NAP")
      case _              => None
    }
  ).flatten

  private[cinemas] def parseScreenHeads(raw: String): Map[String, String] = Try {
    Json.parse(raw).as[JsArray].value.toSeq.flatMap { s =>
      for {
        id   <- (s \ "id").asOpt[String]
        name <- (s \ "name").asOpt[String].filter(_.nonEmpty)
      } yield id -> name
    }.toMap
  }.getOrElse(Map.empty)

  /** One `/movie/{id}` response → a `MovieInfo`, or `None` when it has no
   *  usable title (a malformed/blank response). */
  private[cinemas] def parseMovie(raw: String): Option[MovieInfo] = Try {
    val j     = Json.parse(raw)
    val title = (j \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
    title.map { t =>
      MovieInfo(
        title          = t,
        synopsis       = (j \ "description").asOpt[String].map(_.trim).filter(_.nonEmpty),
        cast           = splitNames((j \ "filmCast").asOpt[String]),
        director       = splitNames((j \ "director").asOpt[String]),
        runtimeMinutes = (j \ "duration").asOpt[Int].filter(_ > 0),
        // `yearOfProduction` is populated only occasionally; the Polish
        // premiere date's year is a reliable fallback when it's blank.
        releaseYear    = (j \ "yearOfProduction").asOpt[String].filter(_.nonEmpty).flatMap(y => Try(y.toInt).toOption)
                           .orElse((j \ "premiereDate").asOpt[String].flatMap(d => Try(OffsetDateTime.parse(d).getYear).toOption)),
        // A co-production ("UK, France, The United States of America") is one
        // comma-joined string with a stray leading space; split into one
        // Movie.countries entry per country, matching every other scraper.
        countries      = splitNames((j \ "country").asOpt[String]),
        genres         = (j \ "genres").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
                           .flatMap(g => (g \ "name").asOpt[String]).filter(_.nonEmpty),
        posters        = (j \ "posters").asOpt[Seq[String]].getOrElse(Seq.empty).filter(_.nonEmpty),
        ageRating      = AgeRating.normalizeDroppingNoRestriction(
                           (j \ "ratings").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
                             .flatMap(r => (r \ "value").asOpt[String]).headOption)
      )
    }
  }.getOrElse(None)

  private def splitNames(raw: Option[String]): Seq[String] =
    raw.getOrElse("").split(",").map(_.trim).filter(_.nonEmpty).toSeq
}
