package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

class CinemaCityClient(http: HttpFetch = new RealHttpFetch()) {

  private val BaseApiUrl = "https://www.cinema-city.pl/pl/data-api-service/v1/quickbook/10103"
  private val FarFuture  = "2027-01-01"

  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] = {
    val datesUrl = s"$BaseApiUrl/dates/in-cinema/$cinemaId/until/$FarFuture?attr=&lang=pl_PL"
    val dates: Seq[LocalDate] = Try {
      (Json.parse(http.get(datesUrl)) \ "body" \ "dates")
        .as[Seq[String]]
        .flatMap(d => Try(LocalDate.parse(d)).toOption)
    }.getOrElse(Seq.empty)

    val pendingEvents = dates.map { date =>
      val url = s"$BaseApiUrl/film-events/in-cinema/$cinemaId/at-date/$date?attr=&lang=pl_PL"
      date -> http.getAsync(url)
    }

    case class FilmInfo(
      name:           String,
      posterLink:     Option[String],
      filmLink:       Option[String],
      runtimeMinutes: Option[Int],
      releaseYear:    Option[Int],
      country:        Option[String]
    )

    val allFilms  = collection.mutable.Map[String, FilmInfo]()
    val allEvents = collection.mutable.ListBuffer[(String, LocalDateTime, Option[String], Option[String], List[String])]()

    for ((_, future) <- pendingEvents) {
      Try {
        val body   = Json.parse(future.join()) \ "body"
        val films  = (body \ "films").as[JsArray].value
        val events = (body \ "events").as[JsArray].value

        for (film <- films) {
          val id = (film \ "id").as[String]
          if (!allFilms.contains(id)) {
            allFilms(id) = FilmInfo(
              name           = (film \ "name").as[String],
              posterLink     = (film \ "posterLink").asOpt[String].filter(_.nonEmpty),
              filmLink       = (film \ "link").asOpt[String].filter(_.nonEmpty),
              runtimeMinutes = (film \ "length").asOpt[Int],
              // releaseYear comes through as a String in the API
              // (`"releaseYear":"2026"`) — asOpt[Int] silently fails on a
              // String and the field stayed None for every film.
              releaseYear    = (film \ "releaseYear").asOpt[String].flatMap(s => Try(s.toInt).toOption)
                .orElse((film \ "releaseYear").asOpt[Int]),
              country        = None   // filled in below from the per-film details page
            )
          }
        }

        for (event <- events) {
          val filmId      = (event \ "filmId").as[String]
          val dateTimeStr = (event \ "eventDateTime").as[String]
          val bookingUrl  = (event \ "bookingLink").asOpt[String].filter(_.nonEmpty)
          val room        = (event \ "auditoriumTinyName").asOpt[String].filter(_.nonEmpty)
                            .map(r => """^S0*(\d+)$""".r.replaceFirstIn(r, "Sala $1"))
          val attrs       = (event \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty).toSet
          // attrs comes in as a kitchen-sink list (genre, 2d/3d, dubbed/subbed/original-lang-*, IMAX,
          // seating type, age rating…). Pick out the screen-feature tokens we want to display.
          val format      = List(
            if (attrs.contains("imax"))  Some("IMAX") else None,
            if (attrs.contains("3d"))    Some("3D")
            else if (attrs.contains("2d")) Some("2D")
            else None,
            if (attrs.contains("dubbed")) Some("DUB")
            else if (attrs.contains("subbed")) Some("NAP")
            else None
          ).flatten
          Try(LocalDateTime.parse(dateTimeStr)).foreach { dateTime =>
            allEvents += ((filmId, dateTime, bookingUrl, room, format))
          }
        }
      }
    }

    // Per-film details page fetch — country isn't in the film-events JSON,
    // but the public film page carries it in a `<p>Produkcja: …</p>` line
    // (countries comma-separated, optionally followed by the release year).
    // Fetched in parallel; a failed fetch or missing line leaves country=None
    // for that film and the rest of the row stays usable.
    val countryByFilmId: Map[String, Option[String]] = {
      val pending = allFilms.toSeq.flatMap { case (id, info) =>
        info.filmLink.map { link => id -> http.getAsync(link) }
      }
      pending.map { case (id, fut) =>
        id -> Try(fut.join()).toOption.flatMap(parseCountry)
      }.toMap
    }
    allFilms.foreach { case (id, info) =>
      countryByFilmId.get(id).flatten.foreach(c => allFilms.update(id, info.copy(country = Some(c))))
    }

    allEvents
      .groupBy(_._1)
      .toSeq
      .flatMap { case (filmId, slots) =>
        allFilms.get(filmId).map { info =>
          CinemaMovie(
            movie       = Movie(info.name.stripPrefix("Ladies Night - "), info.runtimeMinutes, info.releaseYear, country = info.country),
            cinema      = cinema,
            posterUrl   = info.posterLink,
            filmUrl     = info.filmLink,
            synopsis    = None,
            cast        = None,
            director    = None,
            showtimes   = slots.toSeq.map { case (_, dateTime, bookingUrl, room, format) =>
              Showtime(dateTime, bookingUrl, room, format)
            }.sortBy(_.dateTime),
            externalIds = Map("cc" -> filmId)
          )
        }
      }
  }

  // ── Country parser ────────────────────────────────────────────────────────
  //
  // The film page renders a single Polish-language line:
  //   <p>Produkcja: Belgia, Francja, USA 2026</p>
  // …where the country list (comma-separated when co-produced) is followed
  // by an optional release year. Strip the trailing year if it's there;
  // surrounding whitespace gets trimmed.
  private val ProductionLineRe = """<p>Produkcja:\s*([^<]+?)\s*</p>""".r
  private val TrailingYearRe   = """\s+\d{4}\s*$""".r

  private[cinemas] def parseCountry(html: String): Option[String] =
    ProductionLineRe.findFirstMatchIn(html).map { m =>
      TrailingYearRe.replaceFirstIn(m.group(1), "").trim
    }.filter(_.nonEmpty)
}

object CinemaCityClient {
  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] =
    new CinemaCityClient().fetch(cinemaId, cinema)
}
