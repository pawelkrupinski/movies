package clients

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
      releaseYear:    Option[Int]
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
              releaseYear    = (film \ "releaseYear").asOpt[Int]
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
          val format      = List(
            if (attrs.contains("imax")) Some("IMAX") else None,
            if (attrs.contains("3d")) Some("3D") else if (attrs.contains("2d")) Some("2D") else None
          ).flatten
          Try(LocalDateTime.parse(dateTimeStr)).foreach { dateTime =>
            allEvents += ((filmId, dateTime, bookingUrl, room, format))
          }
        }
      }
    }

    allEvents
      .groupBy(_._1)
      .toSeq
      .flatMap { case (filmId, slots) =>
        allFilms.get(filmId).map { info =>
          CinemaMovie(
            movie       = Movie(info.name.stripPrefix("Ladies Night - "), info.runtimeMinutes, info.releaseYear),
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
}

object CinemaCityClient {
  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] =
    new CinemaCityClient().fetch(cinemaId, cinema)
}
