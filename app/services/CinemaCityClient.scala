package clients

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDate
import java.time.LocalDateTime
import scala.util.Try

object CinemaCityClient {

  private val BaseApiUrl = "https://www.cinema-city.pl/pl/data-api-service/v1/quickbook/10103"
  private val FarFuture  = "2027-01-01"

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  private def buildRequest(url: String): HttpRequest =
    HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "application/json")
      .GET()
      .build()

  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] = {
    val datesUrl  = s"$BaseApiUrl/dates/in-cinema/$cinemaId/until/$FarFuture?attr=&lang=pl_PL"
    val datesResp = httpClient.send(buildRequest(datesUrl), HttpResponse.BodyHandlers.ofString())
    if (datesResp.statusCode() != 200)
      throw new RuntimeException(s"cinema-city.pl dates returned ${datesResp.statusCode()}")

    val dates: Seq[LocalDate] = Try {
      (Json.parse(datesResp.body()) \ "body" \ "dates").as[Seq[String]].flatMap(d => Try(LocalDate.parse(d)).toOption)
    }.getOrElse(Seq.empty)

    val pendingEvents = dates.map { date =>
      val url = s"$BaseApiUrl/film-events/in-cinema/$cinemaId/at-date/$date?attr=&lang=pl_PL"
      date -> httpClient.sendAsync(buildRequest(url), HttpResponse.BodyHandlers.ofString())
    }

    case class FilmInfo(name: String, posterLink: Option[String], filmLink: Option[String])

    val allFilms  = collection.mutable.Map[String, FilmInfo]()
    val allEvents = collection.mutable.ListBuffer[(String, LocalDateTime, Option[String])]()

    for ((_, future) <- pendingEvents) {
      Try {
        val body = Json.parse(future.join().body()) \ "body"
        val films  = (body \ "films").as[JsArray].value
        val events = (body \ "events").as[JsArray].value

        for (film <- films) {
          val id = (film \ "id").as[String]
          if (!allFilms.contains(id)) {
            allFilms(id) = FilmInfo(
              name       = (film \ "name").as[String],
              posterLink = (film \ "posterLink").asOpt[String].filter(_.nonEmpty),
              filmLink   = (film \ "link").asOpt[String].filter(_.nonEmpty)
            )
          }
        }

        for (event <- events) {
          val filmId      = (event \ "filmId").as[String]
          val dateTimeStr = (event \ "eventDateTime").as[String]
          val bookingUrl  = (event \ "bookingLink").asOpt[String].filter(_.nonEmpty)
          Try(LocalDateTime.parse(dateTimeStr)).foreach { dateTime =>
            allEvents += ((filmId, dateTime, bookingUrl))
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
            movie     = Movie(info.name),
            cinema    = cinema,
            posterUrl = info.posterLink,
            filmUrl   = info.filmLink,
            synopsis  = None,
            cast      = None,
            director  = None,
            showtimes = slots.toSeq.map { case (_, dateTime, bookingUrl) => Showtime(dateTime, bookingUrl) }.sortBy(_.dateTime)
          )
        }
      }
  }
}
