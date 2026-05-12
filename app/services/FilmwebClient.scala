package clients

import models._
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDate, ZoneId}
import java.util.concurrent.{Callable, Executors}
import scala.util.Try

object FilmwebClient {

  private val BaseUrl = "https://www.filmweb.pl"
  private val CdnUrl  = "https://fwcdn.pl"

  // Filmweb cinema IDs for Poznań mapped to our Cinema objects
  val cinemaIds: Map[Cinema, Int] = Map(
    Multikino             -> 633,
    CinemaCityKinepolis   -> 624,
    CinemaCityPoznanPlaza -> 568,
    KinoBulgarska         -> 1618,
    CharlieMonroe         -> 1499,
    Helios                -> 1943,
    KinoPalacowe          -> 1854,
    KinoMuza              -> 75,
    Rialto                -> 78
  )

  private val http = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  private def get(url: String): String = {
    val req = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36")
      .header("Accept", "application/json, */*")
      .header("Referer", s"$BaseUrl/showtimes/Pozna%C5%84")
      .GET()
      .build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString())
    if (resp.statusCode() != 200) throw new RuntimeException(s"HTTP ${resp.statusCode()} for $url")
    resp.body()
  }

  private case class Seance(filmId: Long, date: LocalDate, hours: Seq[String])

  private def fetchSeances(cinemaId: Int, date: LocalDate): Seq[Seance] =
    Try {
      Json.parse(get(s"$BaseUrl/api/v1/cinema/$cinemaId/seances?date=$date"))
        .as[JsArray].value.toSeq
        .map { s =>
          Seance(
            filmId = (s \ "film").as[Long],
            date   = date,
            hours  = (s \ "hours").as[String].split(" ").toSeq
          )
        }
    }.getOrElse(Seq.empty)

  private def fetchFilmInfo(filmId: Long): Option[(Long, Movie, Option[String])] =
    Try {
      val json      = Json.parse(get(s"$BaseUrl/api/v1/title/$filmId/info"))
      val title     = (json \ "title").as[String]
      val posterUrl = (json \ "posterPath").asOpt[String]
        .map(p => s"$CdnUrl/ppo${p.replace("$", "2")}")
      val runtime      = (json \ "duration").asOpt[Int]
      val year         = (json \ "year").asOpt[Int]
      val premierePl   = (json \ "premierePoland").asOpt[String].flatMap(d => Try(LocalDate.parse(d)).toOption)
      val premiereW    = (json \ "premiereWorld").asOpt[String].flatMap(d => Try(LocalDate.parse(d)).toOption)
      (filmId, Movie(title, runtime, year, premierePl, premiereW), posterUrl)
    }.toOption

  private def parseHour(h: String): (Int, Int) = {
    val parts = h.split("\\.")
    (parts(0).toInt, if (parts.length > 1) parts(1).toInt else 0)
  }

  // Fetches all dates for one cinema, stopping after 2 consecutive empty days or 60 days.
  private def fetchAllSeancesForCinema(cinemaId: Int): Seq[Seance] = {
    val today  = LocalDate.now(ZoneId.of("Europe/Warsaw"))
    val MaxDays = 120
    val result  = scala.collection.mutable.ArrayBuffer.empty[Seance]
    var date    = today
    var emptyStreak = 0
    var daysChecked = 0
    while (emptyStreak < 2 && daysChecked < MaxDays) {
      val seances = fetchSeances(cinemaId, date)
      if (seances.isEmpty) emptyStreak += 1 else { emptyStreak = 0; result ++= seances }
      date = date.plusDays(1)
      daysChecked += 1
    }
    result.toSeq
  }

  def fetch(): Seq[CinemaMovie] = {
    val pool = Executors.newFixedThreadPool(cinemaIds.size)

    try {
      // Fetch all dates concurrently, one task per cinema
      val seanceFutures = cinemaIds.toSeq.map { case (cinema, cinemaId) =>
        cinema -> pool.submit[Seq[Seance]](() => fetchAllSeancesForCinema(cinemaId))
      }

      val seancesByCinemaAndFilm: Map[(Cinema, Long), Seq[Seance]] =
        seanceFutures.flatMap { case (cinema, future) =>
          future.get().groupBy(_.filmId).toSeq.map { case (filmId, ss) => (cinema, filmId) -> ss }
        }.toMap

      // Fetch film info for all unique film IDs concurrently
      val uniqueFilmIds = seancesByCinemaAndFilm.keys.map(_._2).toSeq.distinct
      val infoFutures   = uniqueFilmIds.map { id =>
        pool.submit[Option[(Long, Movie, Option[String])]](() => fetchFilmInfo(id))
      }
      val filmInfo: Map[Long, (Movie, Option[String])] =
        infoFutures.flatMap(_.get()).map { case (id, movie, poster) => id -> (movie, poster) }.toMap

      // Assemble CinemaMovie objects
      seancesByCinemaAndFilm.toSeq.flatMap { case ((cinema, filmId), seances) =>
        filmInfo.get(filmId).map { case (movie, posterUrl) =>
          val showtimes = seances.flatMap { s =>
            s.hours.map { h =>
              val (hour, min) = parseHour(h)
              Showtime(dateTime = s.date.atTime(hour, min), bookingUrl = None)
            }
          }.sortBy(_.dateTime)
          CinemaMovie(
            movie       = movie,
            cinema      = cinema,
            posterUrl   = posterUrl,
            filmUrl     = Some(s"$BaseUrl/film/$filmId"),
            synopsis    = None,
            cast        = None,
            director    = None,
            showtimes   = showtimes,
            externalIds = Map("filmweb" -> filmId.toString)
          )
        }
      }
    } finally {
      pool.shutdown()
    }
  }
}
