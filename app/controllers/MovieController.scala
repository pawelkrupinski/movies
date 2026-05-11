package controllers

import models._
import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.libs.json.Json
import play.api.mvc._
import services.ShowtimeCache

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime}
import javax.inject.{Inject, Singleton}
import scala.util.Try

case class CinemaShowtimes(cinema: Cinema, showtimes: Seq[Showtime])

case class FilmSchedule(
                         movie: Movie,
                         posterUrl: Option[String],
                         synopsis: Option[String],
                         cast: Option[String],
                         director: Option[String],
                         cinemaFilmUrls: Seq[(Cinema, String)],
                         showings: Seq[(LocalDate, Seq[CinemaShowtimes])]
                       )

@Singleton
class MovieController @Inject()(cc: ControllerComponents, cache: ShowtimeCache)
  extends AbstractController(cc) {

  def index(): Action[AnyContent] = Action { _ =>
    Ok(views.html.repertoire(Cinema.all.map(_.displayName)))
  }

  def stream(): Action[AnyContent] = Action { request =>
    val disabled  = disabledCinemas(request)
    val sseSource: Source[ByteString, _] =
      cache.stream(disabled)
        .scan(Seq.empty[CinemaMovie])(_ ++ _)
        .drop(1)
        .map { accumulated =>
          val html = views.html._filmCards(toSchedules(accumulated)).body
          ByteString(s"data: ${Json.toJson(html)}\n\n")
        }
        .concat(Source.single(ByteString("event: done\ndata: {}\n\n")))
    Ok.chunked(sseSource)
      .as("text/event-stream")
      .withHeaders("Cache-Control" -> "no-cache", "X-Accel-Buffering" -> "no")
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    val disabled = disabledCinemas(request)
    toSchedules(cache.get(disabled)).find(_.movie.title == normalizeTitle(title)) match {
      case Some(schedule) => Ok(views.html.film(schedule))
      case None           => NotFound(s"Film not found: $title")
    }
  }

  private def disabledCinemas(request: RequestHeader): Set[String] =
    request.cookies.get("disabledCinemas").flatMap { c =>
      Try(Json.parse(URLDecoder.decode(c.value, StandardCharsets.UTF_8)).as[Seq[String]]).toOption
    }.map(_.toSet).getOrElse(Set.empty)

  private def toSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[FilmSchedule] = {
    cinemaMovies
      .groupBy(cm => normalizeTitle(cm.movie.title).toLowerCase)
      .toSeq
      .flatMap { case (_, entries) =>
        val now = LocalDateTime.now()

        // Prefer non-Bułgarska titles: that cinema uses sentence case which may differ from others.
        // Fall back to Bułgarska only if it is the sole source for this film.
        val nonBulgarska   = entries.filter(_.cinema != KinoBulgarska)
        val titleSource    = if (nonBulgarska.isEmpty) entries else nonBulgarska
        // Normalise to Roman numerals before deduplicating so "II" and "2" collapse to one title.
        val distinctTitles = titleSource.map(e => normalizeTitle(e.movie.title)).toSet
        val displayTitle   = if (distinctTitles.size == 1) distinctTitles.head
                             else normalizeTitle(titleSource.head.movie.title)

        val allShowtimes = entries.flatMap(entry => entry.showtimes.map(st => (entry.cinema, st)))
          .filter(_._2.dateTime.isAfter(now.minusMinutes(30)))
        if (allShowtimes.isEmpty) None
        else {
          val earliest = allShowtimes.map(_._2.dateTime).min

          val byDate: Seq[(LocalDate, Seq[CinemaShowtimes])] =
            allShowtimes
              .groupBy(_._2.dateTime.toLocalDate)
              .toSeq
              .sortBy(_._1)
              .map { case (date, cinemaSlotsOnDate) =>
                val cinemaShowtimes = cinemaSlotsOnDate
                  .groupBy(_._1)
                  .toSeq
                  .sortBy { case (_, slots) => slots.map(_._2.dateTime).min }
                  .map { case (cinema, slots) =>
                    CinemaShowtimes(cinema, slots.map(_._2).sortBy(_.dateTime))
                  }
                (date, cinemaShowtimes)
              }

          // Multikino first so its metadata takes priority; others fill gaps
          val metaPriority = entries.sortBy(e => if (e.cinema == Multikino) 0 else 1)

          Some((earliest, FilmSchedule(
            movie = Movie(displayTitle),
            posterUrl = metaPriority.flatMap(_.posterUrl).headOption,
            synopsis = metaPriority.flatMap(_.synopsis).headOption,
            cast = metaPriority.flatMap(_.cast).headOption,
            director = metaPriority.flatMap(_.director).headOption,
            cinemaFilmUrls = entries.flatMap(entry => entry.filmUrl.map(url => (entry.cinema, url))),
            showings = byDate
          )))
        }
      }
      .sortBy(_._1)
      .map(_._2)
  }

  // Replaces standalone Arabic numerals (1–20) with Roman numerals so that
  // e.g. "Mortal Kombat 2" and "Mortal Kombat II" group as the same film.
  private val ArabicToRoman = Map(
    "1" -> "I", "2" -> "II", "3" -> "III", "4" -> "IV", "5" -> "V",
    "6" -> "VI", "7" -> "VII", "8" -> "VIII", "9" -> "IX", "10" -> "X",
    "11" -> "XI", "12" -> "XII", "13" -> "XIII", "14" -> "XIV", "15" -> "XV",
    "16" -> "XVI", "17" -> "XVII", "18" -> "XVIII", "19" -> "XIX", "20" -> "XX"
  )

  private def normalizeTitle(title: String): String =
    title.split(" ").map(word => ArabicToRoman.getOrElse(word, word)).mkString(" ")
}
