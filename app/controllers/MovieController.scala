package controllers

import models._
import play.api.libs.json.Json
import play.api.mvc._
import services.ShowtimeCache

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime, ZoneId}
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

case class CinemaMovieSchedule(
                                movie: Movie,
                                posterUrl: Option[String],
                                filmUrl: Option[String],
                                showings: Seq[(LocalDate, Seq[Showtime])]
                              )

case class CinemaSchedule(cinema: Cinema, movies: Seq[CinemaMovieSchedule])

@Singleton
class MovieController @Inject()(cc: ControllerComponents, cache: ShowtimeCache)
  extends AbstractController(cc) {

  def index(): Action[AnyContent] = Action { request =>
    val disabled = disabledCinemas(request)
    Ok(views.html.repertoire(toSchedules(cache.get(disabled)), Cinema.all.map(_.displayName)))
  }

  def kina(): Action[AnyContent] = Action { request =>
    val disabled = disabledCinemas(request)
    Ok(views.html.kina(toCinemaSchedules(cache.get(disabled)), Cinema.all.map(_.displayName)))
  }

  def debug(): Action[AnyContent] = Action {
    Ok(views.html.debug(cache.get(Set.empty)))
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
        val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))

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

  private def toCinemaSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[CinemaSchedule] = {
    val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))
    Cinema.all.flatMap { cinema =>
      val movies = cinemaMovies
        .filter(_.cinema == cinema)
        .flatMap { entry =>
          val future = entry.showtimes.filter(_.dateTime.isAfter(now.minusMinutes(30)))
          if (future.isEmpty) None
          else {
            val byDate = future
              .groupBy(_.dateTime.toLocalDate)
              .toSeq.sortBy(_._1)
              .map { case (date, sts) => (date, sts.sortBy(_.dateTime)) }
            Some(CinemaMovieSchedule(
              movie     = Movie(normalizeTitle(entry.movie.title)),
              posterUrl = entry.posterUrl,
              filmUrl   = entry.filmUrl,
              showings  = byDate
            ))
          }
        }
        .sortBy(_.showings.head._1)
      if (movies.isEmpty) None
      else Some(CinemaSchedule(cinema, movies))
    }
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
