package controllers

import clients.{CharlieMonroeClient, KinoPalacoweClient, MultikinoClient}
import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}

import java.time.LocalDate
import play.api.mvc._

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future, blocking}

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
class MovieController @Inject() (cc: ControllerComponents)(implicit ec: ExecutionContext)
    extends AbstractController(cc) {

  def index(): Action[AnyContent] = Action.async { implicit request =>
    Future {
      blocking {
        toSchedules(MultikinoClient.fetch() ++ CharlieMonroeClient.fetch() ++ KinoPalacoweClient.fetch())
      }
    }.map(films => Ok(views.html.repertoire(films)))
  }

  def film(title: String): Action[AnyContent] = Action.async { implicit request =>
    Future {
      blocking {
        toSchedules(MultikinoClient.fetch() ++ CharlieMonroeClient.fetch() ++ KinoPalacoweClient.fetch())
      }
    }.map { films =>
      films.find(_.movie.title == title) match {
        case Some(schedule) => Ok(views.html.film(schedule))
        case None           => NotFound(s"Film not found: $title")
      }
    }
  }

  private def toSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[FilmSchedule] = {
    cinemaMovies
      .groupBy(_.movie.title)
      .toSeq
      .map { case (_, entries) =>
        val allShowtimes = entries.flatMap(entry => entry.showtimes.map(st => (entry.cinema, st)))
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
                .sortBy(_._1.displayName)
                .map { case (cinema, slots) =>
                  CinemaShowtimes(cinema, slots.map(_._2).sortBy(_.dateTime))
                }
              (date, cinemaShowtimes)
            }

        // Multikino first so its metadata takes priority; others fill gaps
        val metaPriority = entries.sortBy(e => if (e.cinema == Multikino) 0 else 1)

        (earliest, FilmSchedule(
          movie          = entries.head.movie,
          posterUrl      = metaPriority.flatMap(_.posterUrl).headOption,
          synopsis       = metaPriority.flatMap(_.synopsis).headOption,
          cast           = metaPriority.flatMap(_.cast).headOption,
          director       = metaPriority.flatMap(_.director).headOption,
          cinemaFilmUrls = entries.flatMap(entry => entry.filmUrl.map(url => (entry.cinema, url))),
          showings       = byDate
        ))
      }
      .sortBy(_._1)
      .map(_._2)
  }
}
