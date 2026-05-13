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
    Ok(views.html.repertoire(toSchedules(cache.get()), Cinema.all.map(_.displayName)))
  }

  def kina(): Action[AnyContent] = Action { request =>
    Ok(views.html.kina(toCinemaSchedules(cache.get()), Cinema.all.map(_.displayName)))
  }

  def debug(): Action[AnyContent] = Action {
    Ok(views.html.debug(cache.get()))
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    toSchedules(cache.get()).find(_.movie.title == normalizeTitle(title)) match {
      case Some(schedule) => Ok(views.html.film(schedule))
      case None           => NotFound(s"Film not found: $title")
    }
  }

  private def toSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[FilmSchedule] = {
    // Pre-compute canonical→merge-key lookup once (O(N)). Without this, mergeKey
    // scans all titles on every call → O(N²) over the groupBy below.
    val keyFor = TitleNormalizer.mergeKeyLookup(cinemaMovies.map(_.movie.title))
    cinemaMovies
      .groupBy(cm => keyFor(cm.movie.title))
      .toSeq
      .flatMap { case (_, entries) =>
        val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))

        // Prefer non-Bułgarska titles: that cinema uses sentence case which may differ from others.
        // Fall back to Bułgarska only if it is the sole source for this film.
        val nonBulgarska   = entries.filter(_.cinema != KinoBulgarska)
        val titleSource    = if (nonBulgarska.isEmpty) entries else nonBulgarska
        // Normalise to Roman numerals before deduplicating so "II" and "2" collapse to one title.
        // preferredDisplay picks the " i " spelling over " & " when both occur.
        val distinctTitles = titleSource.map(e => normalizeTitle(e.movie.title)).distinct
        val displayTitle   = TitleNormalizer.preferredDisplay(distinctTitles)
                              .getOrElse(normalizeTitle(titleSource.head.movie.title))

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

          val runtimeMinutes: Option[Int] = entries.flatMap(_.movie.runtimeMinutes).headOption
          Some((earliest, FilmSchedule(
            movie = Movie(displayTitle, runtimeMinutes),
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
              movie     = entry.movie.copy(title = normalizeTitle(entry.movie.title)),
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

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)
}
