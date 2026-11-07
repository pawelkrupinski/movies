package controllers

import models.{Cinema, Movie, MovieRecord, ResolvedMovie}
import services.readmodel.TestReadModel

import java.time.LocalDate

/** A film's schedule as a spec builds one: `movie` on the day `asOf`, and only what the spec is about; every other
 *  field empty, the resolved document `record`'s under the movie's own title and year, the slug the title's. */
object FilmSchedules {
  def of(movie: Movie, asOf: LocalDate, posterUrl: Option[String] = None, synopsis: Option[String] = None,
         cast: Seq[String] = Nil, director: Seq[String] = Nil, cinemaFilmUrls: Seq[(Cinema, String)] = Nil,
         showings: Seq[(LocalDate, Seq[CinemaShowtimes])] = Nil, record: MovieRecord = MovieRecord(),
         resolved: Option[ResolvedMovie] = None, slug: Option[String] = None): FilmSchedule =
    FilmSchedule(movie, posterUrl, synopsis, cast, director, cinemaFilmUrls, showings,
      resolved.getOrElse(TestReadModel.resolved(movie.title, movie.releaseYear, record)),
      slug.orElse(FilmHref.slugOf(movie.title)), asOf)
}
