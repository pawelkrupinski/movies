package models

case class CinemaMovie(
                        movie: Movie,
                        cinema: Cinema,
                        posterUrl: Option[String],
                        filmUrl: Option[String],
                        synopsis: Option[String],
                        cast: Option[String],
                        director: Option[String],
                        showtimes: Seq[Showtime]
                      )
