package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

/**
 * Serialises a chunk's slice of a listing (a `Seq[CinemaMovie]`) to/from JSON
 * for the chunk store. The `cinema` field is deliberately NOT serialised — the
 * reduce step already knows which cinema it is and re-attaches it on decode — so
 * no `Cinema` registry lookup is needed and the stored value stays small.
 */
object CinemaMovieJson {
  private val Iso = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  private implicit val ldtFormat: Format[LocalDateTime] =
    Format(Reads.localDateTimeReads(Iso), Writes(d => JsString(d.format(Iso))))
  private implicit val showtimeFormat: Format[Showtime] = Json.format[Showtime]
  private implicit val movieFormat: Format[Movie] = Json.format[Movie]

  /** A transport mirror of `CinemaMovie` minus `cinema`. */
  private case class Transport(
    movie: Movie, posterUrl: Option[String], filmUrl: Option[String], synopsis: Option[String],
    cast: Seq[String], director: Seq[String], showtimes: Seq[Showtime],
    externalIds: Map[String, String], trailerUrl: Option[String])
  private implicit val transportFormat: Format[Transport] = Json.format[Transport]

  def encode(movies: Seq[CinemaMovie]): String =
    Json.stringify(Json.toJson(movies.map(m =>
      Transport(m.movie, m.posterUrl, m.filmUrl, m.synopsis, m.cast, m.director, m.showtimes, m.externalIds, m.trailerUrl))))

  def decode(json: String, cinema: Cinema): Seq[CinemaMovie] =
    Json.parse(json).as[Seq[Transport]].map(t =>
      CinemaMovie(t.movie, cinema, t.posterUrl, t.filmUrl, t.synopsis, t.cast, t.director, t.showtimes, t.externalIds, t.trailerUrl))
}
