package services.cinemas.common

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json.*
import services.scrapes.ArchivedFilmDto

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
  private implicit val showtimeFormat: Format[Showtime] = models.ShowtimeJson.format
  private implicit val movieFormat: Format[Movie] = Json.format[Movie]

  /** A slice's films as the scrape archive stores them ([[ArchivedFilmDto]], `CinemaMovie` minus `cinema`): ONE mirror
   *  for both stores, so no field the archive keeps is lost in a chunk. A hand-kept copy here once missed `ageRating`,
   *  then `synopsisExcerpt`: each decoded as its default on the chunked half of the corpus only. The showtimes' digest
   *  is the archive's, never written here; a field a slice from the previous deploy lacks decodes as None. */
  private implicit val filmFormat: Format[ArchivedFilmDto] = Json.format[ArchivedFilmDto]

  def encode(movies: Seq[CinemaMovie]): String = Json.stringify(Json.toJson(movies.map(ArchivedFilmDto.of(_))))

  def decode(json: String, cinema: Cinema): Seq[CinemaMovie] = Json.parse(json).as[Seq[ArchivedFilmDto]].map(_.toDomain(cinema))
}
