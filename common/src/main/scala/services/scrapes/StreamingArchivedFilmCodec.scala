package services.scrapes

import models.Movie
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonType, BsonWriter}
import services.movies.{BsonReads, ShowtimeCodec}

/**
 * One film of an archived listing, written and read field by field — so its booking URLs can be
 * stored split at the prefix they share (the film's `bookingUrlPrefix`, each showtime's
 * `bookingUrlRest`), which the macro codec has no way to express. Everything else reads and
 * writes as the macro codec did (`IgnoreNone`: an absent optional is omitted, and reads back
 * `None`; `ScrapeArchiveCodecsSpec` pins every stored shape to it). `movie` goes through its
 * macro codec.
 *
 * A US archive is ~5,000 venues and 306 MB, more than half of it booking URLs (2026-10-03),
 * and the identity projection reads all of it every pass.
 */
private[scrapes] final class StreamingArchivedFilmCodec(movies: Codec[Movie]) extends Codec[ArchivedFilmDto] {
  override def getEncoderClass: Class[ArchivedFilmDto] = classOf[ArchivedFilmDto]

  override def encode(w: BsonWriter, v: ArchivedFilmDto, c: EncoderContext): Unit = {
    w.writeStartDocument()
    w.writeName("movie")
    movies.encode(w, v.movie, c)
    v.posterUrl.foreach(w.writeString("posterUrl", _))
    v.filmUrl.foreach(w.writeString("filmUrl", _))
    v.synopsis.foreach(w.writeString("synopsis", _))
    strings(w, "cast", v.cast)
    strings(w, "director", v.director)
    w.writeStartArray("showtimes")
    v.showtimes.foreach(ShowtimeCodec.write(w, _, c, null))
    w.writeEndArray()
    w.writeStartDocument("externalIds")
    v.externalIds.foreach { case (k, id) => w.writeString(k, id) }
    w.writeEndDocument()
    v.trailerUrl.foreach(w.writeString("trailerUrl", _))
    v.ageRating.foreach(w.writeString("ageRating", _))
    w.writeEndDocument()
  }

  private def strings(w: BsonWriter, name: String, values: Seq[String]): Unit = {
    w.writeStartArray(name)
    values.foreach(w.writeString)
    w.writeEndArray()
  }

  override def decode(r: BsonReader, c: DecoderContext): ArchivedFilmDto = {
    var movie: Movie                      = null
    var posterUrl, filmUrl, synopsis      = Option.empty[String]
    var trailerUrl, ageRating             = Option.empty[String]
    var cast, director: Seq[String]       = null
    var externalIds: Map[String, String]  = null
    var urlPrefix: String                 = null
    var showtimes: Seq[models.Showtime]   = null
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "movie"       => movie = movies.decode(r, c)
        case "posterUrl"   => posterUrl = BsonReads.optionalString(r)
        case "filmUrl"     => filmUrl = BsonReads.optionalString(r)
        case "synopsis"    => synopsis = BsonReads.optionalString(r)
        case "cast"        => cast = BsonReads.strings(r)
        case "director"    => director = BsonReads.strings(r)
        case ShowtimeCodec.RowPrefixField => urlPrefix = BsonReads.optionalString(r).orNull
        case "showtimes"   =>
          val read = Vector.newBuilder[models.Showtime]
          r.readStartArray()
          while (r.readBsonType() != BsonType.END_OF_DOCUMENT) read += ShowtimeCodec.read(r, c, urlPrefix)
          r.readEndArray()
          showtimes = read.result()
        case "externalIds" =>
          val ids = Map.newBuilder[String, String]
          r.readStartDocument()
          while (r.readBsonType() != BsonType.END_OF_DOCUMENT) ids += r.readName() -> r.readString()
          r.readEndDocument()
          externalIds = ids.result()
        case "trailerUrl"  => trailerUrl = BsonReads.optionalString(r)
        case "ageRating"   => ageRating = BsonReads.optionalString(r)
        case _             => r.skipValue()
      }
    }
    r.readEndDocument()
    if (movie == null || cast == null || director == null || showtimes == null || externalIds == null)
      throw new org.bson.codecs.configuration.CodecConfigurationException("ArchivedFilmDto: a required field is missing")
    ArchivedFilmDto(movie, posterUrl, filmUrl, synopsis, cast, director,
      ShowtimeCodec.completed(showtimes, urlPrefix), externalIds, trailerUrl, ageRating)
  }
}
