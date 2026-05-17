package services.movies

import models.{MovieRecord, Showtime, Source, SourceData}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.{BsonReader, BsonWriter}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros

import java.time.{Instant, LocalDateTime, ZoneOffset}

/**
 * Storage-side mirror of a `movies` document — what mongo-scala-driver's
 * macros derive a codec against. Public domain types are `MovieRecord` (and
 * `StoredMovieRecord` for the read side); this DTO exists only because the
 * domain model uses `Map[Source, SourceData]` and the driver's default codecs
 * only handle `Map[String, V]`. The conversion in `fromDomain` / `toDomain`
 * keys the wire map by `Source.displayName` to match the prior manual encoder
 * exactly; unknown keys on read are dropped silently (legacy cinema slots).
 */
case class StoredMovieDto(
  _id:               String,
  title:             String,
  year:              Option[Int],
  imdbId:            Option[String],
  imdbRating:        Option[Double],
  metascore:         Option[Int],
  filmwebUrl:        Option[String],
  filmwebRating:     Option[Double],
  rottenTomatoes:    Option[Int],
  tmdbId:            Option[Int],
  metacriticUrl:     Option[String],
  rottenTomatoesUrl: Option[String],
  sourceData:        Map[String, SourceData],
  updatedAt:         Instant
)

object StoredMovieDto {
  def fromDomain(id: String, title: String, year: Option[Int], r: MovieRecord, updatedAt: Instant): StoredMovieDto =
    StoredMovieDto(
      _id               = id,
      title             = title,
      year              = year,
      imdbId            = r.imdbId,
      imdbRating        = r.imdbRating,
      metascore         = r.metascore,
      filmwebUrl        = r.filmwebUrl,
      filmwebRating     = r.filmwebRating,
      rottenTomatoes    = r.rottenTomatoes,
      tmdbId            = r.tmdbId,
      metacriticUrl     = r.metacriticUrl,
      rottenTomatoesUrl = r.rottenTomatoesUrl,
      sourceData        = r.data.map { case (s, sd) => s.displayName -> sd },
      updatedAt         = updatedAt
    )

  def toDomain(dto: StoredMovieDto): StoredMovieRecord = StoredMovieRecord(
    title  = dto.title,
    year   = dto.year,
    record = MovieRecord(
      imdbId            = dto.imdbId,
      imdbRating        = dto.imdbRating,
      metascore         = dto.metascore,
      filmwebUrl        = dto.filmwebUrl,
      filmwebRating     = dto.filmwebRating,
      rottenTomatoes    = dto.rottenTomatoes,
      tmdbId            = dto.tmdbId,
      metacriticUrl     = dto.metacriticUrl,
      rottenTomatoesUrl = dto.rottenTomatoesUrl,
      data              = dto.sourceData.flatMap { case (k, sd) => Source.byDisplayName.get(k).map(_ -> sd) }
    )
  )
}

/**
 * BSON codec wiring for the Mongo-backed repo. The macros handle `SourceData`,
 * `Showtime`, and `StoredMovieDto` directly; only `LocalDateTime` needs a
 * hand-written codec (the driver doesn't ship a JSR-310 LocalDateTime codec
 * because LocalDateTime has no zone — we pick UTC, matching the prior
 * `MongoMovieRepo.encodeShowtime` shape so existing rows decode unchanged).
 */
object MovieCodecs {

  private class LocalDateTimeCodec extends Codec[LocalDateTime] {
    override def encode(w: BsonWriter, v: LocalDateTime, c: EncoderContext): Unit =
      w.writeDateTime(v.toInstant(ZoneOffset.UTC).toEpochMilli)
    override def decode(r: BsonReader, c: DecoderContext): LocalDateTime =
      LocalDateTime.ofInstant(Instant.ofEpochMilli(r.readDateTime()), ZoneOffset.UTC)
    override def getEncoderClass: Class[LocalDateTime] = classOf[LocalDateTime]
  }

  val registry: CodecRegistry = fromRegistries(
    fromCodecs(new LocalDateTimeCodec),
    // IgnoreNone omits Optional fields when None instead of writing BsonNull.
    // Matches the prior `SourceData` / `Showtime` encoders, which used
    // `foreach` to skip empty fields rather than persist BsonNull. The
    // top-level `MovieRecord` Optionals (imdbId, …) DID write BsonNull, so
    // `StoredMovieDto` uses the plain provider.
    fromProviders(
      Macros.createCodecProviderIgnoreNone[SourceData](),
      Macros.createCodecProviderIgnoreNone[Showtime](),
      Macros.createCodecProvider[StoredMovieDto]()
    ),
    DEFAULT_CODEC_REGISTRY
  )
}
