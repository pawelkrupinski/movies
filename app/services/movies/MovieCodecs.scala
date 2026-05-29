package services.movies

import models.{MovieRecord, Showtime, Source, SourceData}
import org.bson.{BsonReader, BsonWriter}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
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

  private val macroSourceDataCodec: Codec[SourceData] =
    Macros.createCodecProviderIgnoreNone[SourceData]()
      .get(classOf[SourceData], fromRegistries(
        fromCodecs(new LocalDateTimeCodec),
        fromProviders(Macros.createCodecProviderIgnoreNone[Showtime]()),
        DEFAULT_CODEC_REGISTRY
      ))

  private class BackwardCompatibleSourceDataCodec extends Codec[SourceData] {
    override def getEncoderClass: Class[SourceData] = classOf[SourceData]

    override def encode(w: BsonWriter, v: SourceData, c: EncoderContext): Unit =
      macroSourceDataCodec.encode(w, v, c)

    override def decode(r: BsonReader, c: DecoderContext): SourceData = {
      val doc = org.bson.codecs.BsonDocumentCodec().decode(r, c)
      def optStr(key: String): Option[String] =
        if (doc.containsKey(key) && doc.get(key).isString) Some(doc.getString(key).getValue)
        else None
      def optInt(key: String): Option[Int] =
        if (doc.containsKey(key) && doc.get(key).isInt32) Some(doc.getInt32(key).getValue)
        else None
      def seqStr(key: String): Seq[String] =
        if (!doc.containsKey(key) || doc.get(key).isNull) Seq.empty
        else if (doc.get(key).isString) {
          val s = doc.getString(key).getValue
          if (s.isEmpty) Seq.empty else s.split(",").map(_.trim).filter(_.nonEmpty).toSeq
        }
        else if (doc.get(key).isArray) {
          val arr = doc.getArray(key)
          (0 until arr.size()).map(i => arr.get(i).asString().getValue).toSeq
        }
        else Seq.empty
      def showtimes: Seq[Showtime] =
        if (!doc.containsKey("showtimes") || doc.get("showtimes").isNull) Seq.empty
        else {
          val arr = doc.getArray("showtimes")
          val stCodec = macroSourceDataCodec // reuse the registry's Showtime codec
          (0 until arr.size()).map { i =>
            val stDoc = arr.get(i).asDocument()
            val stReader = new org.bson.BsonDocumentReader(stDoc)
            Macros.createCodecProviderIgnoreNone[Showtime]()
              .get(classOf[Showtime], fromRegistries(fromCodecs(new LocalDateTimeCodec), DEFAULT_CODEC_REGISTRY))
              .decode(stReader, c)
          }.toSeq
        }
      SourceData(
        title          = optStr("title"),
        originalTitle  = optStr("originalTitle"),
        synopsis       = optStr("synopsis"),
        cast           = seqStr("cast"),
        director       = seqStr("director"),
        runtimeMinutes = optInt("runtimeMinutes"),
        releaseYear    = optInt("releaseYear"),
        countries      = seqStr("countries"),
        posterUrl      = optStr("posterUrl"),
        filmUrl        = optStr("filmUrl"),
        trailerUrl     = optStr("trailerUrl"),
        showtimes      = showtimes
      )
    }
  }

  private val sourceDataProvider: CodecProvider = new CodecProvider {
    override def get[T](clazz: Class[T], registry: CodecRegistry): Codec[T] =
      if (clazz == classOf[SourceData]) new BackwardCompatibleSourceDataCodec().asInstanceOf[Codec[T]]
      else null
  }

  val registry: CodecRegistry = fromRegistries(
    fromCodecs(new LocalDateTimeCodec),
    fromProviders(
      sourceDataProvider,
      Macros.createCodecProviderIgnoreNone[Showtime](),
      Macros.createCodecProvider[StoredMovieDto]()
    ),
    DEFAULT_CODEC_REGISTRY
  )
}
