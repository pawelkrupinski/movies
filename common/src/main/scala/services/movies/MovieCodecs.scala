package services.movies

import models.{Showtime, SourceData, TitleSearch}
import org.bson.{BsonReader, BsonType, BsonWriter}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import services.PersistedCodecs

import java.time.Instant

private[movies] final class BackwardCompatibleSourceDataCodec(
    macroCodec: Codec[SourceData], showtimeCodec: Codec[Showtime], titleSearchCodec: Codec[TitleSearch]) extends Codec[SourceData] {
  override def getEncoderClass: Class[SourceData] = classOf[SourceData]

  // The cache-only fields (`showtimesDigest`, `showtimeStartMinutes`) are dropped on
  // the way out (`SourceData.persisted`): `decode` never reads them back, and a
  // cache-stripped record is written through here routinely. `showtimeStartMinutes` is an
  // `IArray[Int]` with no BSON codec at all, so letting it through failed every such
  // `upsert`/`replaceFilm`.
  override def encode(w: BsonWriter, v: SourceData, c: EncoderContext): Unit =
    macroCodec.encode(w, v.persisted, c)

  // Streamed field by field off the reader — a slot is decoded ~100k times in a US worker's
  // boot hydrate, and building each one's whole BsonDocument tree first, then reading it back,
  // was the costliest part of the decode on the restart's critical path. Every legacy rule the
  // tree-based reader applied holds (`MovieCodecsSpec` pins each legacy shape): a text field
  // only when it is a string, a number only when it is an int32, a list from an array or from
  // one comma-joined string, null and absent alike empty, every other field skipped.
  override def decode(r: BsonReader, c: DecoderContext): SourceData = {
    import BackwardCompatibleSourceDataCodec.{LegacyFieldReads, SourceDataFields}
    val fields = new SourceDataFields
    val in     = LegacyFieldReads(r, c, showtimeCodec, titleSearchCodec)
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      SourceDataFields.Readers.get(r.readName()) match {
        case Some(read) => read(in, fields)
        case None       => r.skipValue()
      }
    }
    r.readEndDocument()
    fields.result
  }
}

private object BackwardCompatibleSourceDataCodec {
  /** One legacy `SourceData` document's reader, with the rules [[BackwardCompatibleSourceDataCodec]] reads every field
   *  by: a text field only when it is a string, a number only when it is an int32, a list from an array or from one
   *  comma-joined string, null and absent alike empty. */
  final case class LegacyFieldReads(r: BsonReader, c: DecoderContext,
                                            showtimeCodec: Codec[Showtime], titleSearchCodec: Codec[TitleSearch]) {
    def str(): Option[String] = if (r.getCurrentBsonType == BsonType.STRING) Some(r.readString()) else { r.skipValue(); None }
    def int(): Option[Int]    = if (r.getCurrentBsonType == BsonType.INT32) Some(r.readInt32()) else { r.skipValue(); None }
    def strings(): Seq[String] = r.getCurrentBsonType match {
      case BsonType.STRING =>
        val joined = r.readString()
        if (joined.isEmpty) Seq.empty else joined.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      case BsonType.ARRAY  => elements(r.readString())
      case _               => r.skipValue(); Seq.empty
    }
    def documents[A](codec: Codec[A]): Seq[A] = r.getCurrentBsonType match {
      case BsonType.ARRAY => elements(codec.decode(r, c))
      case _              => r.skipValue(); Seq.empty
    }
    private def elements[A](one: => A): Seq[A] = {
      r.readStartArray()
      val out = Seq.newBuilder[A]
      while (r.readBsonType() != BsonType.END_OF_DOCUMENT) out += one
      r.readEndArray()
      out.result()
    }
  }

  /** The fields of one `SourceData` as [[BackwardCompatibleSourceDataCodec]] reads them, each empty until read. */
  final class SourceDataFields {
    var title, rawTitle, originalTitle, englishTitle, synopsis, posterUrl, filmUrl, trailerUrl, language, ageRating =
      Option.empty[String]
    var runtimeMinutes, releaseYear = Option.empty[Int]
    var cast, director, countries, genres = Seq.empty[String]
    var showtimes     = Seq.empty[Showtime]
    var titleSearches = Seq.empty[TitleSearch]

    def result: SourceData = SourceData(
      title = title, rawTitle = rawTitle, originalTitle = originalTitle, englishTitle = englishTitle,
      synopsis = synopsis, cast = cast, director = director, runtimeMinutes = runtimeMinutes,
      releaseYear = releaseYear, countries = countries, genres = genres, posterUrl = posterUrl,
      filmUrl = filmUrl, trailerUrl = trailerUrl, language = language, showtimes = showtimes,
      ageRating = ageRating, titleSearches = titleSearches)
  }

  object SourceDataFields {
    /** Each stored field's reader; a field not named here is skipped. */
    val Readers: Map[String, (LegacyFieldReads, SourceDataFields) => Unit] = Map(
      "title"          -> ((in, f) => f.title = in.str()),
      "rawTitle"       -> ((in, f) => f.rawTitle = in.str()),
      "originalTitle"  -> ((in, f) => f.originalTitle = in.str()),
      "englishTitle"   -> ((in, f) => f.englishTitle = in.str()),
      "synopsis"       -> ((in, f) => f.synopsis = in.str()),
      "cast"           -> ((in, f) => f.cast = in.strings()),
      "director"       -> ((in, f) => f.director = in.strings()),
      "runtimeMinutes" -> ((in, f) => f.runtimeMinutes = in.int()),
      "releaseYear"    -> ((in, f) => f.releaseYear = in.int()),
      "countries"      -> ((in, f) => f.countries = in.strings()),
      "genres"         -> ((in, f) => f.genres = in.strings()),
      "posterUrl"      -> ((in, f) => f.posterUrl = in.str()),
      "filmUrl"        -> ((in, f) => f.filmUrl = in.str()),
      "trailerUrl"     -> ((in, f) => f.trailerUrl = in.str()),
      // Absent on every row written before the stamp existed — `None` reads as pl-PL (the
      // historical hardcoded enrichment language), which is what those rows actually hold.
      "language"       -> ((in, f) => f.language = in.str()),
      "showtimes"      -> ((in, f) => f.showtimes = in.documents(in.showtimeCodec)),
      // Absent on every row written before the certificate field existed → None.
      "ageRating"      -> ((in, f) => f.ageRating = in.str()),
      // Absent on every slot written before the field existed → no search evidence.
      "titleSearches"  -> ((in, f) => f.titleSearches = in.documents(in.titleSearchCodec)))
  }
}

/** A showtime, written and read by hand: `Showtime` is not a case class (see it), so no macro
 *  derives its codec. Writes the shape the macro codec wrote with `IgnoreNone` — `dateTime`, then
 *  `bookingUrl` and `room` when set, then `format`, always — and reads as it read (`ShowtimeDecodeSpec`
 *  pins every stored shape): an absent or null optional field is `None`, a missing `room`/`format`
 *  takes its default, an unknown field is skipped, a missing `dateTime` fails.
 *
 *  Field by field, too, for speed: a US corpus pass, and a US boot's cache hydrate, each decode
 *  ~1.7M of them, and the macro codec's per-document machinery — a map of the fields read, the
 *  lookups and boxing to build one — was most of the census's CPU and a large share of its
 *  allocation (JFR over the US mirror, 2026-09-30). Stateless. */
private[services] object ShowtimeCodec extends Codec[Showtime] {
  override def getEncoderClass: Class[Showtime] = classOf[Showtime]
  override def encode(w: BsonWriter, v: Showtime, c: EncoderContext): Unit = write(w, v, c, null)

  /** `v` as a row whose `bookingUrlPrefix` is `prefix` stores it: the URL after the prefix as
   *  `bookingUrlRest` — or, with no prefix or a URL that does not start with it, whole. */
  def write(w: BsonWriter, v: Showtime, c: EncoderContext, prefix: String): Unit = {
    w.writeStartDocument()
    w.writeName("dateTime")
    JavaTimeCodecs.localDateTime.encode(w, v.dateTime, c)
    v.bookingUrl.foreach { url =>
      if (prefix != null && url.startsWith(prefix)) w.writeString("bookingUrlRest", url.substring(prefix.length))
      else w.writeString("bookingUrl", url)
    }
    v.room.foreach(w.writeString("room", _))
    w.writeStartArray("format")
    v.format.foreach(w.writeString)
    w.writeEndArray()
    w.writeEndDocument()
  }

  /** A row's showtimes, split at the booking-URL prefix they share: that prefix once, as the
   *  row's `bookingUrlPrefix`, then the `showtimes` array, each holding only the rest of its URL.
   *  A row whose URLs share no prefix — fewer than two, or different sites — is written whole.
   *  The prefix comes from the row's own URLs on every write, so a cinema that moves its booking
   *  pages is written at its new prefix; nothing outside the row is needed to read it back. */
  def writeShowtimes(w: BsonWriter, showtimes: Seq[Showtime], c: EncoderContext): Unit = {
    val common = Showtime.commonUrlPrefix(showtimes)
    val prefix = if (common.isEmpty) null else common
    if (prefix != null) w.writeString(RowPrefixField, prefix)
    w.writeStartArray("showtimes")
    showtimes.foreach(write(w, _, c, prefix))
    w.writeEndArray()
  }

  override def decode(r: BsonReader, c: DecoderContext): Showtime = read(r, c, null)

  /** A showtime of a row whose `bookingUrlPrefix` is `prefix` — null when the row stores none, or
   *  stores it after its showtimes, which leaves a stored remainder [[Showtime.awaitsRowPrefix]]
   *  until [[completed]] puts the prefix in front of it. */
  def read(r: BsonReader, c: DecoderContext, prefix: String): Showtime = {
    var dateTime: java.time.LocalDateTime = null
    var bookingUrl, room = Option.empty[String]
    var rest: String     = null
    var format           = List.empty[String]
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "dateTime"       => dateTime = JavaTimeCodecs.localDateTime.decode(r, c)
        case "bookingUrl"     => bookingUrl = BsonReads.optionalString(r)
        case "bookingUrlRest" => rest = BsonReads.optionalString(r).orNull
        case "room"           => room = BsonReads.optionalString(r)
        case "format"         => format = BsonReads.strings(r)
        case _                => r.skipValue()
      }
    }
    r.readEndDocument()
    if (dateTime == null) throw new org.bson.codecs.configuration.CodecConfigurationException("Showtime: no dateTime")
    if (rest != null && bookingUrl.isEmpty) Showtime.stored(dateTime, prefix, rest, room, format)
    else Showtime(dateTime, bookingUrl, room, format)
  }

  /** A row's showtimes, read before or after its `bookingUrlPrefix` (`prefix`, null if it has
   *  none): each one still awaiting the prefix completed with it. The showtimes themselves when
   *  none was — every row stored whole, and every row whose prefix came first. */
  def completed(showtimes: Seq[Showtime], prefix: String): Seq[Showtime] =
    if (showtimes.exists(_.awaitsRowPrefix)) showtimes.map(_.withRowPrefix(prefix)) else showtimes

  /** The row field [[read]]'s remainders are split from. */
  val RowPrefixField = "bookingUrlPrefix"
}

/** The BSON reads the hand-written decoders share. Stateless. */
private[services] object BsonReads {
  /** Fail the decode of the `what` row `id` unless `present`: a required field is missing. */
  def require(present: Boolean, what: String, id: String): Unit =
    if (!present)
      throw new org.bson.codecs.configuration.CodecConfigurationException(s"$what ${Option(id).getOrElse("?")}: a required field is missing")

  def optionalString(r: BsonReader): Option[String] =
    if (r.getCurrentBsonType == BsonType.NULL) { r.readNull(); None } else Some(r.readString())

  def strings(r: BsonReader): List[String] = {
    val out = List.newBuilder[String]
    r.readStartArray()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) out += r.readString()
    r.readEndArray()
    out.result()
  }
}

/**
 * BSON codec wiring for the Mongo-backed repository. The macros write the case
 * classes; `Showtime`, not one, has [[ShowtimeCodec]], and `LocalDateTime` has
 * `JavaTimeCodecs.localDateTime` — both shared with the read-model collections.
 */
object MovieCodecs extends PersistedCodecs {

  /** `SourceData`, `StoredScreeningsDto` and `StoredSlotDto` are READ by the hand-written codecs above,
   *  each writing through the macro codec derived here; `Showtime` has no macro codec — [[ShowtimeCodec]]. */
  type OmittingNone = (SourceData, TitleSearch)
  /** `movies`, `screenings`, `movie_slots`. */
  type WritingNone  = (StoredTmdbAttempt, StoredMovieDto, StoredScreeningsDto, StoredSlotDto)

  val registry: CodecRegistry = {
    // Every macro codec, none shadowed — what the hand-written codecs write through.
    val macros = fromRegistries(
      fromCodecs(JavaTimeCodecs.localDateTime, ShowtimeCodec),
      fromProviders((PersistedCodecs.omittingNone[OmittingNone] ::: PersistedCodecs.writingNone[WritingNone])*),
      DEFAULT_CODEC_REGISTRY)
    val showtimes  = ShowtimeCodec
    val sourceData = new BackwardCompatibleSourceDataCodec(macros.get(classOf[SourceData]), showtimes, macros.get(classOf[TitleSearch]))
    // The macro codecs a row codec WRITES through, their nested slots and showtimes going through
    // the codecs above — so a cache-stripped slot sheds its cache-only fields in a `movie_slots`
    // row exactly as in `movies`.
    val writers = fromRegistries(fromCodecs(JavaTimeCodecs.localDateTime, sourceData, showtimes), macros)
    fromRegistries(
      // FIRST, so they shadow the macro codecs of the same classes: a slot round-trips through the
      // SAME backward-compatible codec whether it is read from `movies` or from its own
      // `movie_slots` row, and every showtime through the one streaming decoder.
      fromCodecs(JavaTimeCodecs.localDateTime,
        sourceData,
        showtimes,
        StreamingScreeningsCodec,
        new StreamingSlotCodec(writers.get(classOf[StoredSlotDto]), sourceData)),
      fromProviders((PersistedCodecs.omittingNone[OmittingNone] ::: PersistedCodecs.writingNone[WritingNone])*),
      DEFAULT_CODEC_REGISTRY)
  }
}
