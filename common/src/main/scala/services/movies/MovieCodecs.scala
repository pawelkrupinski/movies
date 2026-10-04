package services.movies

import models.{MovieRecord, Showtime, Source, SourceData, TitleSearch}
import services.resolution.TmdbAttempt
import org.bson.{BsonReader, BsonType, BsonWriter}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import services.PersistedCodecs

import java.time.Instant

/** Wire form of [[services.resolution.TmdbAttempt]]. */
case class StoredTmdbAttempt(evidence: String, at: Instant)

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
  // The lookup key `sanitize(title)|year` — a FIELD, because `_id` is the permanent
  // `FilmId` and a retitle moves the key, not the document. Optional on the wire: a
  // document written before ids existed has none, and its `_id` IS its key
  // (`StoredMovieRecord.fromStorage`); `MongoMovieRepository` backfills it at boot.
  key:               Option[String],
  imdbId:            Option[String],
  imdbRating:        Option[Double],
  metascore:         Option[Int],
  filmwebUrl:        Option[String],
  filmwebRating:     Option[Double],
  rottenTomatoes:    Option[Int],
  tmdbId:            Option[Int],
  // WHAT the tmdbId was concluded from (a `TmdbBasis` name). Optional on the wire
  // so legacy documents decode to None — which `CinemaCorroboration` reads as "no
  // evidence recorded", never as a guess. Without this column the basis lived only
  // in the in-memory cache and was lost at every hydrate, so the re-resolve-a-guess
  // path could never fire on a restarted worker.
  tmdbBasis:         Option[String],
  // Optional on the wire so legacy documents (written before this existed)
  // decode to None.
  wikidataId:        Option[String],
  metacriticUrl:     Option[String],
  rottenTomatoesUrl: Option[String],
  searchTitle:       Option[String],
  // READ-ONLY, for documents written before `tmdbAttempt` existed: a `true` here
  // decodes as a no-match attempt on unknown inputs (`TmdbAttempt.Legacy`), which
  // the next look retries. Never written any more — `fromDomain` leaves it None.
  tmdbNoMatch:       Option[Boolean],
  // The last TMDB search that found nothing — its input fingerprint and time.
  // Optional on the wire so legacy documents decode to None.
  tmdbAttempt:       Option[StoredTmdbAttempt],
  // Optional on the wire so a MIGRATED document decodes to None → empty map. Once
  // a film's slots have landed in `movie_slots`, the 2026-07 slot migration
  // `$unset` this field entirely — and as a required `Map` it decoded as
  // `Missing field: sourceData`, killing the whole keyset batch (so the corpus
  // scan reported incomplete) and aborting every write path that loaded such a
  // row. Encoding is unchanged —
  // `fromDomain` always writes the map, empty or not.
  sourceData:        Option[Map[String, SourceData]],
  // Longest synopsis kept per source after its live slot was pruned, keyed by
  // `Source.displayName` like `sourceData`. Optional so legacy documents decode
  // to None → empty map; omitted when empty to keep documents lean.
  retainedSynopses:  Option[Map[String, String]],
  // The only timestamp. A `slotsUpdatedAt` marker used to sit beside it — stamped by a
  // slots-only patch that had to touch `movies` while `movie_slots` had no cursor of its
  // own — and prod documents still carry it; the codec skips a field it does not name
  // (pinned by `MovieRecordFieldWiringSpec`).
  updatedAt:         Instant
)

object StoredMovieDto {
  // `title`/`year` are no longer persisted: the `_id` is `sanitize(title)|year`,
  // so the year is recoverable from it, and the display title is derived from
  // `sourceData` on read. Storing them was a second, order-dependent source of
  // truth (the title was pinned to whichever scrape wrote the row first); see
  // `toDomain`. The `id` still encodes both — the caller computes it via
  // `MovieRepository.documentId(title, year)` — so the cache key is unchanged.
  def fromDomain(id: String, key: String, r: MovieRecord, updatedAt: Instant): StoredMovieDto =
    fromDomain(id, r, updatedAt).copy(key = Some(key))

  /** A document with no `key` of its own — the staging store's rows (keyed by cinema,
   *  never looked up by film key) and the legacy shape the codec specs round-trip. */
  def fromDomain(id: String, r: MovieRecord, updatedAt: Instant): StoredMovieDto =
    StoredMovieDto(
      _id               = id,
      key               = None,
      imdbId            = r.imdbId,
      imdbRating        = r.imdbRating,
      metascore         = r.metascore,
      filmwebUrl        = r.filmwebUrl,
      filmwebRating     = r.filmwebRating,
      rottenTomatoes    = r.rottenTomatoes,
      tmdbId            = r.tmdbId,
      tmdbBasis         = r.tmdbBasis,
      wikidataId        = r.wikidataId,
      metacriticUrl     = r.metacriticUrl,
      rottenTomatoesUrl = r.rottenTomatoesUrl,
      searchTitle       = r.searchTitle,
      tmdbNoMatch       = None,
      tmdbAttempt       = r.tmdbAttempt.map(a => StoredTmdbAttempt(a.evidence, a.at)),
      // Always `Some` — the write shape is unchanged (an empty map still encodes as
      // `sourceData: {}`). Only READS tolerate the field's absence; dropping the
      // embedded copy is the migration's job, not the codec's.
      sourceData        = Some(r.data.map { case (s, sd) => s.displayName -> sd }),
      retainedSynopses  = Option.when(r.retainedSynopses.nonEmpty)(
                            r.retainedSynopses.map { case (s, v) => s.displayName -> v }),
      updatedAt         = updatedAt
    )

  def toDomain(dto: StoredMovieDto, normalizer: TitleNormalizer): StoredMovieRecord = {
    val record = MovieRecord(
      imdbId            = dto.imdbId,
      imdbRating        = dto.imdbRating,
      metascore         = dto.metascore,
      filmwebUrl        = dto.filmwebUrl,
      filmwebRating     = dto.filmwebRating,
      rottenTomatoes    = dto.rottenTomatoes,
      tmdbId            = dto.tmdbId,
      tmdbBasis         = dto.tmdbBasis,
      wikidataId        = dto.wikidataId,
      metacriticUrl     = dto.metacriticUrl,
      rottenTomatoesUrl = dto.rottenTomatoesUrl,
      searchTitle       = dto.searchTitle,
      tmdbAttempt       = dto.tmdbAttempt.map(a => TmdbAttempt(a.evidence, a.at))
                            .orElse(Option.when(dto.tmdbNoMatch.contains(true))(TmdbAttempt.Legacy)),
      // Drop any legacy bare-Cinema slot a per-title CinemaShowing slot now
      // supersedes — pre-split rows (before commit 847f555f) keyed a cinema's
      // slot by the bare Cinema, so a re-scraped film carries BOTH keys with the
      // same showtimes (the /debug twin-slot duplication). See
      // `Source.dropSupersededCinemaSlots`.
      data              = Source.dropSupersededCinemaSlots(
                            dto.sourceData.getOrElse(Map.empty)
                              .flatMap { case (k, sd) => Source.byWireKey(k).map(_ -> sd) }),
      retainedSynopses  = dto.retainedSynopses.getOrElse(Map.empty)
                            .flatMap { case (k, v) => Source.byWireKey(k).map(_ -> v) }
    )
    // title + year are derived from the `_id` + `sourceData`, not stored — see
    // `StoredMovieRecord.fromStorage` (shared with the in-memory repository).
    StoredMovieRecord.fromStorage(dto._id, dto.key, record, normalizer)
  }
}

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
    def str(): Option[String] = if (r.getCurrentBsonType == BsonType.STRING) Some(r.readString()) else { r.skipValue(); None }
    def int(): Option[Int]    = if (r.getCurrentBsonType == BsonType.INT32) Some(r.readInt32()) else { r.skipValue(); None }
    def strings(): Seq[String] = r.getCurrentBsonType match {
      case BsonType.STRING =>
        val joined = r.readString()
        if (joined.isEmpty) Seq.empty else joined.split(",").map(_.trim).filter(_.nonEmpty).toSeq
      case BsonType.ARRAY =>
        r.readStartArray()
        val out = Seq.newBuilder[String]
        while (r.readBsonType() != BsonType.END_OF_DOCUMENT) out += r.readString()
        r.readEndArray()
        out.result()
      case _ => r.skipValue(); Seq.empty
    }
    def documents[A](codec: Codec[A]): Seq[A] = r.getCurrentBsonType match {
      case BsonType.ARRAY =>
        r.readStartArray()
        val out = Seq.newBuilder[A]
        while (r.readBsonType() != BsonType.END_OF_DOCUMENT) out += codec.decode(r, c)
        r.readEndArray()
        out.result()
      case _ => r.skipValue(); Seq.empty
    }
    var title, rawTitle, originalTitle, englishTitle, synopsis, posterUrl, filmUrl, trailerUrl, language, ageRating =
      Option.empty[String]
    var runtimeMinutes, releaseYear = Option.empty[Int]
    var cast, director, countries, genres = Seq.empty[String]
    var showtimes     = Seq.empty[Showtime]
    var titleSearches = Seq.empty[TitleSearch]
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "title"          => title = str()
        case "rawTitle"       => rawTitle = str()
        case "originalTitle"  => originalTitle = str()
        case "englishTitle"   => englishTitle = str()
        case "synopsis"       => synopsis = str()
        case "cast"           => cast = strings()
        case "director"       => director = strings()
        case "runtimeMinutes" => runtimeMinutes = int()
        case "releaseYear"    => releaseYear = int()
        case "countries"      => countries = strings()
        case "genres"         => genres = strings()
        case "posterUrl"      => posterUrl = str()
        case "filmUrl"        => filmUrl = str()
        case "trailerUrl"     => trailerUrl = str()
        // Absent on every row written before the stamp existed — `None` reads as pl-PL (the
        // historical hardcoded enrichment language), which is what those rows actually hold.
        case "language"       => language = str()
        case "showtimes"      => showtimes = documents(showtimeCodec)
        // Absent on every row written before the certificate field existed → None.
        case "ageRating"      => ageRating = str()
        // Absent on every slot written before the field existed → no search evidence.
        case "titleSearches"  => titleSearches = documents(titleSearchCodec)
        case _                => r.skipValue()
      }
    }
    r.readEndDocument()
    SourceData(
      title = title, rawTitle = rawTitle, originalTitle = originalTitle, englishTitle = englishTitle,
      synopsis = synopsis, cast = cast, director = director, runtimeMinutes = runtimeMinutes,
      releaseYear = releaseYear, countries = countries, genres = genres, posterUrl = posterUrl,
      filmUrl = filmUrl, trailerUrl = trailerUrl, language = language, showtimes = showtimes,
      ageRating = ageRating, titleSearches = titleSearches)
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

/** A `screenings` row read field by field — its showtimes through [[ShowtimeCodec]].
 *  ~108k rows a US pass. Written by the macro codec; read as it reads, as above. */
private[movies] object StreamingScreeningsCodec extends Codec[StoredScreeningsDto] {
  override def getEncoderClass: Class[StoredScreeningsDto] = classOf[StoredScreeningsDto]
  /** As the macro codec wrote it (`WritingNone`: an absent `listingKey` is null), its showtimes
   *  split at their row's prefix ([[ShowtimeCodec.writeShowtimes]]). */
  override def encode(w: BsonWriter, v: StoredScreeningsDto, c: EncoderContext): Unit = {
    w.writeStartDocument()
    w.writeString("_id", v._id)
    w.writeString("filmId", v.filmId)
    w.writeString("slotKey", v.slotKey)
    ShowtimeCodec.writeShowtimes(w, v.showtimes, c)
    w.writeDateTime("updatedAt", v.updatedAt.toEpochMilli)
    v.listingKey match {
      case Some(key) => w.writeString("listingKey", key)
      case None      => w.writeNull("listingKey")
    }
    w.writeEndDocument()
  }
  override def decode(r: BsonReader, c: DecoderContext): StoredScreeningsDto = {
    var id, filmId, slotKey: String = null
    var updatedAt: Instant          = null
    var listingKey                  = Option.empty[String]
    var urlPrefix: String           = null
    val read                        = Vector.newBuilder[Showtime]
    var sawShowtimes                = false
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "_id"        => id = r.readString()
        case "filmId"     => filmId = r.readString()
        case "slotKey"    => slotKey = r.readString()
        case "updatedAt"  => updatedAt = Instant.ofEpochMilli(r.readDateTime())
        case "listingKey" => listingKey = BsonReads.optionalString(r)
        case ShowtimeCodec.RowPrefixField => urlPrefix = BsonReads.optionalString(r).orNull
        case "showtimes"  =>
          sawShowtimes = true
          r.readStartArray()
          while (r.readBsonType() != BsonType.END_OF_DOCUMENT) read += ShowtimeCodec.read(r, c, urlPrefix)
          r.readEndArray()
        case _            => r.skipValue()
      }
    }
    r.readEndDocument()
    if (id == null || filmId == null || slotKey == null || updatedAt == null || !sawShowtimes)
      throw new org.bson.codecs.configuration.CodecConfigurationException(s"StoredScreeningsDto ${Option(id).getOrElse("?")}: a required field is missing")
    StoredScreeningsDto(id, filmId, slotKey, ShowtimeCodec.completed(read.result(), urlPrefix), updatedAt, listingKey)
  }
}

/** A `movie_slots` row read field by field — its slot through the backward-compatible
 *  [[SourceData]] codec. ~113k rows a US pass. Written by the macro codec; read as it reads:
 *  an absent or null `listingKey` is `None`, an unknown field is skipped, a missing required
 *  field fails (`SlotDecodeSpec`). */
private[movies] final class StreamingSlotCodec(macroCodec: Codec[StoredSlotDto], sourceData: Codec[SourceData])
    extends Codec[StoredSlotDto] {
  override def getEncoderClass: Class[StoredSlotDto] = classOf[StoredSlotDto]
  override def encode(w: BsonWriter, v: StoredSlotDto, c: EncoderContext): Unit = macroCodec.encode(w, v, c)
  override def decode(r: BsonReader, c: DecoderContext): StoredSlotDto = {
    var id, filmId, slotKey: String = null
    var slot: SourceData            = null
    var updatedAt: Instant          = null
    var listingKey                  = Option.empty[String]
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) {
      r.readName() match {
        case "_id"        => id = r.readString()
        case "filmId"     => filmId = r.readString()
        case "slotKey"    => slotKey = r.readString()
        case "slot"       => slot = sourceData.decode(r, c)
        case "updatedAt"  => updatedAt = Instant.ofEpochMilli(r.readDateTime())
        case "listingKey" => listingKey = BsonReads.optionalString(r)
        case _            => r.skipValue()
      }
    }
    r.readEndDocument()
    if (id == null || filmId == null || slotKey == null || slot == null || updatedAt == null)
      throw new org.bson.codecs.configuration.CodecConfigurationException(s"StoredSlotDto ${Option(id).getOrElse("?")}: a required field is missing")
    StoredSlotDto(id, filmId, slotKey, slot, updatedAt, listingKey)
  }
}

/** The BSON reads the hand-written decoders share. Stateless. */
private[services] object BsonReads {
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
