package services.movies

import models.{FilmId, StoredMovieRecord}

import services.titles.TitleNormalizer

import models.{MovieRecord, Showtime, Source, SourceData, TitleSearch}
import models.TmdbAttempt
import org.bson.{BsonReader, BsonType, BsonWriter}
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import services.PersistedCodecs

import java.time.Instant

/** Wire form of [[models.TmdbAttempt]]. */
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
  // (`StoredMovieDto.fromStorage`); `MongoMovieRepository` backfills it at boot.
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
  // `MovieRecord.tmdbProvisional`: Some(true) while the take is Pending, absent otherwise (legacy rows decode as None).
  tmdbProvisional:   Option[Boolean],
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
      tmdbProvisional   = r.tmdbProvisional.filter(identity),
      updatedAt         = updatedAt
    )

  def toDomain(dto: StoredMovieDto, normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord = {
    val record = MovieRecord(
      imdbId            = dto.imdbId,
      imdbRating        = dto.imdbRating,
      metascore         = dto.metascore,
      filmwebUrl        = dto.filmwebUrl,
      filmwebRating     = dto.filmwebRating,
      rottenTomatoes    = dto.rottenTomatoes,
      tmdbId            = dto.tmdbId,
      tmdbBasis         = dto.tmdbBasis,
      tmdbProvisional   = dto.tmdbProvisional.filter(identity),
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
    // `fromStorage` (shared with the in-memory repository).
    fromStorage(dto._id, dto.key, record, normalizer)
  }

  // A row as the store gives it back: titled with the display title its record votes for under its stored key
  // ([[CinemaSlotReadings]]' `displayTitle`). Here, not on [[StoredMovieRecord]] (a plain value in models, below the
  // title logic), because naming a row from its slots is title logic.
  /** `row` as a read of it back from the store would give it ([[fromStorage]]). A row the cache holds keeps the title it
   *  was keyed under, which its record's later slots can outvote — and the read model titles, and splits, the row the
   *  store gives. */
  def readBack(row: StoredMovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord =
    row.storedKey.fold(row)(key => fromStorage(row.id.value, Some(key), row.record, normalizer))

  /** A document with no `key` field — written before ids existed, its `_id` is its key. */
  def fromStorage(id: String, record: MovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord =
    fromStorage(id, None, record, normalizer)

  /** Rebuild a stored row from its persisted `_id`, its `key` field (absent on a
   *  document written before keys were stored — then the `_id` IS the key) and its
   *  `MovieRecord`, deriving the display `title` and `year` rather than reading pinned
   *  columns — used by the Mongo codec (`MovieCodecs.toDomain`), whose BSON drops the
   *  `title`/`year` fields. The key is `sanitize(title)|year`: `sanitize` never emits
   *  `|`, so the suffix is the year and the prefix is the cache key's sanitized form.
   *  Every spelling in a row sanitizes to that prefix (the `CacheKey` identity), so
   *  `displayTitle(prefix)` sanitizes back to it.
   *
   *  CALL IT WITH A COMPLETE RECORD. `displayTitle` names the film from its SLOTS,
   *  so a record whose slots have not been stitched back in yet has nothing to name
   *  it with and falls through to the key prefix — a real title only by accident
   *  ("Interstellar"), otherwise a mangled "Thecabinetofdrcaligari". That is exactly
   *  the state `MovieCodecs.toDomain` decodes into now that the slots live in
   *  `movie_slots`, which is why `MongoMovieRepository.stitchSlots` calls this again
   *  once the record is whole. */
  def fromStorage(id: String, key: Option[String], record: MovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord = {
    val k        = key.getOrElse(id)
    val sep      = k.lastIndexOf('|')
    val idPrefix = if (sep >= 0) k.substring(0, sep) else k
    val year     = if (sep >= 0) k.substring(sep + 1).toIntOption else None
    StoredMovieRecord(record.displayTitle(idPrefix, normalizer), year, record, FilmId(id), Some(k))
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
    val row = new ScreeningsRowFields
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) row.read(r.readName(), r, c)
    r.readEndDocument()
    row.result
  }

  /** A `screenings` row's fields as [[StreamingScreeningsCodec]] reads them; `result` fails on a missing required one. */
  private final class ScreeningsRowFields {
    private var id, filmId, slotKey: String = null
    private var updatedAt: Instant          = null
    private var listingKey                  = Option.empty[String]
    private var urlPrefix: String           = null
    private val showtimes                   = Vector.newBuilder[Showtime]
    private var sawShowtimes                = false

    def read(name: String, r: BsonReader, c: DecoderContext): Unit = name match {
      case "_id"        => id = r.readString()
      case "filmId"     => filmId = r.readString()
      case "slotKey"    => slotKey = r.readString()
      case "updatedAt"  => updatedAt = Instant.ofEpochMilli(r.readDateTime())
      case "listingKey" => listingKey = BsonReads.optionalString(r)
      case ShowtimeCodec.RowPrefixField => urlPrefix = BsonReads.optionalString(r).orNull
      case "showtimes"  => readShowtimes(r, c)
      case _            => r.skipValue()
    }

    private def readShowtimes(r: BsonReader, c: DecoderContext): Unit = {
      sawShowtimes = true
      r.readStartArray()
      while (r.readBsonType() != BsonType.END_OF_DOCUMENT) showtimes += ShowtimeCodec.read(r, c, urlPrefix)
      r.readEndArray()
    }

    def result: StoredScreeningsDto = {
      BsonReads.require(Seq(id, filmId, slotKey, updatedAt).forall(_ != null) && sawShowtimes, "StoredScreeningsDto", id)
      StoredScreeningsDto(id, filmId, slotKey, ShowtimeCodec.completed(showtimes.result(), urlPrefix), updatedAt, listingKey)
    }
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
    val row = new StreamingSlotCodec.SlotRowFields(sourceData)
    r.readStartDocument()
    while (r.readBsonType() != BsonType.END_OF_DOCUMENT) row.read(r.readName(), r, c)
    r.readEndDocument()
    row.result
  }
}

private[movies] object StreamingSlotCodec {
  /** A `movie_slots` row's fields as [[StreamingSlotCodec]] reads them; `result` fails on a missing required one. */
  final class SlotRowFields(sourceData: Codec[SourceData]) {
    private var id, filmId, slotKey: String = null
    private var slot: SourceData            = null
    private var updatedAt: Instant          = null
    private var listingKey                  = Option.empty[String]

    def read(name: String, r: BsonReader, c: DecoderContext): Unit = name match {
      case "_id"        => id = r.readString()
      case "filmId"     => filmId = r.readString()
      case "slotKey"    => slotKey = r.readString()
      case "slot"       => slot = sourceData.decode(r, c)
      case "updatedAt"  => updatedAt = Instant.ofEpochMilli(r.readDateTime())
      case "listingKey" => listingKey = BsonReads.optionalString(r)
      case _            => r.skipValue()
    }

    def result: StoredSlotDto = {
      BsonReads.require(Seq(id, filmId, slotKey, slot, updatedAt).forall(_ != null), "StoredSlotDto", id)
      StoredSlotDto(id, filmId, slotKey, slot, updatedAt, listingKey)
    }
  }
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
