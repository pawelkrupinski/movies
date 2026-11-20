package services.movies

import models.{MovieRecord, Source, SourceData, StoredMovieRecord, TmdbAttempt}
import services.titles.TitleNormalizer

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
    row.readBack(row.record.displayTitle(_, normalizer))

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
  def fromStorage(id: String, key: Option[String], record: MovieRecord, normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord =
    StoredMovieRecord.fromStorage(id, key, record, record.displayTitle(_, normalizer))
}
