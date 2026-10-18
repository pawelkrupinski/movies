package services.movies

import models.MovieRecord

/** One persisted film row: its display `title` and `year` (together, its lookup
 *  [[CacheKey]]), the record, and the [[FilmId]] it is stored under. Used as the return
 *  type of `MovieRepository.findAll` and `MovieCache.snapshot` so callers iterate
 *  named fields instead of destructuring an anonymous tuple.
 *
 *  `id` is the row's Mongo `_id` — permanent, see [[FilmId]]. A row synthesised
 *  without storage (tests, the odd in-memory construction) defaults to the legacy
 *  form, its key, which is what every row stored before ids existed carries. */
case class StoredMovieRecord(title: String, year: Option[Int], record: MovieRecord, id: FilmId,
                             storedKey: Option[String] = None) {
  /** The row's lookup key as STORED in the document's `key` field (for a row written
   *  before keys were stored, its `_id`); derived from the title and year only for a
   *  row synthesised without storage. NOT the identity: a retitle changes it, the `id`
   *  stays. */
  def key(normalizer: TitleNormalizer): String = storedKey.getOrElse(StoredMovieRecord.keyFor(title, year, normalizer))

  /** The cache key this row answers to — the stored key, labelled with the display title. */
  private[services] def cacheKey(normalizer: TitleNormalizer): CacheKey =
    storedKey.fold(CacheKey(title, year, normalizer))(CacheKey.stored(title, _))

  /** This row as a read of it back from the store would give it ([[StoredMovieRecord.fromStorage]]): titled with the
   *  display title its record votes for under its stored key. A row the cache holds keeps the title it was keyed under,
   *  which its record's later slots can outvote — and the read model titles, and splits, the row the store gives. */
  def asReadBack(normalizer: TitleNormalizer)(using LatestTitleYear): StoredMovieRecord =
    storedKey.fold(this)(key => StoredMovieRecord.fromStorage(id.value, Some(key), record, normalizer))
}

object StoredMovieRecord {
  /** A row synthesised without storage: its id is the legacy form of its key under
   *  `normalizer`'s rules. */
  def synthesised(title: String, year: Option[Int], record: MovieRecord, normalizer: TitleNormalizer): StoredMovieRecord =
    StoredMovieRecord(title, year, record, FilmId.legacy(title, year, normalizer))

  /** The lookup KEY of a `(title, year)` row: `sanitize(title)|year` — the `key` field of
   *  the document (and, for a row stored before ids existed, its `_id` too). Matches
   *  the in-memory `CacheKey` normalization (case/diacritic-folded). */
  def keyFor(title: String, year: Option[Int], normalizer: TitleNormalizer): String =
    s"${normalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** The same key, for a caller that already holds it. A [[CacheKey]] carries the
   *  normalised form it was BUILT with, so this needs no normalizer and cannot re-derive
   *  a different one. */
  def keyFor(k: CacheKey): String =
    s"${k.normalized}|${k.year.map(_.toString).getOrElse("")}"

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
