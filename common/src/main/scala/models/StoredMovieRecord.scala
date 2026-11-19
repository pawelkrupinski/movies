package models

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
  def key(normalizer: String => String): String = storedKey.getOrElse(StoredMovieRecord.keyFor(title, year, normalizer))

  /** The cache key this row answers to — the stored key, labelled with the display title. */
  def cacheKey(normalizer: String => String): CacheKey =
    storedKey.fold(CacheKey(title, year, normalizer))(CacheKey.stored(title, _))
}

object StoredMovieRecord {
  /** A row synthesised without storage: its id is the legacy form of its key under
   *  `normalizer`'s rules. */
  def synthesised(title: String, year: Option[Int], record: MovieRecord, normalizer: String => String): StoredMovieRecord =
    StoredMovieRecord(title, year, record, FilmId.legacy(title, year, normalizer))

  /** The lookup KEY of a `(title, year)` row: `sanitize(title)|year` — the `key` field of
   *  the document (and, for a row stored before ids existed, its `_id` too). Matches
   *  the in-memory `CacheKey` normalization (case/diacritic-folded). */
  def keyFor(title: String, year: Option[Int], normalizer: String => String): String =
    s"${normalizer(title)}|${year.map(_.toString).getOrElse("")}"

  /** The same key, for a caller that already holds it. A [[CacheKey]] carries the
   *  normalised form it was BUILT with, so this needs no normalizer and cannot re-derive
   *  a different one. */
  def keyFor(k: CacheKey): String =
    s"${k.normalized}|${k.year.map(_.toString).getOrElse("")}"
}
