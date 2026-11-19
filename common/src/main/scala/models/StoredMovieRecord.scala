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

  /** Whether the read model may project this row: its record's [[MovieRecord.readyToProject]], asked of the row so
   *  a reader of rows (the projector, its sweeps and boot) need not reach into the record for it. */
  def readyToProject: Boolean = record.readyToProject

  /** A copy of this row with every source's `showtimes` dropped — the shared rule behind
   *  `MovieRepositoryReader.findAllForListing`. `MongoMovieRepository` strips the same field server-side; this keeps the
   *  in-memory store's listing view byte-for-byte equivalent so both impls honour the same "no showtimes" contract (the
   *  listing renders cinema metadata + counts, never showtimes). */
  def withoutShowtimes: StoredMovieRecord =
    copy(record = record.copy(data = record.data.view.mapValues(_.copy(showtimes = Seq.empty)).toMap))

  /** This row as a read of it back from the store would give it ([[StoredMovieRecord.fromStorage]]), titled by
   *  `displayTitle`. */
  def readBack(displayTitle: String => String): StoredMovieRecord =
    storedKey.fold(this)(key => StoredMovieRecord.fromStorage(id.value, Some(key), record, displayTitle))
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

  /** A row rebuilt from its persisted `_id` and `key` field (absent on a document written before keys were stored --
   *  then the `_id` IS the key): the key is `sanitize(title)|year`, so its suffix is the year, and `displayTitle` names
   *  the row from its key's prefix (`StoredMovieDto.fromStorage`, which names it from the record's slots). */
  def fromStorage(id: String, key: Option[String], record: MovieRecord, displayTitle: String => String): StoredMovieRecord = {
    val k        = key.getOrElse(id)
    val sep      = k.lastIndexOf('|')
    val idPrefix = if (sep >= 0) k.substring(0, sep) else k
    val year     = if (sep >= 0) k.substring(sep + 1).toIntOption else None
    StoredMovieRecord(displayTitle(idPrefix), year, record, FilmId(id), Some(k))
  }
}
