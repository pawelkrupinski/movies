package services.movies

/**
 * The identity of a film row: the `_id` of its `movies` document, and the `filmId`
 * its `movie_slots`, `screenings` and read-model rows are filed under.
 *
 * Opaque and PERMANENT. It is assigned when the row is created and never changes —
 * which is the whole point. Until 2026-09 the `_id` was the lookup key
 * `sanitize(displayTitle)|year`, and both halves of that string are functions of
 * mutable state (TMDB's year once resolved; whichever spelling the slot set makes
 * dominant), so every spelling or year change was a delete-and-upsert across three
 * collections plus a read-model reprojection: 1,211 such moves in nine days of
 * production logs, 122 of them ping-ponging. Now a spelling or year change is a
 * RETITLE — the row's `key` field moves, its id does not — and only a genuine merge
 * of two films moves side rows (`SideCollectionMove`).
 *
 * A row created before the change keeps its old `_id` as its id (`persepolis|2007`):
 * it is just an opaque string now and nothing parses it. A new row's id is `f` + 15
 * hex characters derived from the key it was created under, so a replay of the same
 * arrivals assigns the same ids; which key a film is first seen under depends on
 * arrival order, so two replays in different orders may id the same film differently
 * — opaque means exactly that nothing may care. A collision with a LIVE id (the same
 * first key, used again after that row was retitled) bumps a nonce.
 */
final case class FilmId(value: String) extends AnyVal {
  override def toString: String = value
}

object FilmId {
  /** The id of a row created under `key`. `taken` says whether a candidate is a live
   *  id already — the corpus index in the cache, a lookup in the fold's transaction. */
  def fresh(key: CacheKey, taken: FilmId => Boolean): FilmId =
    Iterator.from(0)
      .map(nonce => FilmId("f" + tools.Digest.sha1Hex(StoredMovieRecord.keyFor(key) + (if (nonce == 0) "" else s"#$nonce")).take(15)))
      .dropWhile(taken)
      .next()

  /** The id a row created BEFORE ids existed carries: its old `_id`, which was the
   *  key. Also what an in-memory row synthesised without storage answers to. */
  def legacy(key: CacheKey): FilmId = FilmId(StoredMovieRecord.keyFor(key))
  def legacy(title: String, year: Option[Int], normalizer: TitleNormalizer): FilmId =
    FilmId(StoredMovieRecord.keyFor(title, year, normalizer))
}
