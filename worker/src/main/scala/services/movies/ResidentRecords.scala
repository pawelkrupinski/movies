package services.movies

import models.{MovieRecord, SourceData}

/** How [[CaffeineMovieCache]] shapes a row to hold it resident: its slots' showtimes stripped while
 *  `screenings` holds them (`stripShowtimes`), and every string a slot repeats across cinemas taken
 *  from the one `stringPool` — keeping, wherever a slot reads as the one already held, that very
 *  object, so the comparisons on the hot write paths stop at `eq`. */
private[movies] final class ResidentRecords(stripShowtimes: () => Boolean, stringPool: StringPool) {
  /** `r` as the cache holds it: every slot shaped by [[slot]]. */
  def of(r: MovieRecord): MovieRecord = r.copy(data = r.data.view.mapValues(slot).toMap)
  /** [[of]] of one slot — the slot itself when it is one already: stripped, every string the pool's. The identity
   *  projection writes the lean slots it keeps, so a film written again at the same venues hands the cache back the very
   *  objects it holds, and every comparison of the two stops at `eq` ([[over]]). */
  def slot(sd: SourceData): SourceData = {
    // Strip only when the read-split is active (showtimes live in `screenings`); without it
    // the cache must keep showtimes — there's nowhere else to hold them.
    val lean   = if (stripShowtimes()) ShowtimesDigest.stripSlot(sd) else sd
    val pooled = stringPool.slot(lean)
    if (ResidentRecords.pooledAlike(pooled, lean)) lean else pooled
  }

  /** [[of]] of `read`, a change-stream read of a film the cache holds as `held`: each slot that reads as the held
   *  one ([[kept]]) is the held object, and a read alike in every field and slot is `held` itself. */
  def keeping(held: MovieRecord, read: MovieRecord): MovieRecord = {
    val data = read.data.map { case (source, sd) => source -> kept(held.data.get(source), slot(sd)) }
    val same = data.size == held.data.size && data.forall { case (source, sd) => held.data.get(source).exists(_ eq sd) } &&
      read.copy(data = Map.empty) == held.copy(data = Map.empty)
    if (same) held else read.copy(data = data)
  }

  /** `read` — a slot as the cache keeps it — or the slot held at its source when that one reads the same: equal fields,
   *  showtimes by digest, and the cast in the same order (equality reads it as a set; the store keeps the order). */
  def kept(held: Option[SourceData], read: SourceData): SourceData =
    held.filter(h => LeanRecords.slotsEqual(h, read) && h.cast == read.cast).getOrElse(read)

  /** [[of]] of `after` written over `before`, which the cache holds: a slot `after` holds as `before` does is
   *  kept as it is, not stripped and pooled again — on a film at thousands of venues, every write did that to all. */
  def over(before: MovieRecord, after: MovieRecord): MovieRecord =
    // Over `after`'s own map, moving only what it holds unstripped: a map rebuilt whole was a copy of a wide film's
    // thousands of slots each write, kept until the next one — promoted, to die in the old generation.
    after.copy(data = after.data.foldLeft(after.data) { case (data, (source, sd)) =>
      if (before.data.get(source).exists(_ eq sd)) data else { val cached = slot(sd); if (cached eq sd) data else data.updated(source, cached) }
    })
}

private[movies] object ResidentRecords {
  // Rating sources occasionally hand us a literal zero — MC/RT search pages
  // that surface 0% for an unrated title, Filmweb's API for a film with no
  // votes yet, IMDb GraphQL for a brand-new entry. Zero isn't a real rating;
  // persisting it would render a misleading "0/10" badge. Squash to None at
  // the single write boundary so neither Caffeine nor Mongo holds the
  // phantom score. Applied to every write (`persist` and `putIfPresent`, which
  // `putSlotsIfPresent` defers to for a row carrying one), so any future caller
  // automatically inherits the rule.
  def withoutZeroRatings(e: MovieRecord): MovieRecord = e.copy(
    imdbRating     = e.imdbRating.filter(_ > 0.0),
    metascore      = e.metascore.filter(_ > 0),
    filmwebRating  = e.filmwebRating.filter(_ > 0.0),
    rottenTomatoes = e.rottenTomatoes.filter(_ > 0)
  )

  /** Would [[withoutZeroRatings]] change `e`? */
  def carriesZeroRating(e: MovieRecord): Boolean =
    e.imdbRating.exists(_ <= 0.0) || e.metascore.exists(_ <= 0) ||
    e.filmwebRating.exists(_ <= 0.0) || e.rottenTomatoes.exists(_ <= 0)

  /** Whether pooling `slot` ([[StringPool.slot]] → `pooled`) changed none of its fields: every one the pool's already. */
  def pooledAlike(pooled: SourceData, slot: SourceData): Boolean =
    (pooled.title eq slot.title) && (pooled.rawTitle eq slot.rawTitle) && (pooled.originalTitle eq slot.originalTitle) &&
      (pooled.englishTitle eq slot.englishTitle) && (pooled.synopsis eq slot.synopsis) && (pooled.cast eq slot.cast) &&
      (pooled.director eq slot.director) && (pooled.countries eq slot.countries) && (pooled.genres eq slot.genres) &&
      (pooled.posterUrl eq slot.posterUrl) && (pooled.filmUrl eq slot.filmUrl) && (pooled.trailerUrl eq slot.trailerUrl) &&
      (pooled.language eq slot.language) && (pooled.ageRating eq slot.ageRating) &&
      (pooled.runtimeMinutes eq slot.runtimeMinutes) && (pooled.releaseYear eq slot.releaseYear)
}
