package services.movies

import models.MovieRecord

/**
 * Pure collapse of a cluster of same-film rows into the single canonical
 * `(CacheKey, MovieRecord)` it should be stored under.
 *
 * Extracted from `MovieCache.collapseCluster` so the SAME decision — which
 * year, which spelling, which merged record — can be reused outside the cache
 * (a future projector / reconciler) without dragging in the cache's mutation,
 * locks, or Caffeine state. This object is side-effect-free: it never reads or
 * writes the cache; it only computes. `MovieCache` keeps the mutation half
 * (the `needsFix` guard + `withTitleLock`/`invalidate`/`put`) and delegates the
 * decision here, so there is ONE definition of the canonicalisation rules.
 */
object FilmCanonicalizer {

  /** Total order picking the canonical (surviving) key among same-tmdbId,
   *  same-normalised-title rows: prefer a row that carries a release year over
   *  a yearless one, then the lower year, then the cleanTitle. A pure function
   *  of the key, so the canonical never depends on write order. */
  private[services] def canonicalRank(k: CacheKey): (Boolean, Int, String) =
    (k.year.isEmpty, k.year.getOrElse(Int.MaxValue), k.cleanTitle)

  /** Year a cluster collapses to — TMDB's resolved year if any member carries
   *  one (all resolved members of a cluster share a tmdbId hence a tmdbYear),
   *  else the lowest present key/slot year, else yearless. */
  private[services] def clusterYear(cluster: Seq[(CacheKey, MovieRecord)]): Option[Int] =
    cluster.flatMap { case (_, e) => e.tmdbYear }.minOption.orElse {
      val present = (cluster.iterator.map(_._1.year) ++
                     cluster.iterator.flatMap { case (_, e) => e.data.values.iterator.map(_.releaseYear) }).flatten.toSeq
      if (present.isEmpty) None else Some(present.min)
    }

  /** The single canonical key + merged record for a cluster of same-film rows.
   *  Spelling is decoupled from year:
   *    - year:    TMDB's resolved year is authoritative (it overrides
   *      cinema-reported years, which often carry the production year and
   *      disagree — the "Dzień objawienia" 2025 vs 2026 split); only an
   *      all-unresolved cluster falls back to the lowest present year.
   *    - spelling: the min cleanTitle across ALL variants regardless of year, so
   *      a yearless all-caps variant ("SAVAGE HOUSE") can't win the spelling
   *      just because it's the only one at the resolved year.
   *
   *  The merge is `unionAll`, not `reduce(union)`: it picks the tmdbId-bearing
   *  row as the union base, so a lower-canonicalRank UNRESOLVED ±1 sibling (e.g.
   *  the production-year 2025 row attached to a TMDB-2026 resolved cluster)
   *  can't clobber the resolved row's tmdbId/imdbId/ratings. Order independent
   *  for the per-source `data` (it's a keyed merge). */
  def canonical(cluster: Seq[(CacheKey, MovieRecord)]): (CacheKey, MovieRecord) = {
    // Every reported variant: each cinema slot's derived key (cleaned title +
    // year) plus the rows' current keys.
    val slotKeys = cluster.flatMap { case (_, e) => e.data.values.flatMap(d => d.title.map(t => CacheKey(t, d.releaseYear))) }
    val keys     = cluster.map(_._1)
    val allKeys  = slotKeys ++ keys
    val canonicalYear = clusterYear(cluster)
    // Prefer a normally-cased spelling over a SHOUTING one ("Savage House" over
    // "SAVAGE HOUSE"), then break ties by string order — a pure function of the
    // variant set.
    def isAllCaps(t: String): Boolean = t.exists(_.isLetter) && t == t.toUpperCase(java.util.Locale.ROOT)
    val canonicalTitle = allKeys.map(_.cleanTitle).minBy(t => (isAllCaps(t), t))
    val canonicalKey   = CacheKey(canonicalTitle, canonicalYear)
    val merged = MovieRecordMerge.unionAll(cluster.sortBy { case (k, _) => canonicalRank(k) }.map(_._2))
    (canonicalKey, merged)
  }
}
