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

  /** One per-film cluster within a `sanitize(title)` group — its member rows and
   *  the reference year used for ±1 adjacency. `minRank` is the cluster's
   *  smallest `canonicalRank`, the deterministic tie-break for "which cluster is
   *  canonical / nearest". */
  private case class Cluster(refYear: Option[Int], rows: Seq[(CacheKey, MovieRecord)]) {
    def minRank: (Boolean, Int, String) = rows.map(r => canonicalRank(r._1)).min
  }

  /** Partition one `sanitize(title)` group into per-film clusters. A pure
   *  function of the row SET — every intermediate collection is sorted by
   *  `canonicalRank` (or a total order on its key) before iterating, so the
   *  result is independent of cache/scrape/iteration order. Precedence:
   *
   *    1. Resolved rows (carry a `tmdbId`) cluster BY tmdbId — two distinct ids
   *       are two films, never merged. Each resolved cluster's reference year is
   *       its `tmdbYear`.
   *    2. Unresolved rows that HAVE a year attach to a resolved cluster iff the
   *       year is within ±1 of that cluster's tmdbYear; ties (adjacent to
   *       several) break on the cluster's smallest `canonicalRank`.
   *    3. The remaining year-bearing unresolved rows form their OWN clusters by
   *       a greedy 2-year window from the lowest year — {y, y+1} absorbs all
   *       rows at y or y+1, the next window opens at the next distinct year
   *       > y+1. At most two neighbouring years per cluster, order-independent.
   *    4. Yearless AND idless rows fold into the group's canonical cluster (the
   *       smallest-`canonicalRank` cluster from 1–3) on the title match alone;
   *       a lone such row stays its own singleton. */
  def clusterByFilm(group: Seq[(CacheKey, MovieRecord)]): Seq[Seq[(CacheKey, MovieRecord)]] = {
    type Row = (CacheKey, MovieRecord)
    def rank(r: Row): (Boolean, Int, String) = canonicalRank(r._1)

    // (1) Resolved rows → one cluster per distinct tmdbId. Sort the ids so the
    // cluster sequence is order-independent.
    val resolved = group.filter(_._2.tmdbId.isDefined)
    val resolvedClusters: Seq[Cluster] =
      resolved.groupBy(_._2.tmdbId.get).toSeq.sortBy(_._1).map { case (_, rows) =>
        Cluster(refYear = rows.flatMap(_._2.tmdbYear).minOption, rows = rows)
      }

    val unresolved         = group.filter(_._2.tmdbId.isEmpty)
    val (yeared, yearless) = unresolved.partition(_._1.year.isDefined)

    // (2) Year-bearing unresolved rows adjacent (±1) to a resolved cluster's
    // tmdbYear attach to it (nearest by canonicalRank on tie). The rest fall
    // through to the greedy window in (3). Iterate in canonicalRank order so the
    // attachment is order-independent.
    val adjacent = scala.collection.mutable.LinkedHashMap.empty[Int, scala.collection.mutable.ListBuffer[Row]]
    val orphans  = scala.collection.mutable.ListBuffer.empty[Row]
    yeared.sortBy(rank).foreach { row =>
      val y = row._1.year.get
      resolvedClusters.zipWithIndex
        .filter { case (c, _) => c.refYear.exists(ry => math.abs(y - ry) <= 1) }
        .minByOption { case (c, idx) => (c.minRank, idx) } match {
        case Some((_, idx)) => adjacent.getOrElseUpdate(idx, scala.collection.mutable.ListBuffer.empty) += row
        case None           => orphans += row
      }
    }
    val resolvedWithAdjacent: Seq[Cluster] = resolvedClusters.zipWithIndex.map { case (c, idx) =>
      c.copy(rows = c.rows ++ adjacent.getOrElse(idx, Nil).toSeq)
    }

    // (3) Orphaned year-bearing rows → greedy 2-year windows from the lowest
    // distinct year. {y, y+1} absorbs every orphan at y or y+1; the next window
    // opens at the next distinct year > y+1.
    val orphanRows  = orphans.toSeq
    val orphanYears = orphanRows.map(_._1.year.get).distinct.sorted
    val windowClusters = scala.collection.mutable.ListBuffer.empty[Cluster]
    var remaining = orphanYears
    while (remaining.nonEmpty) {
      val lo    = remaining.head
      val inWin = orphanRows.filter(r => r._1.year.get == lo || r._1.year.get == lo + 1)
      windowClusters += Cluster(refYear = Some(lo), rows = inWin)
      remaining = remaining.dropWhile(_ <= lo + 1)
    }

    val seeded: Seq[Cluster] = resolvedWithAdjacent ++ windowClusters.toSeq

    // (4) Yearless+idless rows fold into the group's canonical cluster (smallest
    // canonicalRank). With no other cluster, each such row stands alone.
    val yearlessRows = yearless.toSeq
    val clusters: Seq[Cluster] =
      if (seeded.isEmpty) yearlessRows.map(r => Cluster(refYear = None, rows = Seq(r)))
      else {
        val canonicalIdx = seeded.zipWithIndex.minBy { case (c, idx) => (c.minRank, idx) }._2
        seeded.zipWithIndex.map { case (c, idx) =>
          if (idx == canonicalIdx) c.copy(rows = c.rows ++ yearlessRows) else c
        }
      }

    clusters.map(_.rows).filter(_.nonEmpty)
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
