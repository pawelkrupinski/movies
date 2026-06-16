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
   *  else the lowest present KEY year, else yearless.
   *
   *  Deliberately NOT a fallback to slot `releaseYear`: a deferred-detail cinema
   *  scrapes a film YEARLESS (yearless key) and its detail later adds a production
   *  year to the SLOT only. If that provisional slot year promoted the row's key
   *  here, a row that folds ALONE (before its resolved siblings — the interleaved
   *  arrival the reaper folds one cinema at a time) would become a year-bearing
   *  movies row that the siblings can no longer absorb (Δ>window) — the order-
   *  dependent "Głos Hind Rajab" / Kino Amondo (slot 2022 vs resolved 2025) split.
   *  Keeping a yearless-key cluster yearless leaves it a rule-(4) row the settle
   *  folds into the resolved film, regardless of fold order. A row whose KEY
   *  carries a year (a non-deferred year-bearing scrape) still windows normally. */
  private[services] def clusterYear(cluster: Seq[(CacheKey, MovieRecord)]): Option[Int] =
    cluster.flatMap { case (_, e) => e.tmdbYear }.minOption
      .orElse(cluster.flatMap { case (k, _) => k.year }.minOption)

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
   *    2. Unresolved rows that HAVE a year attach to the NEAREST resolved cluster
   *       (by `|year − tmdbYear|`) within ±2 of its tmdbYear — wide enough to
   *       absorb a cinema's production-vs-release-year disagreement (a film shot
   *       in year Y but released Y+1/Y+2), but NOT so wide that a genuinely
   *       different same-title film still awaiting its own tmdbId gets swallowed.
   *       Ties (equidistant between two resolved remakes) break on the cluster's
   *       smallest `canonicalRank` then index. Rows beyond ±2 of every resolved
   *       cluster fall through to rule 3.
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

    // (2) Year-bearing unresolved rows attach to the NEAREST resolved cluster
    // within ±2 of its tmdbYear. The ±1 window this widened from was too tight:
    // Kino Muzeum reports "Zawieście czerwone latarnie" at its 1989 PRODUCTION
    // year while TMDB resolved the film to its 1991 release — two years off — so
    // the unresolved 1989 row orphaned into its own cluster and only merged when
    // its own TMDB lookup happened to land before this pass (a race that left the
    // corpus, and the rendered snapshot, nondeterministic). ±2 covers the usual
    // production-vs-release gap; staying bounded (not "nearest at any distance")
    // keeps a genuinely different same-title film still awaiting its tmdbId — a
    // remake decades apart — from being swallowed into the wrong cluster. Pick the
    // nearest by |year − tmdbYear|, ties on the cluster's smallest canonicalRank
    // then index, so the attachment is a pure function of the row set, not arrival
    // order. Iterate in canonicalRank order for the same reason.
    val adjacent = scala.collection.mutable.LinkedHashMap.empty[Int, scala.collection.mutable.ListBuffer[Row]]
    val orphans  = scala.collection.mutable.ListBuffer.empty[Row]
    yeared.sortBy(rank).foreach { row =>
      val year = row._1.year.get
      resolvedClusters.zipWithIndex
        .filter { case (c, _) => c.refYear.exists(ry => math.abs(year - ry) <= 2) }
        .minByOption { case (c, index) => (math.abs(year - c.refYear.get), c.minRank, index) } match {
        case Some((_, index)) => adjacent.getOrElseUpdate(index, scala.collection.mutable.ListBuffer.empty) += row
        case None           => orphans += row
      }
    }
    val resolvedWithAdjacent: Seq[Cluster] = resolvedClusters.zipWithIndex.map { case (c, index) =>
      c.copy(rows = c.rows ++ adjacent.getOrElse(index, Nil).toSeq)
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
        val canonicalIndex = seeded.zipWithIndex.minBy { case (c, index) => (c.minRank, index) }._2
        seeded.zipWithIndex.map { case (c, index) =>
          if (index == canonicalIndex) c.copy(rows = c.rows ++ yearlessRows) else c
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
    // Every reported variant: each CINEMA slot's derived title plus the rows'
    // current keys. Enrichment-source slots (Tmdb/Imdb/Filmweb) are excluded on
    // purpose: a row's identity spelling must come from what CINEMAS call it, not
    // from TMDB's title for the base film — otherwise a decorated variant (a dub
    // "Straszny film ukraiński dubbing", a programme "Kino Dostępne: …") whose
    // Tmdb slot carries the bare base title ("Straszny film") would canonicalise
    // to that base title and collapse onto the base row. Cinema titles keep the
    // variant distinct; `displayTitle` still derives a nice label separately.
    val slotKeys = cluster.flatMap { case (_, e) => e.cinemaData.values.flatMap(d => d.title.map(t => CacheKey(t, d.releaseYear))) }
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
