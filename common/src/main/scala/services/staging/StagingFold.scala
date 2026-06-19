package services.staging

import models.MovieRecord
import services.movies.{CacheKey, FilmCanonicalizer, MovieRecordMerge, StoredMovieRecord}

/**
 * The PURE decision half of folding a newcomer's staging rows into `movies`,
 * shared by every `StagingFolder` impl.
 *
 * It folds the WHOLE `sanitize(title)` GROUP — every year-variant at once — and
 * runs the SAME `clusterByFilm`/`canonical` collapse the cache's
 * `canonicalizeBySanitize` runs, so the folded `movies` state is already the
 * settled steady state: ±1-year variants merged into one row, each resolved
 * cluster re-keyed to its TMDB year, the canonical spelling chosen. This replaces
 * the old fold-per-year + periodic-settle split — Cinema City reports a film at
 * `zawodowcy|2025` while everyone else reports `zawodowcy|2026`, and both variants
 * now collapse HERE instead of landing as two `movies` rows for a separate settle
 * pass to merge later.
 */
object StagingFold {

  /** What to write to bring `movies` to its folded+settled state, and which
   *  staging rows were consumed (all of them). `moviesDeletes` are existing
   *  `movies` rows in the group whose key the collapse retired (re-keyed to a
   *  TMDB year, or merged into a sibling). `newPromotions` is the subset of
   *  `moviesUpserts` that is a BRAND-NEW film — a cluster no pre-existing
   *  `movies` row joined — so the folder can schedule its first-time rating
   *  enrichment; a fold that merely merges into an existing `movies` row is not
   *  listed (that row already carries its ratings). */
  case class Plan(
    moviesUpserts:  Seq[(CacheKey, MovieRecord)],
    moviesDeletes:  Seq[CacheKey],
    stagingDeletes: Seq[StagingRecord],
    newPromotions:  Seq[(CacheKey, MovieRecord)]
  )

  /** `stagingRows` are every per-cinema row of ONE `sanitize(title)` group (all
   *  its year-variants); `moviesRows` are the existing `movies` rows in that same
   *  group. */
  def planGroup(stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord]): Plan = {
    // Union the per-cinema staging rows to ONE row per (sanitize, year) key FIRST,
    // restoring the one-row-per-key invariant `clusterByFilm` assumes. Without it,
    // N separate YEARLESS cinema rows would each become a rule-4 singleton cluster
    // that then collapses onto the same `(sanitize, None)` key and clobbers all but
    // one — dropping every cinema's slot but one for the all-yearless events
    // (Maraton Horrorów, Filmowe Poranki).
    val stagingByKey = stagingRows.groupBy(r => CacheKey(r.title, r.year)).toSeq.map {
      case (key, rows) => key -> MovieRecordMerge.unionAll(rows.map(_.record))
    }
    val moviesByKey = moviesRows.map(r => CacheKey(r.title, r.year) -> r.record)
    val moviesKeys  = moviesByKey.map(_._1).toSet
    // Union ACROSS the staging↔movies boundary too: `CacheKey` is case-insensitive,
    // so a staging "iron maiden" row and an already-promoted movies "Iron Maiden"
    // row share a key but sit in SEPARATE entries above. Left un-unioned they each
    // become a rule-4 singleton cluster that re-collapses onto the one
    // (sanitize, None) key and clobbers a cinema's slot — the same loss the
    // all-staging union guards against, across the boundary (a yearless event whose
    // first cinema promoted before the rest arrived). Merge by key, canonicalRank
    // order so the union base is deterministic.
    val byKey = (stagingByKey ++ moviesByKey).groupBy(_._1).toSeq.map { case (_, entries) =>
      val sorted = entries.sortBy { case (k, _) => FilmCanonicalizer.canonicalRank(k) }
      sorted.head._1 -> MovieRecordMerge.unionAll(sorted.map(_._2))
    }
    val planned = FilmCanonicalizer.clusterByFilm(byKey).map { cluster =>
      val (canonKey, merged) = FilmCanonicalizer.canonical(cluster)
      // A cluster is a brand-new promotion iff no existing `movies` row joined it
      // (all members came from staging) — a merge into an existing row, or a
      // re-key of one, does NOT count: that row already owns its ratings.
      val isNewFilm = !cluster.exists { case (k, _) => moviesKeys.contains(k) }
      // Drop the staging-only `searchTitle`: a `movies` row queries external
      // services off its canonical title, so it never carries the (order-pinned)
      // staging search title — movies stay a deterministic function of the corpus.
      (canonKey -> merged.copy(searchTitle = None), isNewFilm)
    }
    val upserts       = planned.map(_._1)
    val newPromotions = planned.collect { case (upsert, true) => upsert }
    val canonicalKeys = upserts.map(_._1).toSet
    val moviesDeletes = moviesByKey.map(_._1).distinct.filterNot(canonicalKeys.contains)
    Plan(upserts, moviesDeletes, stagingRows, newPromotions)
  }
}
