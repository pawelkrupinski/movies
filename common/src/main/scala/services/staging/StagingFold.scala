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
   *  TMDB year, or merged into a sibling). */
  case class Plan(
    moviesUpserts:  Seq[(CacheKey, MovieRecord)],
    moviesDeletes:  Seq[CacheKey],
    stagingDeletes: Seq[StagingRecord]
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
    val upserts = FilmCanonicalizer.clusterByFilm(stagingByKey ++ moviesByKey).map { cluster =>
      val (canonKey, merged) = FilmCanonicalizer.canonical(cluster)
      // Drop the staging-only `searchTitle`: a `movies` row queries external
      // services off its canonical title, so it never carries the (order-pinned)
      // staging search title — movies stay a deterministic function of the corpus.
      canonKey -> merged.copy(searchTitle = None)
    }
    val canonicalKeys = upserts.map(_._1).toSet
    val moviesDeletes = moviesByKey.map(_._1).distinct.filterNot(canonicalKeys.contains)
    Plan(upserts, moviesDeletes, stagingRows)
  }
}
