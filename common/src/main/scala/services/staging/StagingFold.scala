package services.staging

import models.MovieRecord
import services.movies.{CacheKey, FilmCanonicalizer, StoredMovieRecord}

/**
 * The PURE decision half of folding ONE `(sanitize(title), year)` VARIANT's
 * staging rows into `movies`, shared by every `StagingFolder` impl.
 *
 * It unions the variant's per-cinema rows (+ any existing `movies` row at the
 * SAME exact `(sanitize, year)`) into one record per film (using
 * `clusterByFilm`/`canonical` only to split the rare same-year distinct-tmdbId
 * remake and pick the spelling), and **keeps the variant's OWN year** as the
 * key — it does NOT rekey to the TMDB year. That last point is load-bearing:
 * Cinema City reports a film at 2025 while everyone else reports 2026, so the
 * fold must leave `zawodowcy|2025` and `zawodowcy|2026` as two `movies` rows and
 * let the existing `canonicalizeBySanitize` ±1 settle merge them. Rekeying to the
 * TMDB year here made the 2025 fold overwrite the 2026 row (its movies lookup,
 * scoped to year 2025, never saw the 2026 row), silently dropping its cinemas.
 */
object StagingFold {

  /** What to write to bring `movies` to its folded state, and which staging rows
   *  were consumed (all of them). `moviesDeletes` are existing `movies` rows at
   *  this variant that lost the spelling tie-break (folded away); usually empty. */
  case class Plan(
    moviesUpserts:  Seq[(CacheKey, MovieRecord)],
    moviesDeletes:  Seq[CacheKey],
    stagingDeletes: Seq[StagingRecord]
  )

  /** `stagingRows` all share one `(sanitize, year)` variant (the folder scopes
   *  them); `moviesRows` are any existing `movies` rows at that exact variant. */
  def plan(stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord]): Plan = {
    val variantYear  = stagingRows.headOption.map(_.year).getOrElse(None)
    val stagingPairs = stagingRows.map(r => CacheKey(r.title, r.year) -> r.record)
    val moviesPairs  = moviesRows.map(r => CacheKey(r.title, r.year) -> r.record)
    // Every row here is the SAME film-variant (one sanitize + one year), just one
    // per cinema, so the only split that can be needed is a genuine distinct-tmdbId
    // remake — group by tmdbId (all unresolved rows share the `None` group and so
    // merge into ONE row). Do NOT use `clusterByFilm` here: it assumes the cache's
    // one-row-per-key invariant and would turn N separate YEARLESS cinema rows into
    // N singleton clusters (rule 4), which then collapse onto the same `(sanitize,
    // None)` key and clobber each other — dropping every cinema but one (the
    // all-yearless events: Maraton Horrorów, Filmowe Poranki). `canonical` then
    // picks the cinema spelling + unions; force the key year to the variant's own.
    val upserts = (stagingPairs ++ moviesPairs).groupBy(_._2.tmdbId).valuesIterator.map { cluster =>
      val (canonKey, merged) = FilmCanonicalizer.canonical(cluster)
      CacheKey(canonKey.cleanTitle, variantYear) -> merged
    }.toSeq
    val canonicalKeys = upserts.map(_._1).toSet
    val moviesDeletes = moviesPairs.map(_._1).distinct.filterNot(canonicalKeys.contains)
    Plan(upserts, moviesDeletes, stagingRows)
  }
}
