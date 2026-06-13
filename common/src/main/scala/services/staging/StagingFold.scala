package services.staging

import models.MovieRecord
import services.movies.{CacheKey, FilmCanonicalizer, StoredMovieRecord}

/**
 * The PURE decision half of folding a newcomer's staging rows into `movies`,
 * shared by every `StagingFolder` impl so the merge is computed ONE way.
 *
 * It reuses the EXACT current movies merge — `FilmCanonicalizer.clusterByFilm`
 * (so distinct-tmdbId remakes that share a title stay separate films) then
 * `FilmCanonicalizer.canonical` per cluster — over the combined staging rows and
 * any existing `movies` rows for the same `sanitize(title)`. A newcomer normally
 * has no `movies` sibling (routing only diverts unknown films), so the cluster is
 * usually just the staging rows; the `moviesRows` input covers the rare race
 * where one already exists.
 */
object StagingFold {

  /** What to write to bring `movies` to its folded state, and which staging rows
   *  were consumed (all of them). `moviesDeletes` are existing `movies` rows that
   *  lost to a different canonical key (folded away); usually empty. */
  case class Plan(
    moviesUpserts:  Seq[(CacheKey, MovieRecord)],
    moviesDeletes:  Seq[CacheKey],
    stagingDeletes: Seq[StagingRecord]
  )

  def plan(stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord]): Plan = {
    val stagingPairs = stagingRows.map(r => CacheKey(r.title, r.year) -> r.record)
    val moviesPairs  = moviesRows.map(r => CacheKey(r.title, r.year) -> r.record)
    val upserts      = FilmCanonicalizer.clusterByFilm(stagingPairs ++ moviesPairs).map(FilmCanonicalizer.canonical)
    val canonicalKeys = upserts.map(_._1).toSet
    val moviesDeletes = moviesPairs.map(_._1).distinct.filterNot(canonicalKeys.contains)
    Plan(upserts, moviesDeletes, stagingRows)
  }
}
