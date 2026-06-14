package services.staging

import models.MovieRecord
import play.api.Logging
import services.movies.{CacheKey, MovieRepository, TitleNormalizer}

/**
 * Folds a concluded newcomer's per-cinema staging rows into the merged `movies`
 * collection and deletes them — the graduation step. The merge decision is
 * `StagingFold.planGroup` (the exact `canonicalizeBySanitize` settle, applied to
 * the staging + movies rows); impls differ only in how they apply it:
 * `MongoStagingFolder` in a transaction (so a concurrent `movies` write can't be
 * lost), `InMemoryStagingFolder` under a lock (tests / no-Mongo).
 */
trait StagingFolder {
  /** Fold the WHOLE `sanitize(title)` GROUP's per-cinema staging rows (every
   *  year-variant) into `movies` and delete them, settling as it goes:
   *  `StagingFold.planGroup` runs the same `clusterByFilm`/`canonical` collapse
   *  the cache's `canonicalizeBySanitize` does, so the production-year and
   *  release-year variants merge into ONE deterministically-keyed `movies` row
   *  rather than waiting for a separate settle pass. Idempotent and group-scoped,
   *  so re-firing as each year concludes converges. No-op when no staging rows
   *  match (already folded).
   *
   *  Returns the BRAND-NEW films this fold introduced (`Plan.newPromotions`):
   *  upserted `movies` rows that no pre-existing row merged into, so the caller
   *  can schedule their first-time rating enrichment. Empty when nothing folded
   *  or every row merged into an existing movie. */
  def foldGroup(cleanTitle: String): Seq[(CacheKey, MovieRecord)]
}

/**
 * Lock-guarded `StagingFolder` over the `MovieRepository` + `StagingRepository` traits — the
 * no-Mongo / test path (and the shape `MongoStagingFolder` mirrors with a real
 * transaction). Writing to `movieRepository` (not a `MovieCache`) is deliberate: the
 * worker's in-memory cache + read-model projector catch the folded row up via
 * the `movies` change stream, exactly as every other out-of-band `movies` write.
 */
class InMemoryStagingFolder(stagingRepository: StagingRepository, movieRepository: MovieRepository) extends StagingFolder with Logging {
  private val lock = new AnyRef

  def foldGroup(cleanTitle: String): Seq[(CacheKey, MovieRecord)] = lock.synchronized {
    val key         = TitleNormalizer.sanitize(cleanTitle)
    val stagingRows = stagingRepository.findAll().filter(r => TitleNormalizer.sanitize(r.title) == key)
    if (stagingRows.isEmpty) Seq.empty
    else {
      val moviesRows = movieRepository.findAll().filter(r => TitleNormalizer.sanitize(r.title) == key)
      val plan       = StagingFold.planGroup(stagingRows, moviesRows)
      plan.moviesUpserts.foreach { case (k, record) => movieRepository.upsert(k.cleanTitle, k.year, record) }
      plan.moviesDeletes.foreach(k => movieRepository.delete(k.cleanTitle, k.year))
      plan.stagingDeletes.foreach(r => stagingRepository.delete(r.cinema, r.title, r.year))
      logger.info(s"Folded group '$cleanTitle': ${stagingRows.size} staging row(s) → ${plan.moviesUpserts.size} movies row(s).")
      plan.newPromotions
    }
  }
}
