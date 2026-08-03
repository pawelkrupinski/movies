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
  /**
   * Fold one film's staging group.
   *
   * `candidateIds`, when given, names the `_id`s that MIGHT belong to the group so an
   * implementation can read those rows instead of every staged row. It is a hint and a
   * superset only: the group is still selected by `sanitize(row.title)`, so passing too
   * many costs a little I/O and passing the wrong ones changes nothing.
   *
   * It exists because the Mongo folder read the WHOLE staging collection inside every
   * fold transaction — fine at the handful of trickling newcomers production usually
   * holds, and 3,629 folds over 28,572 rows on a convergence leg, which is ~104 million
   * document decodes and 47 minutes of a 62-minute budget.
   */
  def foldGroup(cleanTitle: String, candidateIds: Option[Set[String]] = None): Seq[(CacheKey, MovieRecord)]
}

/**
 * Lock-guarded `StagingFolder` over the `MovieRepository` + `StagingRepository` traits — the
 * no-Mongo / test path (and the shape `MongoStagingFolder` mirrors with a real
 * transaction). Writing to `movieRepository` (not a `MovieCache`) is deliberate: the
 * worker's in-memory cache + read-model projector catch the folded row up via
 * the `movies` change stream, exactly as every other out-of-band `movies` write.
 */
class InMemoryStagingFolder(
  stagingRepository: StagingRepository,
  movieRepository:   MovieRepository,
  // The country whose title rules select and key the group. Defaults to Poland so
  // the many single-country test constructions are unchanged; the worker wires its
  // own country's instance.
  normalizer: services.movies.TitleNormalizer =
    services.movies.TitleNormalizer.deployment
) extends StagingFolder with Logging {
  private given services.movies.TitleNormalizer = normalizer
  private val lock = new AnyRef

  /**
   * Fold one film's staging group.
   *
   * `candidateIds`, when given, names the `_id`s that MIGHT belong to the group so an
   * implementation can read those rows instead of every staged row. It is a hint and a
   * superset only: the group is still selected by `sanitize(row.title)`, so passing too
   * many costs a little I/O and passing the wrong ones changes nothing.
   *
   * It exists because the Mongo folder read the WHOLE staging collection inside every
   * fold transaction — fine at the handful of trickling newcomers production usually
   * holds, and 3,629 folds over 28,572 rows on a convergence leg, which is ~104 million
   * document decodes and 47 minutes of a 62-minute budget.
   */
  def foldGroup(cleanTitle: String, candidateIds: Option[Set[String]] = None): Seq[(CacheKey, MovieRecord)] = lock.synchronized {
    val key         = normalizer.sanitize(cleanTitle)
    val stagingRows = StagingFold.selectStagingGroup(stagingRepository.findAll(), cleanTitle, normalizer)
    if (stagingRows.isEmpty) Seq.empty
    else {
      val all        = movieRepository.findAll()
      val groupRows  = all.filter(r => normalizer.sanitize(r.title) == key)
      // Also pull cross-title same-tmdbId siblings (any title) so a cross-language
      // duplicate already in `movies` merges at fold time (see reconcileTmdbIds).
      val ids        = StagingFold.reconcileTmdbIds(stagingRows, groupRows)
      val siblings   = if (ids.isEmpty) Seq.empty
        else all.filter(r => r.record.tmdbId.exists(ids.contains) && normalizer.sanitize(r.title) != key)
      val plan       = StagingFold.planGroup(stagingRows, groupRows ++ siblings, normalizer)
      plan.moviesUpserts.foreach { case (k, record) => movieRepository.upsert(k.cleanTitle, k.year, record) }
      plan.moviesDeletes.foreach(k => movieRepository.delete(k.cleanTitle, k.year))
      plan.stagingDeletes.foreach(stagingRepository.deleteRow)
      logger.info(s"Folded group '$cleanTitle': ${stagingRows.size} staging row(s) → ${plan.moviesUpserts.size} movies row(s).")
      plan.newPromotions
    }
  }
}
