package services.staging

import play.api.Logging
import services.movies.{MovieRepo, TitleNormalizer}

/**
 * Folds a concluded newcomer's per-cinema staging rows into the merged `movies`
 * collection and deletes them — the graduation step. The merge decision is
 * `StagingFold.plan` (the exact current movies merge); impls differ only in how
 * they apply it: `MongoStagingFolder` in a transaction (so a concurrent `movies`
 * write can't be lost), `InMemoryStagingFolder` under a lock (tests / no-Mongo).
 */
trait StagingFolder {
  /** Fold the `(sanitize(title), year)` VARIANT's per-cinema staging rows into a
   *  single `movies` row at that year, then delete them. Scoped to ONE year (not
   *  the whole sanitize group) so the production-year and release-year variants
   *  each land as their own `movies` row — the existing `canonicalizeBySanitize`
   *  ±1 settle then merges them deterministically, exactly as for the direct
   *  path. No-op when no staging rows match (already folded). */
  def foldFilm(cleanTitle: String, year: Option[Int]): Unit
}

/**
 * Lock-guarded `StagingFolder` over the `MovieRepo` + `StagingRepo` traits — the
 * no-Mongo / test path (and the shape `MongoStagingFolder` mirrors with a real
 * transaction). Writing to `movieRepo` (not a `MovieCache`) is deliberate: the
 * worker's in-memory cache + read-model projector catch the folded row up via
 * the `movies` change stream, exactly as every other out-of-band `movies` write.
 */
class InMemoryStagingFolder(stagingRepo: StagingRepo, movieRepo: MovieRepo) extends StagingFolder with Logging {
  private val lock = new AnyRef

  def foldFilm(cleanTitle: String, year: Option[Int]): Unit = lock.synchronized {
    val key         = TitleNormalizer.sanitize(cleanTitle)
    val stagingRows = stagingRepo.findAll().filter(r => TitleNormalizer.sanitize(r.title) == key && r.year == year)
    if (stagingRows.nonEmpty) {
      val moviesRows = movieRepo.findAll().filter(r => TitleNormalizer.sanitize(r.title) == key && r.year == year)
      val plan       = StagingFold.plan(stagingRows, moviesRows)
      plan.moviesUpserts.foreach { case (k, rec) => movieRepo.upsert(k.cleanTitle, k.year, rec) }
      plan.moviesDeletes.foreach(k => movieRepo.delete(k.cleanTitle, k.year))
      plan.stagingDeletes.foreach(r => stagingRepo.delete(r.cinema, r.title, r.year))
      logger.info(s"Folded '$cleanTitle' (${year.getOrElse("—")}): ${stagingRows.size} staging row(s) → " +
        s"${plan.moviesUpserts.size} movies row(s).")
    }
  }
}
