package scripts

import services.movies.StoredMovieRecord
import services.tasks.{EnqueueResult, EnrichTaskKeys, TaskQueue, TaskType}

/**
 * Queues the forced TMDB re-resolve that both [[CountryForceResolve]] (a whole
 * country) and [[ReresolveSelfLockedRows]] (a named handful) run.
 *
 * `force = true` is the one sanctioned bypass of `MovieService.needsTmdbResolution`,
 * which otherwise never re-queries TMDB for a row that already holds a `tmdbId`. On
 * the forced path `MovieService.resolveTmdbOnce` first calls `forgetResolutions` +
 * `resetToScrapedData`, so the row is stripped back to its cinema slots and re-keyed
 * onto the SCRAPED year before the lookup — which is what lets a self-locked row
 * escape the wrong film it is keyed under, rather than re-confirming it.
 *
 * The dedup key makes this idempotent, so a re-run costs nothing.
 */
object ForceResolveEnqueue {

  /** How the queue answered, counted. */
  case class Enqueued(added: Int, duplicate: Int) {
    def describe: String = s"$added enqueued, $duplicate already queued (dedup)"
  }

  def all(queue: TaskQueue, rows: Seq[StoredMovieRecord]): Enqueued =
    rows.foldLeft(Enqueued(0, 0)) { (counts, r) =>
      queue.enqueue(
        TaskType.ResolveTmdb,
        EnrichTaskKeys.resolveTmdbDedup(r.title, r.year),
        EnrichTaskKeys.resolveTmdbPayload(r.title, r.year, force = true)) match {
        case EnqueueResult.Added     => counts.copy(added = counts.added + 1)
        case EnqueueResult.Duplicate => counts.copy(duplicate = counts.duplicate + 1)
      }
    }
}
