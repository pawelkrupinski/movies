package services.tasks

import play.api.Logging

/**
 * Handles a per-movie `ResolveTmdb` task by resolving that one film's TMDB id
 * via `resolve` (wired to `MovieService.resolveTmdbOnce`). Both the normal
 * enrichment flow (each scraped film whose `tmdbId` is still empty) and the
 * operator `/debug` "re-enrich" button enqueue this task — the latter sets the
 * `force` flag so it re-resolves even an already-resolved row.
 *
 * `resolve` writes the resolved row (and publishes `ImdbIdMissing` on a hit with
 * no IMDb cross-reference, for id recovery); the `EnrichmentReaper` then enqueues
 * the row's rating refreshes on its next pass.
 * It returns `false` on a TRANSIENT failure (rate-limit / network blip) — we
 * map that to `Reschedule` so the queue re-claims the task (with backoff) until
 * the row reaches a definitive TMDB state; a hit or a persisted `tmdbNoMatch`
 * returns `true` → `Done`. This is the infinite-transient-retry policy the old
 * inline `scheduleTmdbRetry` hand-rolled, now owned by the queue.
 *
 * Takes the resolve as a function (like [[RatingHandler]]) rather than the
 * concrete `MovieService`, so it depends only on the narrow capability it uses.
 * Runs synchronously (one TMDB lookup, well within the worker's lease).
 */
class ResolveTmdbHandler(
  resolve: (String, Option[Int], Option[String], Option[String], Boolean) => Boolean
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ResolveTmdb

  override def handle(task: Task): HandlerOutcome = {
    val title = EnrichTaskKeys.titleOf(task.payload)
    val year  = EnrichTaskKeys.yearOf(task.payload)
    if (title.isEmpty) {
      logger.warn(s"ResolveTmdb task ${task.dedupKey} has no title payload; dropping.")
      Done
    } else if (resolve(
                 title, year,
                 EnrichTaskKeys.originalTitleOf(task.payload),
                 EnrichTaskKeys.directorOf(task.payload),
                 EnrichTaskKeys.forceOf(task.payload))) {
      Done
    } else {
      Reschedule(Some(s"TMDB resolve for '$title' (${year.getOrElse("?")}) failed transiently; retrying"))
    }
  }
}
