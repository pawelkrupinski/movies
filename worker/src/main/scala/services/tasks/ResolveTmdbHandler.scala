package services.tasks

import play.api.Logging

/**
 * Handles a per-movie `ResolveTmdb` task (the `/debug` row "re-enrich" button)
 * by forcing a TMDB re-resolution of that one film via `reenrich` (wired to
 * `MovieService.reenrichTmdbSync`). That call publishes `TmdbResolved` /
 * `ImdbIdMissing`, so the downstream rating refreshers re-run for the row off
 * the existing event chain — that's the "followed by all the other
 * enrichments" hook, with no extra sequencing here.
 *
 * Takes the re-resolve as a function (like [[RatingHandler]]) rather than the
 * concrete `MovieService`, so it depends only on the narrow capability it uses.
 * Unlike a rating refresh it is NOT freshness-gated: the operator clicked it to
 * force a re-resolve, and the queue's dedup key already collapses repeat clicks.
 * Runs synchronously (one TMDB lookup, well within the worker's lease) so a
 * failure surfaces as a retry rather than being lost.
 */
class ResolveTmdbHandler(reenrich: (String, Option[Int]) => Unit) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.ResolveTmdb

  override def handle(task: Task): HandlerOutcome = {
    val title = EnrichTaskKeys.titleOf(task.payload)
    val year  = EnrichTaskKeys.yearOf(task.payload)
    if (title.isEmpty) {
      logger.warn(s"ResolveTmdb task ${task.dedupKey} has no title payload; dropping.")
      Done
    } else {
      reenrich(title, year)
      Done
    }
  }
}
