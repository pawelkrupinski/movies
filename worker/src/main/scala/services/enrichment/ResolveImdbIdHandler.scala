package services.enrichment

import services.tasks.{EnrichTaskKeys, HandlerOutcome, Task, TaskHandler}
import services.tasktypes.TaskType

/**
 * Worker task for recovering a resolved movies row's missing IMDb id (movies
 * path). The id was previously recovered INLINE off the `ImdbIdMissing` bus
 * event; this lets the merge-retrigger path re-kick it as a proper queue task
 * (deduped, retried, metered) when a merge changed the id-resolution inputs.
 *
 * `ImdbIdRecovery.resolve` writes the id through the cache; the
 * `EnrichmentReaper` then enqueues the now-eligible IMDb rating on its next pass.
 */
class ResolveImdbIdHandler(recovery: ImdbIdRecovery) extends TaskHandler {
  override val taskType: TaskType = TaskType.ResolveImdbId

  override def handle(task: Task): HandlerOutcome = {
    val title       = EnrichTaskKeys.titleOf(task.payload)
    val year        = EnrichTaskKeys.yearOf(task.payload)
    val searchTitle = EnrichTaskKeys.searchTitleOf(task.payload).getOrElse(title)
    recovery.resolve(title, year, searchTitle)
    HandlerOutcome.Done
  }
}
