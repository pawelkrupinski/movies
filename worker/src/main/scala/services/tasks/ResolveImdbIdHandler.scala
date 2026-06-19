package services.tasks

import services.enrichment.ImdbIdResolver

/**
 * Worker task for recovering a resolved movies row's missing IMDb id (movies
 * path). The id was previously recovered INLINE off the `ImdbIdMissing` bus
 * event; this lets the merge-retrigger path re-kick it as a proper queue task
 * (deduped, retried, metered) when a merge changed the id-resolution inputs.
 *
 * `ImdbIdResolver.resolveSync` writes the id through the cache and publishes
 * `ImdbIdResolved`, so the downstream IMDb rating still follows off the event.
 */
class ResolveImdbIdHandler(resolver: ImdbIdResolver) extends TaskHandler {
  override val taskType: TaskType = TaskType.ResolveImdbId

  override def handle(task: Task): HandlerOutcome = {
    val title       = EnrichTaskKeys.titleOf(task.payload)
    val year        = EnrichTaskKeys.yearOf(task.payload)
    val searchTitle = EnrichTaskKeys.searchTitleOf(task.payload).getOrElse(title)
    resolver.resolveSync(title, year, searchTitle)
    HandlerOutcome.Done
  }
}
