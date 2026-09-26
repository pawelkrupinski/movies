package services.identity

import services.tasks.{HandlerOutcome, Task, TaskHandler, TaskType}

/**
 * A cut-over country's task handlers (docs/design/identity-resolver.md §8, phase 5): the old
 * identity path's task types — the deferred venue detail that fed the TMDB resolve, the per-film
 * TMDB resolve and its bulk re-run, and the staging chain — are completed without running.
 * Their work is the projection's now (the resolver reads venue details and TMDB as lookups), and a
 * task of theirs still queued from before the cutover must not write an identity decision the
 * projection did not make. Completed rather than unhandled: the worker returns an unhandled task
 * to the queue at once, and would claim it again for ever.
 */
object CutoverTaskHandlers {

  val OldPathTypes: Set[TaskType] = Set(TaskType.EnrichDetails, TaskType.ResolveTmdb, TaskType.RefreshAllTmdb,
    TaskType.StagingDetail, TaskType.StagingResolveTmdb, TaskType.StagingResolveImdbId, TaskType.StagingFold)

  def of(handlers: Seq[TaskHandler]): Seq[TaskHandler] =
    handlers.map(h => if (OldPathTypes(h.taskType)) new Retired(h.taskType) else h)

  /** Completes every task of `taskType` unrun. */
  final class Retired(val taskType: TaskType) extends TaskHandler {
    def handle(task: Task): HandlerOutcome = HandlerOutcome.Skipped
  }
}
