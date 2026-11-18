package services.refresh

import play.api.Logging
import services.tasks.{BulkTaskResult, BulkTaskResultStore, HandlerOutcome, RefreshableSources, Task, TaskHandler}
import services.tasktypes.TaskType

import java.time.Clock

/**
 * The queue's handler of [[TaskType.RefreshSource]]: the named source refreshed for the task's keys on the worker's
 * thread, its outcome recorded for the `/tasks` page (`lastResults`, under `RefreshSource`). A source this worker does
 * not wire is a failed result, the task done: asking again cannot help. A refresh that throws is recorded and goes back
 * to the queue's retry.
 */
final class RefreshSourceHandler(sources: RefreshableSources, results: BulkTaskResultStore, clock: Clock) extends TaskHandler with Logging {
  val taskType: TaskType = TaskType.RefreshSource

  def handle(task: Task): HandlerOutcome = {
    val name = RefreshableSources.sourceOf(task)
    val keys = RefreshableSources.keysOf(task)
    sources.byName(name) match {
      case None =>
        record(succeeded = false, s"$name: no such source on this worker (${sources.all.map(_.name).mkString(", ")})")
        HandlerOutcome.Done
      case Some(source) =>
        val result =
          tools.AnyThrowable.rethrownAfter(source.refresh(keys))(e => record(succeeded = false, s"$name: failed: ${e.getMessage}"))
        logger.info(s"refresh $name for ${keys.size} keys: ${result.message}")
        results.record(BulkTaskResult.from(taskType, clock.instant(), succeeded = true, result.copy(message = s"$name: ${result.message}")))
        HandlerOutcome.Done
    }
  }

  private def record(succeeded: Boolean, message: String): Unit =
    results.record(BulkTaskResult(taskType, clock.instant(), succeeded, message))
}
