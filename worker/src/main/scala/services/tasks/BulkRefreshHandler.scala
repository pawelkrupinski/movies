package services.tasks

import play.api.Logging
import tools.DaemonExecutors

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future}

/**
 * Handles an operator-triggered, corpus-wide refresh task (the `/tasks` page
 * buttons) by running the supplied `run` — the matching existing `refreshAll`
 * / `retryUnresolvedTmdb` — and PERSISTING its outcome so the operator can see
 * what happened.
 *
 * The run is dispatched on a private background EC and the task is marked
 * `Done` immediately, rather than blocking the worker thread for the whole
 * (multi-minute) walk: a synchronous handler would hold its task lease past the
 * worker's processing timeout and get reaped + re-run mid-flight. An in-process
 * `running` flag makes an overlapping trigger a no-op (`Skipped`), so a corpus
 * refresh stays a singleton even though the queue task itself completes straight
 * away (and the queue's dedup key already collapses clicks while it's enqueued).
 *
 * Because `Done` deletes the task doc instantly, the run's result would otherwise
 * vanish the moment it started — leaving the operator staring at an empty queue,
 * the outcome visible only in the worker log. So on completion (success OR
 * failure, including the walk throwing) the handler writes a [[BulkTaskResult]]
 * keyed by its `taskType` to the shared [[BulkTaskResultStore]], which the web
 * reads for the `/tasks` page. A `Skipped` overlapping trigger writes nothing, so
 * it can't clobber the good result of the run already in flight.
 */
class BulkRefreshHandler(
  override val taskType: TaskType,
  label:                 String,
  run:                   () => BulkRefreshResult,
  resultStore:           BulkTaskResultStore,
  clock:                 () => Instant       = () => Instant.now(),
  executionContext:      ExecutionContext    = DaemonExecutors.virtualThreadEC("bulk-refresh")
) extends TaskHandler with Logging {
  import HandlerOutcome._

  private val running = new AtomicBoolean(false)

  override def handle(task: Task): HandlerOutcome =
    if (!running.compareAndSet(false, true)) {
      logger.info(s"Bulk $label refresh already running — skipping duplicate trigger.")
      Skipped
    } else {
      logger.info(s"Bulk $label refresh: starting operator-triggered run.")
      Future {
        val result =
          try BulkTaskResult.from(taskType, clock(), succeeded = true, run())
          catch {
            case e: Throwable =>
              logger.error(s"Bulk $label refresh failed", e)
              BulkTaskResult(taskType, clock(), succeeded = false, s"failed: ${e.getMessage}")
          } finally running.set(false)
        resultStore.record(result)
      }(using executionContext)
      Done
    }
}
