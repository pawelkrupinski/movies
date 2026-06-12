package services.tasks

import play.api.Logging
import tools.DaemonExecutors

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

/**
 * Handles an operator-triggered, corpus-wide refresh task (the `/tasks` page
 * buttons) by running the supplied `run` — the matching existing `refreshAll`
 * / `retryUnresolvedTmdb`.
 *
 * The run is dispatched on a private background EC and the task is marked
 * `Done` immediately, rather than blocking the worker thread for the whole
 * (multi-minute) walk: a synchronous handler would hold its task lease past the
 * worker's processing timeout and get reaped + re-run mid-flight. An in-process
 * `running` flag makes an overlapping trigger a no-op (`Skipped`), so a corpus
 * refresh stays a singleton even though the queue task itself completes straight
 * away (and the queue's dedup key already collapses clicks while it's enqueued).
 */
class BulkRefreshHandler(
  override val taskType: TaskType,
  label:                 String,
  run:                   () => Unit
) extends TaskHandler with Logging {
  import HandlerOutcome._

  private val running = new AtomicBoolean(false)
  private val ec      = DaemonExecutors.virtualThreadEC(s"bulk-$label")

  override def handle(task: Task): HandlerOutcome =
    if (!running.compareAndSet(false, true)) {
      logger.info(s"Bulk $label refresh already running — skipping duplicate trigger.")
      Skipped
    } else {
      logger.info(s"Bulk $label refresh: starting operator-triggered run.")
      Future {
        try run()
        catch { case e: Throwable => logger.error(s"Bulk $label refresh failed", e) }
        finally running.set(false)
      }(using ec)
      Done
    }
}
