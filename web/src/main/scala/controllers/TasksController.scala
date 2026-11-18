package controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.tasks.{BulkTaskResult, BulkTaskResultStore, QueueSnapshot, TaskQueueMonitor, TaskSummary}

/**
 * Operational view of the durable task queue at `/tasks`. The page renders a
 * static shell and polls `/tasks/data` every 5s; each poll is one bounded,
 * index-backed `monitor` read (counts + N active tasks, worked-on first so the
 * rows a worker is holding lead the table rather than scattering through the
 * waiting backlog — see `TaskState.activeByPriority`), so the cost
 * scales with viewers-while-open, not with queue churn. A persistent change
 * stream was rejected for the same reason `/uptime` dropped one — it would push
 * every task transition 24/7 even with nobody watching. The page's run buttons
 * post to [[TaskRunController]].
 */
class TasksController(cc: ControllerComponents, adminAction: AdminAction, queue: TaskQueueMonitor,
                      bulkResults: BulkTaskResultStore,
                      // The deployment's country — the one the page's country switch marks current.
                      country: models.Country,
                      // The server time the page ticks task ages from.
                      clock: java.time.Clock) extends AbstractController(cc) {

  /** Cap on the live rows returned per poll — enough to see the head of a
   *  backed-up queue without an unbounded scan. */
  private val ActiveLimit = 300

  def index: Action[AnyContent] = adminAction {
    Ok(views.html.tasks(country))
  }

  def data: Action[AnyContent] = adminAction {
    val snap = queue.monitor(ActiveLimit)
    Ok(snapshotJson(snap))
  }

  private def snapshotJson(snap: QueueSnapshot): JsObject = Json.obj(
    "now"     -> clock.millis(),
    "counts"  -> Json.toJson(snap.counts),
    "shown"   -> snap.active.size,
    "limit"   -> ActiveLimit,
    "active"  -> snap.active.map(taskJson),
    // Each bulk job's LAST outcome, keyed by TaskType name so the page can show it
    // under the matching Run button long after the task doc itself was deleted.
    "lastResults" -> JsObject(bulkResults.latest().map { case (taskType, result) =>
      taskType.name -> resultJson(result)
    })
  )

  private def resultJson(r: BulkTaskResult): JsObject = Json.obj(
    "ranAt"      -> r.ranAt.toEpochMilli,
    "succeeded"  -> r.succeeded,
    "message"    -> r.message,
    "walked"     -> r.walked,
    "changed"    -> r.changed,
    "discovered" -> r.discovered,
    "failed"     -> r.failed
  )

  private def taskJson(t: TaskSummary): JsObject = Json.obj(
    "id"             -> t.id,
    "taskType"       -> t.taskType,
    "dedupKey"       -> t.dedupKey,
    "state"          -> t.state,
    "submittedAt"    -> t.submittedAt.toEpochMilli,
    "attempts"       -> t.attempts,
    "workerId"       -> t.workerId,
    "leaseExpiresAt" -> t.leaseExpiresAt.map(_.toEpochMilli),
    "lastError"      -> t.lastError
  )
}
