package controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.tasks.{QueueSnapshot, TaskQueue, TaskSummary}

/**
 * Operational view of the durable task queue at `/tasks`. The page renders a
 * static shell and polls `/tasks/data` every 5s; each poll is one bounded,
 * index-backed `monitor` read (counts + the oldest N active tasks), so the cost
 * scales with viewers-while-open, not with queue churn. A persistent change
 * stream was rejected for the same reason `/uptime` dropped one — it would push
 * every task transition 24/7 even with nobody watching.
 */
class TasksController(cc: ControllerComponents, adminAction: AdminAction, queue: TaskQueue) extends AbstractController(cc) {

  /** Cap on the live rows returned per poll — enough to see the head of a
   *  backed-up queue without an unbounded scan. */
  private val ActiveLimit = 300

  def index: Action[AnyContent] = adminAction {
    Ok(views.html.tasks())
  }

  def data: Action[AnyContent] = adminAction {
    val snap = queue.monitor(ActiveLimit)
    Ok(snapshotJson(snap))
  }

  private def snapshotJson(snap: QueueSnapshot): JsObject = Json.obj(
    "now"     -> System.currentTimeMillis(),
    "counts"  -> Json.toJson(snap.counts),
    "shown"   -> snap.active.size,
    "limit"   -> ActiveLimit,
    "active"  -> snap.active.map(taskJson)
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
