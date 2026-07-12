package controllers

import play.api.libs.json.{JsObject, Json}
import play.api.mvc._
import services.tasks.{BulkTaskResult, BulkTaskResultStore, EnqueueResult, EnrichTaskKeys, QueueSnapshot, TaskQueue, TaskSummary, TaskType}

/**
 * Operational view of the durable task queue at `/tasks`. The page renders a
 * static shell and polls `/tasks/data` every 5s; each poll is one bounded,
 * index-backed `monitor` read (counts + the oldest N active tasks), so the cost
 * scales with viewers-while-open, not with queue churn. A persistent change
 * stream was rejected for the same reason `/uptime` dropped one — it would push
 * every task transition 24/7 even with nobody watching.
 */
class TasksController(cc: ControllerComponents, adminAction: AdminAction, queue: TaskQueue,
                      bulkResults: BulkTaskResultStore) extends AbstractController(cc) {

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

  /** The corpus-wide runs the `/tasks` page buttons trigger. Each maps a short
   *  job slug → the bulk TaskType the worker's `BulkRefreshHandler` consumes; the
   *  worker calls that source's existing `refreshAll` / `retryUnresolvedTmdb`, or
   *  `settle` (consolidate same-film rows, the SettleReaper's job) on demand. */
  private val BulkJobs: Map[String, TaskType] = Map(
    "tmdb"       -> TaskType.RefreshAllTmdb,
    "imdb"       -> TaskType.RefreshAllImdb,
    "filmweb"    -> TaskType.RefreshAllFilmweb,
    "metacritic" -> TaskType.RefreshAllMetacritic,
    "rt"         -> TaskType.RefreshAllRt,
    "settle"     -> TaskType.SettleNow
  )

  /** Enqueue a corpus-wide refresh run. Idempotent: the constant per-type dedup
   *  key means a second click while one is queued returns `duplicate` rather
   *  than stacking a second run. The page's poll then surfaces the task. */
  def run(job: String): Action[AnyContent] = adminAction {
    BulkJobs.get(job) match {
      case None => BadRequest(Json.obj("error" -> s"unknown job '$job'"))
      case Some(taskType) =>
        val result = queue.enqueue(taskType, EnrichTaskKeys.bulkDedup(taskType))
        Ok(Json.obj(
          "job"       -> job,
          "taskType"  -> taskType.name,
          "enqueued"  -> (result == EnqueueResult.Added),
          "duplicate" -> (result == EnqueueResult.Duplicate)
        ))
    }
  }

  private def snapshotJson(snap: QueueSnapshot): JsObject = Json.obj(
    "now"     -> System.currentTimeMillis(),
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
