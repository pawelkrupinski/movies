package controllers

import play.api.libs.json.Json
import play.api.mvc._
import services.tasks.{EnqueueResult, EnrichTaskKeys, TaskProducer}
import services.tasktypes.TaskType

/** `POST /tasks/run/:job`: the `/tasks` page's run buttons, each enqueueing a corpus-wide run. It only
 *  ever enqueues ([[TaskProducer]]); the page's reads are [[TasksController]]'s. */
class TaskRunController(cc: ControllerComponents, adminAction: AdminAction, queue: TaskProducer,
                        clock: java.time.Clock) extends AbstractController(cc) {

  /** The corpus-wide runs the `/tasks` page buttons trigger. Each maps a short
   *  job slug → the bulk TaskType the worker's `BulkRefreshHandler` consumes; the
   *  worker calls that source's existing `refreshAll`, or runs the
   *  identity projection pass (`settle`) on demand. */
  private val BulkJobs: Map[String, TaskType] = Map(
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
        val result = queue.enqueue(taskType, EnrichTaskKeys.bulkDedup(taskType), submittedAt = clock.instant())
        Ok(Json.obj(
          "job"       -> job,
          "taskType"  -> taskType.name,
          "enqueued"  -> (result == EnqueueResult.Added),
          "duplicate" -> (result == EnqueueResult.Duplicate)
        ))
    }
  }
}
