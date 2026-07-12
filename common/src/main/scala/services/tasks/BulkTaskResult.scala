package services.tasks

import java.time.Instant

/**
 * Summary a full-corpus `refreshAll` walk produces — the counts each `*Ratings`
 * walk already computes and logs, lifted from a log-only side effect to a return
 * value so the operator-triggered [[BulkRefreshHandler]] can persist them. The
 * counts are `Option` because the generic bulk jobs (settle, TMDB re-enrich,
 * OMDb) have no per-source changed/failed tally — they carry only a `message`.
 */
case class BulkRefreshResult(
  message:    String,
  walked:     Option[Int] = None,
  changed:    Option[Int] = None,
  discovered: Option[Int] = None,
  failed:     Option[Int] = None
)

object BulkRefreshResult {
  /** A message-only result — for a bulk job with no per-source count tally. */
  def message(msg: String): BulkRefreshResult = BulkRefreshResult(msg)

  /** A counted result — the shape the four `*Ratings` corpus walks return. */
  def counts(walked: Int, changed: Int, discovered: Int, failed: Int, message: String): BulkRefreshResult =
    BulkRefreshResult(message, Some(walked), Some(changed), Some(discovered), Some(failed))
}

/**
 * The persisted last outcome of one operator-triggered corpus-wide bulk job (a
 * `/tasks` page button). The worker's [[BulkRefreshHandler]] writes one per
 * `taskType` on completion — crucially AFTER the queue has deleted the task doc,
 * so unlike the ephemeral task itself this survives and the `/tasks` page can
 * show "IMDb refresh · 4 changed · 2m ago" long after the walk finished.
 */
case class BulkTaskResult(
  taskType:   TaskType,
  ranAt:      Instant,
  succeeded:  Boolean,
  message:    String,
  walked:     Option[Int] = None,
  changed:    Option[Int] = None,
  discovered: Option[Int] = None,
  failed:     Option[Int] = None
)

object BulkTaskResult {
  /** Fold a walk's [[BulkRefreshResult]] into the persisted record for `taskType`. */
  def from(taskType: TaskType, ranAt: Instant, succeeded: Boolean, result: BulkRefreshResult): BulkTaskResult =
    BulkTaskResult(taskType, ranAt, succeeded, result.message,
      result.walked, result.changed, result.discovered, result.failed)
}

/**
 * Where each bulk job's last outcome is stored so it outlives the task doc. The
 * worker WRITES (`record`) on completion; the web READS (`latest`) to render the
 * `/tasks` page — two separate Fly apps sharing one Mongo, exactly like the
 * `tasks` collection itself ([[MongoTaskQueue]]). Two implementations share this
 * contract: [[MongoBulkTaskResultStore]] (durable, cross-process) and
 * [[InMemoryBulkTaskResultStore]] (tests / Mongo-less dev).
 */
trait BulkTaskResultStore {
  /** Record (overwriting any prior) the last outcome for `result.taskType`. */
  def record(result: BulkTaskResult): Unit

  /** The last outcome of every bulk job that has run at least once, by type. */
  def latest(): Map[TaskType, BulkTaskResult]
}

/** In-memory [[BulkTaskResultStore]] — one entry per taskType, last write wins.
 *  Used by tests and Mongo-less dev; no business logic of its own (the handler
 *  above the store decides what a result is), so the real/fake split is trivial. */
class InMemoryBulkTaskResultStore extends BulkTaskResultStore {
  private val results = new java.util.concurrent.ConcurrentHashMap[TaskType, BulkTaskResult]()

  override def record(result: BulkTaskResult): Unit = { results.put(result.taskType, result); () }

  override def latest(): Map[TaskType, BulkTaskResult] = {
    import scala.jdk.CollectionConverters._
    results.asScala.toMap
  }
}
