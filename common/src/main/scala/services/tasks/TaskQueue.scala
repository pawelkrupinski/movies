package services.tasks

import scala.concurrent.duration.FiniteDuration
import java.time.Instant

/** The kinds of deferred work the queue carries. Extensible — add a case and a
 *  handler. The `name` is the on-disk discriminator, so keep it stable. */
sealed trait TaskType { def name: String }

object TaskType {
  case object ScrapeCinema  extends TaskType { val name = "ScrapeCinema"  }
  case object EnrichDetails extends TaskType { val name = "EnrichDetails" }
  case object ResolveTmdb   extends TaskType { val name = "ResolveTmdb"   }
  case object ResolveImdbId extends TaskType { val name = "ResolveImdbId" }
  case object ImdbRating    extends TaskType { val name = "ImdbRating"    }
  case object FilmwebRating extends TaskType { val name = "FilmwebRating" }
  case object RtRating      extends TaskType { val name = "RtRating"      }
  case object McRating      extends TaskType { val name = "McRating"      }

  // Operator-triggered, corpus-wide refresh runs (the `/tasks` page buttons).
  // Each is a global singleton: a constant dedup key collapses repeat clicks
  // while one is active, and the worker handler runs the corresponding existing
  // `refreshAll` / `retryUnresolvedTmdb`.
  case object RefreshAllTmdb       extends TaskType { val name = "RefreshAllTmdb"       }
  case object RefreshAllImdb       extends TaskType { val name = "RefreshAllImdb"       }
  case object RefreshAllFilmweb    extends TaskType { val name = "RefreshAllFilmweb"    }
  case object RefreshAllMetacritic extends TaskType { val name = "RefreshAllMetacritic" }
  case object RefreshAllRt         extends TaskType { val name = "RefreshAllRt"         }

  // Staging incubation: a newcomer in `pending_movies` walks these the same way
  // the direct path walks EnrichDetails → ResolveTmdb → ResolveImdbId, but pointed
  // at staging rows and ending in a fold into `movies`. Each step is its own task
  // so it retries/backs off independently; `StagingReaper` chains them (off the
  // generic `TaskFinished` event) and is the periodic backstop. See StagingSteps.
  case object StagingDetail        extends TaskType { val name = "StagingDetail"        }
  case object StagingResolveTmdb   extends TaskType { val name = "StagingResolveTmdb"   }
  case object StagingResolveImdbId extends TaskType { val name = "StagingResolveImdbId" }
  case object StagingFold          extends TaskType { val name = "StagingFold"          }

  val all: Seq[TaskType] =
    Seq(ScrapeCinema, EnrichDetails, ResolveTmdb, ResolveImdbId, ImdbRating, FilmwebRating, RtRating, McRating,
        RefreshAllTmdb, RefreshAllImdb, RefreshAllFilmweb, RefreshAllMetacritic, RefreshAllRt,
        StagingDetail, StagingResolveTmdb, StagingResolveImdbId, StagingFold)

  def byName(s: String): Option[TaskType] = all.find(_.name == s)
}

/** A claimed unit of work handed to a handler. The queue's bookkeeping
 *  (state, lease, worker) is deliberately not exposed — the handler only needs
 *  what to do (`taskType` + `payload`) and `attempts` for any backoff choice. */
case class Task(
  id:       String,
  taskType: TaskType,
  dedupKey: String,
  payload:  Map[String, String],
  attempts: Int
)

sealed trait EnqueueResult
object EnqueueResult {
  /** A new waiting task was created. */
  case object Added extends EnqueueResult
  /** An active (waiting or worked-on) task with the same dedupKey already
   *  existed, so nothing was added. */
  case object Duplicate extends EnqueueResult
}

/** What a handler decided about a claimed task. */
sealed trait HandlerOutcome
object HandlerOutcome {
  /** Work done — remove the task (tombstone). */
  case object Done extends HandlerOutcome
  /** Data was already fresh; nothing to do — remove the task (tombstone). */
  case object Skipped extends HandlerOutcome
  /** Couldn't finish now; return the task to waiting to retry later. */
  case class Reschedule(error: Option[String] = None) extends HandlerOutcome
}

/** The three states a task moves through. `deleted` is a tombstone reaped by a
 *  TTL index, kept briefly so a finishing worker can't race a re-enqueue. */
object TaskState {
  val Waiting  = "waiting"
  val WorkedOn = "worked_on"
  val Deleted  = "deleted"
}

/**
 * A durable, deduplicated work queue.
 *
 * Tasks are ordered by submission time (FIFO). `enqueue` is idempotent per
 * `dedupKey`: while a task with that key is active (waiting or worked-on) a
 * second enqueue is a no-op, which is how N cinema locations asking to enrich
 * the same film collapse to one task. `claim` atomically leases the oldest
 * waiting task; if the holder dies, `reapExpiredLeases` returns it to waiting.
 * The *redundancy* decision (is this still worth doing?) is the handler's job,
 * not the queue's — the queue just hands out leased work.
 *
 * Two implementations share this contract: [[MongoTaskQueue]] (durable,
 * multi-instance-safe) and [[InMemoryTaskQueue]] (tests / Mongo-less dev).
 */
trait TaskQueue {
  /** Add a waiting task unless one with `dedupKey` is already active. */
  def enqueue(
    taskType:    TaskType,
    dedupKey:    String,
    payload:     Map[String, String] = Map.empty,
    submittedAt: Instant             = Instant.now()
  ): EnqueueResult

  /** Atomically lease the oldest *eligible* waiting task to `workerId` for
   *  `lease`, or None when nothing is waiting. A task released with a future
   *  `notBefore` (transient-failure backoff) is skipped until `now` reaches it,
   *  so a perpetually-failing task can't hot-loop ahead of newer work. */
  def claim(workerId: String, lease: FiniteDuration, now: Instant = Instant.now()): Option[Task]

  /** Mark a worked-on task done (tombstone). No-op unless `workerId` still holds
   *  it — so a late call from a worker whose lease was reaped can't clobber a
   *  task another worker has since reclaimed. */
  def complete(id: String, workerId: String): Unit

  /** Return a worked-on task to waiting (retry). Same ownership guard as
   *  [[complete]]. `notBefore`, when set, holds the task back from `claim` until
   *  that instant — the transient-failure backoff (see [[TaskWorker]]); `None`
   *  makes it immediately claimable again (e.g. a no-handler hand-off). */
  def release(id: String, workerId: String, error: Option[String] = None,
              notBefore: Option[Instant] = None): Unit

  /** Return every worked-on task whose lease has expired to waiting. Returns how
   *  many were reaped. */
  def reapExpiredLeases(now: Instant = Instant.now()): Int

  /** Count of tasks per state — for the debug view and tests. */
  def countByState(): Map[String, Long]

  /** Read-only snapshot for the monitoring page: per-state counts plus the live
   *  ACTIVE tasks (waiting + worked-on), oldest-first, capped at `activeLimit`.
   *  Tombstones are counted but not listed. Index-backed + bounded so the web
   *  can poll it cheaply. */
  def monitor(activeLimit: Int = 200): QueueSnapshot

  /** Push: ring `onWaiting` whenever fresh work becomes claimable (a newly
   *  enqueued task), so a worker pool can park on a signal instead of polling.
   *  The push is a doorbell only — it carries no task, so workers still `claim`
   *  the work atomically and a missed or duplicate ring is harmless (the pool's
   *  idle backstop re-checks regardless). Returns a handle to stop watching, or
   *  None when this queue can't push (then the backstop is the only wakeup).
   *  Default no-op so a queue need only override it if it can push. */
  def watchWaiting(onWaiting: () => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/** One active task as shown on the monitoring page. */
case class TaskSummary(
  id:             String,
  taskType:       String,
  dedupKey:       String,
  state:          String,
  submittedAt:    Instant,
  attempts:       Int,
  workerId:       Option[String],
  leaseExpiresAt: Option[Instant],
  lastError:      Option[String]
)

/** A point-in-time view of the queue for the monitoring page. */
case class QueueSnapshot(counts: Map[String, Long], active: Seq[TaskSummary])
