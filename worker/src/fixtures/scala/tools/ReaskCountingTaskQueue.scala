package tools

import services.freshness.FreshnessStore
import services.tasks.{DueWindow, EnqueueResult, ForwardingTaskQueue, TaskQueue, TaskType}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

/**
 * Counts the enqueues that ask again for work already DONE and not yet due again: a task
 * added under a dedup key the freshness store holds a stamp for, which that key's own
 * [[DueWindow]] says is still fresh.
 *
 * The distinction a fixpoint pass needs and a plain enqueue count cannot make. Detail and
 * rating tasks share their dedup key with the freshness stamp their handler writes on
 * success, so each enqueue is one of three things:
 *   - a RETRY: no stamp, the work never succeeded — a transient failure, or a page the
 *     fixture recording lacks — re-tried each tick exactly as production re-tries one;
 *   - a REFRESH: stamped, but the key's phase boundary has passed since — wall-clock time
 *     moved during the run, and the schedule says ask again;
 *   - a RE-ASK: stamped and not due, enqueued anyway. That is a producer going round the
 *     gate, and the only one of the three that is churn.
 * The first two are production working as designed; counting them made the fixpoint flake
 * on whichever keys' boundaries happened to fall inside a run. Every other task type is
 * counted by the metered queue's own `added` series.
 */
final class ReaskCountingTaskQueue(delegate: TaskQueue, freshness: => FreshnessStore,
                                   dueWindowFor: TaskType => Option[DueWindow],
                                   // The clock the producers gate on — the wiring's — so "is it
                                   // due?" is asked of the same instant they asked it of.
                                   now: () => Instant) extends ForwardingTaskQueue(delegate) {
  private val reasks = new ConcurrentHashMap[TaskType, AtomicLong]()

  private val examples = new java.util.concurrent.ConcurrentLinkedQueue[String]()

  /** A few of the re-asked dedup keys, newest last — what a failing fixpoint names. */
  def reaskedKeys: Seq[String] = examples.asScala.toSeq

  /** Stamped-key enqueues so far, per task type. */
  def reasked: Map[TaskType, Long] = reasks.asScala.view.mapValues(_.get).toMap

  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String],
                       submittedAt: Instant, notBefore: Option[Instant], claimAhead: FiniteDuration): EnqueueResult = {
    val result = delegate.enqueue(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead)
    val stamp  = if (result == EnqueueResult.Added) freshness.lastFetchedAt(dedupKey) else None
    if (stamp.isDefined && dueWindowFor(taskType).exists(!_.isDue(dedupKey, stamp, now()))) {
      reasks.computeIfAbsent(taskType, _ => new AtomicLong()).incrementAndGet()
      examples.add(dedupKey)
      while (examples.size > 20) examples.poll()
    }
    result
  }
}

object ReaskCountingTaskQueue {
  /** The task types whose dedup key is also the freshness stamp their handler writes on
   *  success — the only ones a re-ask can be told apart from a retry for. */
  val StampedTypes: Set[TaskType] =
    Set(TaskType.EnrichDetails, TaskType.ImdbRating, TaskType.McRating, TaskType.RtRating, TaskType.FilmwebRating)
}
