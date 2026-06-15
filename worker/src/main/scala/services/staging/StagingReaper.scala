package services.staging

import play.api.Logging
import services.Stoppable
import services.events.{DomainEvent, TaskFinished}
import services.movies.TitleNormalizer
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import services.tasks.{EnqueueResult, StagingTaskKeys, TaskQueue, TaskType}
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Drives the staging-incubation pipeline through the durable [[TaskQueue]] — the
 * queue-era replacement for the old synchronous `StagingPromoter` tick. It owns
 * ALL the chaining: given a film's current `pending_movies` state it enqueues the
 * single next step it needs (detail → resolve → imdb → fold), and the queue's
 * per-`dedupKey` idempotency makes every enqueue safe to repeat.
 *
 * It advances the chain two ways, mirroring [[services.tasks.RatingEnqueuer]] +
 * [[services.tasks.DetailReaper]]:
 *   - EVENT-DRIVEN: subscribes to `TaskFinished` and, when a staging step
 *     completes, enqueues the film's next step immediately (low latency).
 *   - PERIODIC BACKSTOP: a `ScheduledRunStore`-gated tick scans every incubating
 *     film and enqueues its next step — the initial kick for brand-new
 *     newcomers, and recovery for any film whose chain stalled (a step that kept
 *     rescheduling, or a completion event lost across a restart).
 *
 * The decision lives in ONE place ([[enqueueNext]]) so both paths agree.
 */
class StagingReaper(
  steps:        StagingSteps,
  queue:        TaskQueue,
  staging:      StagingRepository,
  interval:     FiniteDuration    = 2.minutes,
  initialDelay: FiniteDuration    = 30.seconds,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock             = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("staging-reaper")

  def start(): Unit = {
    if (!staging.enabled) { logger.info("StagingReaper: staging disabled; not starting."); return }
    scheduler.scheduleWithFixedDelay(() => Try(tickIfClaimed()),
      initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"StagingReaper started — incubating pending_movies via the queue every ${interval.toSeconds}s (first in ${initialDelay.toSeconds}s).")
  }

  /** Advance the chain in reaction to a finished staging step. Ignores
   *  `StagingFold` (terminal — the fold deletes the rows) and every non-staging
   *  task type. */
  def onTaskFinished: PartialFunction[DomainEvent, Unit] = {
    case TaskFinished(t, _, payload) if chainable(t) => enqueueNext(StagingTaskKeys.anchorOf(payload)); ()
  }

  private def chainable(t: TaskType): Boolean =
    t == TaskType.StagingDetail || t == TaskType.StagingResolveTmdb || t == TaskType.StagingResolveImdbId

  /** Run the periodic scan only if this machine wins the window's occurrence
   *  claim (cluster-safe, like the other reapers). Returns tasks enqueued (0 when
   *  the claim was lost). Package-private so tests drive it directly. */
  private[staging] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("staging", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Enqueue the next pending step for EVERY incubating film. Public so tests /
   *  the fixture harness can drive one pass directly. Returns tasks enqueued. */
  def tick(): Int = {
    val n = staging.findAll().map(r => TitleNormalizer.sanitize(r.title)).distinct.foldLeft(0)((acc, a) => acc + enqueueNext(a))
    if (n > 0) logger.info(s"StagingReaper enqueued $n staging step(s).")
    n
  }

  /** The staging state machine for one film: enqueue whichever step it needs next.
   *  Idempotent (the queue dedups), so calling it on every completion AND every
   *  periodic scan converges without double-work. Returns tasks enqueued. */
  private[staging] def enqueueNext(anchor: String): Int = {
    val rows = steps.rowsFor(anchor)
    rows.headOption.map(_.title) match {
      case None => 0
      case Some(title) =>
        if (!rows.forall(_.record.tmdbConcluded)) {
          // Some hint-combination still unresolved: finish its detail first, then
          // resolve. `forall` (not `exists`) so a partially-resolved anchor — one
          // group concluded, another not — keeps advancing the unconcluded group
          // instead of jumping to fold and stranding it.
          val notReady = rows.filterNot(steps.detailReady)
          if (notReady.nonEmpty)
            notReady.map(_.cinema).distinct.count(c =>
              added(queue.enqueue(TaskType.StagingDetail, StagingTaskKeys.detailDedup(title, c.displayName),
                StagingTaskKeys.detailPayload(title, c.displayName))))
          else
            countOne(queue.enqueue(TaskType.StagingResolveTmdb,
              StagingTaskKeys.resolveTmdbDedup(title), StagingTaskKeys.titlePayload(title)))
        } else if (rows.exists(r => r.record.tmdbId.isDefined && r.record.imdbId.isEmpty) && !steps.imdbRecoveryDone(anchor)) {
          // Resolved with a tmdbId but no IMDb cross-reference, and recovery not yet
          // attempted: recover it before folding. Best-effort — once attempted (even
          // on a no-match) `imdbRecoveryDone` is true and the film folds.
          countOne(queue.enqueue(TaskType.StagingResolveImdbId,
            StagingTaskKeys.resolveImdbDedup(title), StagingTaskKeys.titlePayload(title)))
        } else {
          // Concluded (hit+imdb, or tmdbNoMatch): fold the whole sanitize group at
          // once (group-scoped + idempotent), one task per film.
          countOne(queue.enqueue(TaskType.StagingFold, StagingTaskKeys.foldDedup(title), StagingTaskKeys.titlePayload(title)))
        }
    }
  }

  private def added(r: EnqueueResult): Boolean = r == EnqueueResult.Added
  private def countOne(r: EnqueueResult): Int = if (added(r)) 1 else 0

  override def stop(): Unit = { scheduler.shutdown(); () }
}
