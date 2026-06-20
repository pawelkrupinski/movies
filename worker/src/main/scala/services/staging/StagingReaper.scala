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
 * It advances the chain two ways, mirroring [[services.tasks.DetailReaper]]:
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
   *  the fixture harness can drive one pass directly. Returns tasks enqueued.
   *
   *  Takes ONE corpus-wide `findAll` snapshot and groups it by anchor, rather
   *  than re-scanning the repository once per distinct anchor (the old
   *  `enqueueNext(anchor)` → `rowsFor` → `findAll` did 1 + N scans per pass; on a
   *  cold-corpus boot N is the whole staging set). The grouped rows are
   *  `_id`-sorted (findAll is sorted and `groupBy` keeps encounter order), so each
   *  group is byte-identical to what `rowsFor(anchor)` would have returned. */
  def tick(): Int = {
    val byAnchor = staging.findAll().groupBy(r => TitleNormalizer.sanitize(r.title))
    val n = byAnchor.foldLeft(0) { case (acc, (anchor, rows)) => acc + enqueueNext(anchor, rows) }
    if (n > 0) logger.info(s"StagingReaper enqueued $n staging step(s).")
    n
  }

  /** Event path: fetch just this one anchor's rows, then enqueue its next step. */
  private[staging] def enqueueNext(anchor: String): Int = enqueueNext(anchor, steps.rowsFor(anchor))

  /** The staging state machine for one film: enqueue whichever step it needs next,
   *  given its already-fetched `rows`. Idempotent (the queue dedups), so calling
   *  it on every completion AND every periodic scan converges without double-work.
   *  Returns tasks enqueued. */
  private[staging] def enqueueNext(anchor: String, rows: Seq[StagingRecord]): Int = {
    (rows.headOption.map(_.title), stepFor(rows, anchor)) match {
      case (Some(title), Some(StagingStep.Detail)) =>
        // Some hint-combination still owes detail: finish each unready cinema's
        // detail first (one task per cinema).
        rows.filterNot(steps.detailReady).map(_.cinema).distinct.count(c =>
          added(queue.enqueue(TaskType.StagingDetail, StagingTaskKeys.detailDedup(title, c.displayName),
            StagingTaskKeys.detailPayload(title, c.displayName))))
      case (Some(title), Some(StagingStep.ResolveTmdb)) =>
        countOne(queue.enqueue(TaskType.StagingResolveTmdb,
          StagingTaskKeys.resolveTmdbDedup(title), StagingTaskKeys.titlePayload(title)))
      case (Some(title), Some(StagingStep.ResolveImdb)) =>
        // Resolved with a tmdbId but no IMDb cross-reference, recovery not yet
        // attempted: recover before folding (best-effort — once attempted, even on
        // a no-match, `imdbRecoveryDone` is true and the film folds).
        countOne(queue.enqueue(TaskType.StagingResolveImdbId,
          StagingTaskKeys.resolveImdbDedup(title), StagingTaskKeys.titlePayload(title)))
      case (Some(title), Some(StagingStep.Fold)) =>
        // Concluded (hit+imdb, or tmdbNoMatch): fold the whole sanitize group at
        // once (group-scoped + idempotent), one task per film.
        countOne(queue.enqueue(TaskType.StagingFold, StagingTaskKeys.foldDedup(title), StagingTaskKeys.titlePayload(title)))
      case _ => 0 // no rows for this anchor
    }
  }

  /** Classify the film at `anchor` (given its already-fetched `rows`) to the step
   *  it needs NEXT. The single source of truth for the staging state machine —
   *  `enqueueNext` acts on it, `stepCounts` tallies by it — so the dashboard and
   *  the reaper can never disagree. `forall(tmdbConcluded)` (not `exists`) so a
   *  partially-resolved anchor keeps advancing its unconcluded group rather than
   *  jumping to fold and stranding it. None when the anchor has no rows. */
  private[staging] def stepFor(rows: Seq[StagingRecord], anchor: String): Option[StagingStep] =
    if (rows.isEmpty) None
    else if (!rows.forall(_.record.tmdbConcluded))
      if (rows.exists(r => !steps.detailReady(r))) Some(StagingStep.Detail) else Some(StagingStep.ResolveTmdb)
    else if (rows.exists(r => r.record.tmdbId.isDefined && r.record.imdbId.isEmpty) && !steps.imdbRecoveryDone(anchor))
      Some(StagingStep.ResolveImdb)
    else Some(StagingStep.Fold)

  /** How many incubating films sit at each staging step right now — the
   *  population view behind the queue tasks, for the Prometheus gauge. One
   *  `findAll()`; counts DISTINCT films (a film's multiple cinema rows count
   *  once, keyed by `sanitize(title)` like the chain). Reuses [[stepFor]] so it
   *  agrees with what the reaper will actually enqueue. */
  def stepCounts(): Map[StagingStep, Int] =
    staging.findAll()
      .groupBy(r => TitleNormalizer.sanitize(r.title))
      .toSeq
      .flatMap { case (anchor, rows) => stepFor(rows, anchor) }
      .groupBy(identity)
      .map { case (step, occurrences) => step -> occurrences.size }

  private def added(r: EnqueueResult): Boolean = r == EnqueueResult.Added
  private def countOne(r: EnqueueResult): Int = if (added(r)) 1 else 0

  override def stop(): Unit = { scheduler.shutdown(); () }
}
