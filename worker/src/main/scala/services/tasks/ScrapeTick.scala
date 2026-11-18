package services.tasks

import settings.ScrapeEnqueueSpreadSlices

import tools.{ScheduledTick, TickRecorder, TickRun}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.Try

/**
 * One [[ScrapeReaper]] tick's work: enqueue every stale cinema, most-overdue first ([[DueScrapes]]),
 * under the per-tick cap and the outstanding-task budget ([[ScrapeAdmission]]), and report the tick
 * completed once that work has ended.
 *
 * @param interval      how often the reaper ticks: the window a spread batch is staggered across
 * @param enqueueSpread SPREAD the per-tick batch across the tick interval instead of dumping it all
 *                      at the tick instant. The reaper enqueues a clump of due cinemas each tick; they
 *                      fetch in parallel and their HTML/JSON payloads PARSE together — a CPU spike that
 *                      floors the shared-CPU credit balance (the parse-wave burst). Slicing the same batch
 *                      into `enqueueSpread` groups enqueued at staggered offsets within the interval keeps
 *                      the parses from landing together: SAME total work and freshness (a sub-minute stagger
 *                      is negligible against the 60-min scrape window), lower CPU peak. Default 1 disables
 *                      it (single group at offset 0), leaving tests that drive `tick()` directly — and the
 *                      deterministic snapshot — unaffected.
 * @param ticks         where each tick whose work ran to its end is reported (`ScheduledTickStale`): by the
 *                      tick itself, or — when it spreads its batch — by its last slice, never at the hand-off.
 * @param scheduler     runs the spread slices
 */
class ScrapeTick(
  due:           DueScrapes,
  admission:     ScrapeAdmission,
  enqueuer:      ScrapeEnqueuer,
  interval:      FiniteDuration,
  enqueueSpread: ScrapeEnqueueSpreadSlices = ScrapeEnqueueSpreadSlices(1),
  ticks:         TickRecorder = TickRecorder.Unrecorded,
  scheduler:     ScheduledExecutorService
) extends Logging {

  /** A tick that had no work to do (another replica claimed it): reported completed. */
  def skipped(): Unit = complete(TickRun.Completed)

  /** A tick's work ended as `run`: reported when it completed. */
  private def complete(run: TickRun): Unit =
    if (run == TickRun.Completed) ticks.completed(ScrapeTick.Name, interval)

  /** The slices of one spread tick still to end, and whether every one ended complete: the last to end completes the
   *  tick, unless one could not enqueue. A slice dropped on shutdown never ends, and the tick never completes. */
  private final class SpreadTick(slices: Int) {
    private val left       = new AtomicInteger(slices)
    private val incomplete = new AtomicBoolean(false)
    def ended(run: TickRun): Unit = {
      if (run == TickRun.Incomplete) incomplete.set(true)
      if (left.decrementAndGet() == 0) complete(TickRun.completedWhen(!incomplete.get))
    }
  }

  /** Enqueue every stale cinema, most-overdue first under the per-tick cap; returns how many were
   *  enqueued synchronously. Admits nothing when the waiting backlog cannot be read: unknown is not
   *  empty, the next tick re-reads, and the same Mongo would have failed the enqueues anyway. */
  def run(now: Instant): Int = enqueuer.outstanding() match {
    case scala.util.Failure(exception) =>
      logger.warn(s"ScrapeReaper: waiting count unreadable (${exception.getMessage}) — admitting nothing this tick.")
      complete(TickRun.Incomplete)
      0
    case scala.util.Success(outstanding) =>
      val room = admission.room(outstanding)
      if (enqueueSpread.value <= 1) enqueueAll(due.at(now), now, room, outstanding)
      else enqueueSpreadOut(admission.admissible(due.at(now).take(admission.rampedCap(now)), room))
  }

  /** Un-spread healthy path (the default): enqueue the whole capped batch now. */
  private def enqueueAll(due: Vector[(String, String)], now: Instant, room: ScrapeAdmission.Room, outstanding: Int): Int = {
    val enqueued = enqueuer.enqueueUpTo(due, admission.rampedCap(now), room)
    if (enqueued.added > 0) logger.info(s"ScrapeReaper enqueued ${enqueued.added} stale cinema(s) ($outstanding scrape task(s) already waiting).")
    complete(enqueued.run)
    enqueued.added
  }

  /** Spread the capped batch across the interval so the parses don't clump — see
   *  `enqueueSpread`. Enqueue the first slice now; defer the rest onto staggered
   *  offsets. The queue dedups, so a slice landing near the next tick can't
   *  double-enqueue. Returns only what was enqueued SYNCHRONOUSLY (slice 0),
   *  matching the un-spread contract for the first-of-batch. The tick completes when its
   *  last slice has enqueued, not when it handed the slices off ([[SpreadTick]]). */
  private def enqueueSpreadOut(batch: Vector[(String, String)]): Int = {
    val plan   = planSlices(batch, enqueueSpread.value)
    val spread = new SpreadTick(plan.size)
    val enqueuedNow = plan.headOption.map { case (_, first) => enqueuer.enqueueUpTo(first, first.size, admission.unbounded) }.getOrElse(Enqueued.Zero)
    plan.drop(1).foreach { case (offset, group) => scheduleSlice(offset, () => enqueueSlice(offset, group, spread)) }
    spread.ended(enqueuedNow.run)
    val deferred = plan.drop(1).map(_._2.size).sum
    if (enqueuedNow.added > 0 || deferred > 0)
      logger.info(s"ScrapeReaper enqueued ${enqueuedNow.added} stale cinema(s) now, spreading $deferred more across the tick in ${plan.size - 1} slice(s).")
    enqueuedNow.added
  }

  private def enqueueSlice(offset: FiniteDuration, group: Vector[(String, String)], spread: SpreadTick): Unit =
    ScheduledTick.logged(s"ScrapeReaper slice at +${offset.toSeconds}s", logger) {
      val n = Try(enqueuer.enqueueUpTo(group, group.size, admission.unbounded))
      spread.ended(n.fold(_ => TickRun.Incomplete, _.run))
      if (n.get.added > 0) logger.info(s"ScrapeReaper enqueued ${n.get.added} stale cinema(s) (spread slice at +${offset.toSeconds}s).")
    }

  /** Split the tick's due-and-capped batch into `slices` contiguous groups, each
   *  tagged with the within-interval offset at which it should be enqueued (group
   *  k at k/groupCount of the interval), so their scrape parses land staggered
   *  across the tick rather than clumping at the tick instant — same total work,
   *  lower CPU peak. Group sizes differ by at most one (earlier groups take the
   *  remainder). `slices <= 1` or a batch of one → a single group at offset 0 (the
   *  un-spread default). Pure given the batch, `slices`, and `interval`. */
  private[tasks] def planSlices(
    batch:  Vector[(String, String)],
    slices: Int
  ): Vector[(FiniteDuration, Vector[(String, String)])] = {
    val n = math.max(1, slices)
    if (n <= 1 || batch.size <= 1) Vector((Duration.Zero, batch))
    else {
      val groupCount = math.min(n, batch.size)
      val base       = batch.size / groupCount
      val remainder  = batch.size % groupCount
      var idx        = 0
      (0 until groupCount).iterator.map { k =>
        val size   = base + (if (k < remainder) 1 else 0)
        val group  = batch.slice(idx, idx + size)
        idx += size
        ((interval.toMillis * k / groupCount).millis, group)
      }.toVector
    }
  }

  /** Deferral seam for the spread slices (overridable in tests so the staggering is
   *  observed deterministically without wall-clock waits). Runs `task` after `delay`
   *  on the reaper's scheduler; the returned handle is intentionally discarded — a
   *  dropped slice on shutdown just re-enqueues on the next tick. */
  protected def scheduleSlice(delay: FiniteDuration, task: Runnable): Unit = {
    scheduler.schedule(task, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }
}

object ScrapeTick {
  /** The tick's name, as [[tools.TickRecorder]] reports it. */
  val Name = "scrape-reaper"
}
