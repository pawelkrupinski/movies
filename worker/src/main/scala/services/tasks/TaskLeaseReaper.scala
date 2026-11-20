package services.tasks

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}

import scala.concurrent.duration.*
import scala.util.Try

import play.api.Logging
import tools.{ScheduledTick, ThreadPools, TickRecorder}

/**
 * The [[TaskWorker]] pool's one shared timer thread: it returns expired leases to waiting every
 * `reapInterval` — independently of the workers, so a stuck lease is reaped even while all workers are
 * busy — and rings the pool when a held task becomes claimable. Reaping is global, so one thread does it
 * for the whole pool, which a per-worker reap can't.
 */
final class TaskLeaseReaper(
  queue:        TaskConsumer,
  reapInterval: TaskWorker.ReapInterval = TaskWorker.ReapInterval(30.seconds),
  clock:        Clock,
  // Where each completed lease reap is reported (`ScheduledTickStale`).
  ticks:        TickRecorder = TickRecorder.Unrecorded,
  threadPools:  ThreadPools
) extends Logging {

  private val scheduler: ScheduledExecutorService = threadPools.scheduler("task-reaper")

  /** Reap every `reapInterval`, calling `ring` when a reap freed work. Leases run `processingTimeout` (5min),
   *  so reaping every 30s is ample. A reap returns crashed/stuck (and other nodes') leases to waiting; ring
   *  so the freed work is picked up now, not at the next backstop. */
  def start(ring: () => Unit): Unit = {
    ticks.started("task-lease-reaper", reapInterval.value, reapInterval.value)
    scheduler.scheduleWithFixedDelay(
      () => ScheduledTick.recorded("task-lease-reaper", reapInterval.value, logger, ticks) {
        if (queue.reapExpiredLeases(clock.instant()) > 0) ring()
      },
      reapInterval.value.toMillis, reapInterval.value.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Call `ring` for a new task when it can be claimed: now, or when its hold ends. A held task's insert finds
   *  nothing to claim, and its hold ending writes nothing, so without a timed ring the pool — parked
   *  again — sat out `idleBackstop` with the task claimable: a staggered ScrapeChunk fan-out read 0–28 s
   *  of head-of-line age around the clock on worker-uk with three of four slots idle (2026-10-07).
   *  The timed ring runs on this thread; it only bumps the doorbell. */
  def ringWhenClaimable(heldUntil: Option[Instant], ring: () => Unit): Unit =
    TaskWorker.untilClaimable(clock.instant(), heldUntil) match {
      // A push racing `stop()` meets a shut scheduler: nothing is left to wake then.
      case Some(wait) => Try(scheduler.schedule((() => ring()): Runnable, wait.toNanos, TimeUnit.NANOSECONDS)); ()
      case None       => ring()
    }

  /** Stop the reap, and every hold's pending ring with it. */
  def stop(): Unit = { scheduler.shutdownNow(); () }

  /** How often leases are reaped. */
  def interval: TaskWorker.ReapInterval = reapInterval
}
