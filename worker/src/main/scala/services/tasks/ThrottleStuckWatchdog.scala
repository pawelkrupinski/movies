package services.tasks

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.time.{Duration => JDuration, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Last-resort backstop against a WEDGED sustained-throttle spiral. If the worker
 * stays CPU-credit throttled CONTINUOUSLY longer than `stuckAfter`, the pool
 * duty-cycle ([[TaskWorker.throttlePauseMillis]]) and reaper enqueue-backoff have
 * failed to recover it — the self-sustaining loop of 2026-06-23, where the box
 * stayed pinned at the throttle ceiling for two hours. Exit non-zero so Fly
 * restarts the machine (the worker already relies on this: see WorkerMain).
 *
 * A bare restart did NOT recover on its own that day — the slow host was still
 * hammered, so the spiral re-formed. This is safe to wire NOW only because it
 * ships alongside the per-host circuit breaker ([[HostCircuitBreakerHttpFetch]]):
 * after the restart the offending host is skipped, the backlog + heap are clear,
 * and the worker comes back healthy instead of re-spiralling.
 *
 * `stuckAfter` is far above any healthy backoff (which clears in minutes, well
 * under the threshold), so this never fires on a normal throttle episode — it's a
 * deep safety net, not a control loop. `now` and `onStuck` are injectable so a
 * test drives the decision without a clock or a real `sys.exit`.
 */
class ThrottleStuckWatchdog(
  throttle:   ScrapeThrottleSignal,
  stuckAfter: FiniteDuration,
  onStuck:    () => Unit,
  checkEvery: FiniteDuration = 1.minute,
  now:        () => Instant  = () => Instant.now()
) extends Stoppable with Logging {

  // None while healthy; the instant throttle FIRST engaged in the current unbroken
  // stretch otherwise. Cleared the moment throttle releases, so only a continuous
  // run counts toward the threshold.
  private val throttledSince = new AtomicReference[Option[Instant]](None)
  private val fired          = new AtomicBoolean(false)
  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("throttle-stuck-watchdog")

  /** True once throttle has been continuously engaged for at least `stuckAfter`.
   *  Resets on any release. Pure given `now` — the unit under test. */
  private[tasks] def isStuck(): Boolean =
    if (!throttle.isThrottled) { throttledSince.set(None); false }
    else {
      val since = throttledSince.updateAndGet(prev => if (prev.isDefined) prev else Some(now()))
      since.exists(start => JDuration.between(start, now()).toMillis >= stuckAfter.toMillis)
    }

  /** One watchdog tick: fire `onStuck` exactly once if the throttle is wedged. */
  private[tasks] def check(): Unit =
    if (isStuck() && fired.compareAndSet(false, true)) {
      logger.error(s"Worker throttled continuously > ${stuckAfter.toMinutes}min — the duty-cycle + reaper " +
        s"backoff did not recover credit; restarting the machine (Fly reschedules on the non-zero exit, and the " +
        s"per-host circuit breaker skips the offending host on the way back up).")
      onStuck()
    }

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(check()), checkEvery.toMillis, checkEvery.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ThrottleStuckWatchdog armed: restart if throttled continuously > ${stuckAfter.toMinutes}min (checked every ${checkEvery.toSeconds}s).")
  }

  def stop(): Unit = { scheduler.shutdownNow(); () }
}
