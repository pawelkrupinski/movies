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
 *
 * TREND GUARD: even past `stuckAfter`, a restart is the wrong move when credit is
 * already CLIMBING — the duty-cycle + reaper backoff IS rebuilding it, just slowly.
 * Restarting then is actively harmful: the boot storm (freshness-hydrate + ~300
 * cinema re-scrape) costs more credit than it saves, so the watchdog itself keeps
 * the balance pinned below the release threshold — a self-inflicted crash loop
 * (observed 2026-06-24, ~30-min cadence). So `check` defers the restart while the
 * credit balance is on a net-upward path across `trendWindow`, and only fires once
 * recovery has stalled (flat or declining). `creditBalance` reads the live balance
 * (the [[CpuCreditPoller]]'s last sample); `None` (no Fly-Prometheus token, or a
 * failed read) disables the guard and falls back to the bare stuck behavior.
 */
class ThrottleStuckWatchdog(
  throttle:      ScrapeThrottleSignal,
  stuckAfter:    FiniteDuration,
  onStuck:       () => Unit,
  // Live shared-CPU credit balance, or None when unreadable (guard then disabled).
  creditBalance: () => Option[Double] = () => None,
  // Window over which "trending up" is judged; the guard needs samples spanning at
  // least half of it before it trusts the trend (so a brief spike can't defer).
  trendWindow:   FiniteDuration       = 10.minutes,
  // Net rise (credit units) over the window required to count as "recovering". 0 =
  // strictly rising; a stall (flat) or decline does NOT defer, so a true wedge
  // (balance pinned) still restarts.
  minRise:       Double               = 0.0,
  checkEvery:    FiniteDuration       = 1.minute,
  now:           () => Instant        = () => Instant.now()
) extends Stoppable with Logging {

  // None while healthy; the instant throttle FIRST engaged in the current unbroken
  // stretch otherwise. Cleared the moment throttle releases, so only a continuous
  // run counts toward the threshold.
  private val throttledSince = new AtomicReference[Option[Instant]](None)
  private val fired          = new AtomicBoolean(false)
  // Recent (time, balance) samples, pruned to `trendWindow`; backs the trend guard.
  // Recorded only while throttled and cleared on release, so a healthy stretch's
  // readings can't leak into a later episode's trend.
  private val creditSamples  = new AtomicReference[Vector[(Instant, Double)]](Vector.empty)
  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("throttle-stuck-watchdog")

  /** True once throttle has been continuously engaged for at least `stuckAfter`.
   *  Resets on any release. Pure given `now` — the unit under test. */
  private[tasks] def isStuck(): Boolean =
    if (!throttle.isThrottled) { throttledSince.set(None); false }
    else {
      val since = throttledSince.updateAndGet(prev => if (prev.isDefined) prev else Some(now()))
      since.exists(start => JDuration.between(start, now()).toMillis >= stuckAfter.toMillis)
    }

  /** Append the current credit reading (if any) and drop samples older than the
   *  trend window. No-op when the balance source is unavailable. */
  private def recordCredit(): Unit =
    creditBalance().foreach { b =>
      val cutoff = now().minusMillis(trendWindow.toMillis)
      creditSamples.updateAndGet(prev => (prev :+ (now() -> b)).dropWhile(_._1.isBefore(cutoff)))
    }

  /** True when credit is on a net-upward path across (at least half of) the trend
   *  window — recovery is underway, so a restart would only set it back. False when
   *  there's no balance source or too little history to judge (fall back to the bare
   *  stuck behavior). Pure given the recorded samples. */
  private[tasks] def creditTrendingUp(): Boolean = {
    val s = creditSamples.get()
    (s.headOption, s.lastOption) match {
      case (Some((t0, v0)), Some((t1, v1))) =>
        JDuration.between(t0, t1).toMillis >= trendWindow.toMillis / 2 && (v1 - v0) > minRise
      case _ => false
    }
  }

  /** One watchdog tick: fire `onStuck` exactly once if the throttle is wedged AND
   *  credit isn't already recovering. */
  private[tasks] def check(): Unit = {
    if (throttle.isThrottled) recordCredit() else creditSamples.set(Vector.empty)
    if (isStuck()) {
      if (creditTrendingUp())
        logger.warn(s"Worker throttled continuously > ${stuckAfter.toMinutes}min, but credit is TRENDING UP — " +
          s"the duty-cycle + reaper backoff is rebuilding it; DEFERRING the restart (a restart's boot-storm credit " +
          s"cost would set recovery back).")
      else if (fired.compareAndSet(false, true)) {
        logger.error(s"Worker throttled continuously > ${stuckAfter.toMinutes}min and credit is NOT recovering — the " +
          s"duty-cycle + reaper backoff did not rebuild credit; restarting the machine (Fly reschedules on the " +
          s"non-zero exit, and the per-host circuit breaker skips the offending host on the way back up).")
        onStuck()
      }
    }
  }

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(check()), checkEvery.toMillis, checkEvery.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ThrottleStuckWatchdog armed: restart if throttled continuously > ${stuckAfter.toMinutes}min (checked every ${checkEvery.toSeconds}s).")
  }

  def stop(): Unit = { scheduler.shutdownNow(); () }
}
