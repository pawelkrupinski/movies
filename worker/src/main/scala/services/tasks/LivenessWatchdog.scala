package services.tasks

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.util.Try

/**
 * Last-resort backstop against a WEDGED-but-alive JVM. The worker's
 * [[WorkerHeartbeat]] stamps a wall-clock pulse every minute from its own
 * scheduler thread; this watchdog watches that pulse and, if it goes stale past
 * `stalenessThreshold`, exits the process (non-zero) so Fly reschedules the
 * machine. A fresh boot re-establishes Mongo/HTTP and clears the heap — the same
 * recovery the manual restart gave on 2026-06-23.
 *
 * Why this exists ALONGSIDE [[ThrottleStuckWatchdog]]: that one fires only while
 * `throttle.isThrottled` is CONTINUOUSLY true, but the 2026-06-23 wedge was a
 * heap OOM, not a throttle spiral. When the heap blew, the OOM killed the
 * `CpuCreditPoller`'s HTTP selector, so the poller failed OPEN to "healthy" —
 * `isThrottled` read false and the throttle watchdog never tripped, while the
 * process limped on for ~2h answering the (then-static) `/health` 200. A pulse
 * that stalls regardless of throttle state catches that: it keys off "is the
 * worker still TICKING", not "is it throttled" or "is it doing useful work" (the
 * heartbeat thread fires even when the reapers are intentionally throttled to a
 * trickle, so an idle-but-healthy worker never reads as wedged).
 *
 * `now` and `onWedged` are injectable so a test drives the decision without a
 * clock or a real `sys.exit`; `lastBeatMillis` is a supplier so the test feeds a
 * pulse without a live [[WorkerHeartbeat]]. `stalenessThreshold` sits several
 * heartbeat intervals above the 1-min pulse so GC jitter or a single slow tick
 * never trips it — it's a deep safety net, not a control loop.
 */
class LivenessWatchdog(
  lastBeatMillis:     () => Long,
  stalenessThreshold: FiniteDuration,
  onWedged:           () => Unit,
  checkEvery:         FiniteDuration = 1.minute,
  now:                () => Long     = () => System.currentTimeMillis()
) extends Stoppable with Logging {

  private val fired = new AtomicBoolean(false)
  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("liveness-watchdog")

  /** True once the heartbeat pulse has been stale for at least `stalenessThreshold`.
   *  A non-positive pulse (never stamped) is treated as alive — the heartbeat seeds
   *  itself at construction, so 0 only means "not wired", not "wedged". Pure given
   *  `now` and `lastBeatMillis` — the unit under test. */
  private[tasks] def isWedged(): Boolean = {
    val last = lastBeatMillis()
    last > 0L && (now() - last) >= stalenessThreshold.toMillis
  }

  /** True while the worker's heartbeat is fresh — read by `/health`. */
  def isAlive: Boolean = !isWedged()

  /** One watchdog tick: exit ONCE if the heartbeat has gone stale. */
  private[tasks] def check(): Unit =
    if (isWedged() && fired.compareAndSet(false, true)) {
      val staleSeconds = (now() - lastBeatMillis()) / 1000
      logger.error(s"Worker heartbeat stale for ${staleSeconds}s (> ${stalenessThreshold.toSeconds}s) — the JVM is " +
        s"wedged (heartbeat thread starved/dead, e.g. a heap death-spiral); restarting the machine so Fly reschedules " +
        s"on the non-zero exit and a fresh boot clears the heap and reconnects Mongo.")
      onWedged()
    }

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(check()), checkEvery.toMillis, checkEvery.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"LivenessWatchdog armed: restart if the heartbeat is stale > ${stalenessThreshold.toSeconds}s (checked every ${checkEvery.toSeconds}s).")
  }

  def stop(): Unit = { scheduler.shutdownNow(); () }
}
