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
 * `stalenessThreshold`, exits the process (non-zero) so the pod is rescheduled. A
 * fresh boot re-establishes Mongo/HTTP and clears the heap — the same recovery the
 * manual restart gave on 2026-06-23.
 *
 * IT KEYS OFF "IS THE WORKER STILL TICKING", not "is it doing useful work" — the
 * heartbeat thread fires even when the reapers are deliberately idle, so a quiet
 * worker never reads as wedged.
 *
 * A second watchdog used to sit beside it, firing while an external CPU-throttle
 * signal stayed continuously on. It could not have caught the 2026-06-23 wedge,
 * which was a heap OOM: the OOM killed the credit poller's HTTP selector, so the
 * signal failed OPEN to "healthy" and the throttle watchdog never tripped while the
 * process limped on for ~2h answering a then-static `/health` 200. That whole
 * throttle path went with Fly; this one, which never depended on it, is what
 * remains.
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
