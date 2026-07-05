package services.tasks

import play.api.Logging

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration.FiniteDuration

/**
 * Rate-limits the machine-restart response to a genuine CPU-credit WEDGE so it can
 * clear an intermittent spin without ever looping the machine.
 *
 * Background: the throttle-restart was dropped 2026-07-03 because a sustained
 * credit floor was read as a STRUCTURAL deficit (steady CPU just over the
 * shared-cpu earn rate) that a reboot can't clear — restarting only burned the
 * ~16k boot re-grant and re-drained, an ~18-45min self-inflicted loop. But
 * 2026-07-05 proved the floor is often a client-side JDK-NIO2 epoll BUSY-SPIN on a
 * wedged Mongo socket: ~15-24cc of dead-flat wasted CPU that a reboot (fresh
 * sockets) DOES clear. The Netty transport removes that spin at the source; this
 * policy is the safety net for any residual wedge (spin, or another cause a fresh
 * boot clears).
 *
 * The loop the 2026-07-03 removal feared is defeated by a COOLDOWN persisted
 * ACROSS the reboot it gates: once a wedge-restart fires, another can't fire until
 * `cooldown` has elapsed (default 2h — long enough for the ~16k re-grant to refill
 * toward healthy at +5-7/s). Within the cooldown a still-wedged worker only ALARMS
 * and rides it out on the reaper backoff, so the worst case is one reboot per
 * cooldown, never a spiral. The caller (the [[ThrottleStuckWatchdog]]) has already
 * gated on "throttled continuously > stuckAfter AND credit not trending up", so
 * `onWedge` is only ever reached on a real, non-recovering wedge.
 *
 * `lastRestartAt` / `recordRestart` are the persisted-marker seam (a file on the
 * `/data` volume in prod); `restart` is the machine-restart primitive; `now` is
 * injectable — so a test drives the decision without a clock, a real `sys.exit`,
 * or touching disk.
 */
class WedgeRestartPolicy(
  cooldown:      FiniteDuration,
  lastRestartAt: () => Option[Instant],
  recordRestart: Instant => Unit,
  restart:       String => Unit,
  now:           () => Instant = () => Instant.now()
) extends Logging {

  /** Restart on a genuine wedge, unless a wedge-restart already fired within
   *  `cooldown` — then alarm and ride it out (a re-wedge must not loop the box). */
  def onWedge(reason: String): Unit =
    lastRestartAt().map(t => JDuration.between(t, now()).toMillis) match {
      case Some(sinceMs) if sinceMs < cooldown.toMillis =>
        logger.error(s"CPU-credit wedge ($reason), but a wedge-restart fired ${sinceMs / 60000}min ago — within the " +
          s"${cooldown.toMinutes}min cooldown; NOT restarting again so a re-wedge can't loop the machine. Riding it " +
          s"out on the reaper backoff (Netty transport should have prevented the spin; this is the residual-wedge net).")
      case _ =>
        recordRestart(now())
        restart(reason)
    }
}
