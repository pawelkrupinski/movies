package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

/**
 * The cooldown contract: a genuine wedge restarts, but a second wedge WITHIN the
 * cooldown only alarms — so a re-wedge (or a structural floor a reboot can't clear)
 * can never loop the machine, the ~18-45min spiral the 2026-07-03 restart-removal
 * fought. The marker seam is in-memory here; `now` is injected so the cooldown
 * boundary is exercised without waiting.
 */
class WedgeRestartPolicySpec extends AnyFlatSpec with Matchers {

  private val Cooldown = 120.minutes
  private val T0       = Instant.parse("2026-07-05T12:00:00Z")

  /** A policy over a mutable in-memory marker + a fixed clock, recording restarts. */
  private class Harness(nowAt: Instant) {
    var marker:   Option[Instant] = None
    var restarts: Vector[String]  = Vector.empty
    val policy = new WedgeRestartPolicy(
      cooldown      = Cooldown,
      lastRestartAt = () => marker,
      recordRestart = at => marker = Some(at),
      restart       = reason => restarts :+= reason,
      now           = () => nowAt)
  }

  "WedgeRestartPolicy" should "restart on the first wedge (no prior restart recorded)" in {
    val h = new Harness(T0)
    h.policy.onWedge("stuck watchdog")
    h.restarts shouldBe Vector("stuck watchdog")
    h.marker   shouldBe Some(T0)  // the restart is stamped so the cooldown survives the reboot
  }

  it should "NOT restart again when a restart fired within the cooldown (rides it out instead)" in {
    val h = new Harness(T0)
    h.marker = Some(T0.minusSeconds(30.minutes.toSeconds))  // 30min ago, inside the 120min cooldown
    h.policy.onWedge("stuck watchdog")
    h.restarts shouldBe empty
    h.marker   shouldBe Some(T0.minusSeconds(30.minutes.toSeconds))  // untouched — no new restart stamped
  }

  it should "restart again once the cooldown has fully elapsed" in {
    val h = new Harness(T0)
    h.marker = Some(T0.minusSeconds(121.minutes.toSeconds))  // just past the 120min cooldown
    h.policy.onWedge("stuck watchdog")
    h.restarts shouldBe Vector("stuck watchdog")
    h.marker   shouldBe Some(T0)
  }

  it should "restart at the exact cooldown boundary (>= cooldown is the restart threshold)" in {
    val h = new Harness(T0)
    h.marker = Some(T0.minusSeconds(Cooldown.toSeconds))  // exactly cooldown ago → cooldown elapsed
    h.policy.onWedge("stuck watchdog")
    h.restarts shouldBe Vector("stuck watchdog")
  }
}
