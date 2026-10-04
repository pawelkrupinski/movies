package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.LivenessWatchdog

import scala.concurrent.duration._

/**
 * The multi-country fold behind `/health`. Every case puts the interesting country
 * SECOND or LAST: a fleet that reads `wirings.head` (what the endpoint did before)
 * passes a first-country-only test by accident, so only a non-primary country
 * proves the fold actually happens.
 *
 * A `setThrottled` fold was tested here too, for the same reason. It applied an
 * externally-pushed CPU-credit back-off to every country; that whole path went with
 * Fly.
 */
class WorkerFleetSpec extends AnyFlatSpec with Matchers {

  private val start = 1_750_000_000_000L // arbitrary fixed wall-clock millis

  /** A watchdog whose pulse is `ageMillis` old against a frozen clock. */
  private def watchdog(ageMillis: Long) =
    new LivenessWatchdog(
      lastBeatMillis     = () => start - ageMillis,
      stalenessThreshold = settings.LivenessStaleAfter(5.minutes),
      onWedged           = () => (),
      now                = () => start)

  private def fresh = watchdog(1.minute.toMillis)
  private def stale = watchdog(6.minutes.toMillis)

  private def recovered = new services.DatabaseBinding { def boundDegraded = true; def restartRequired = true }

  // A country that booted into an unreachable Mongo wired every repository to no database; its
  // reconnect cannot rebind them, so once Mongo answers the process asks to be restarted.
  "isAlive" should "be false once a country's Mongo recovered under repositories wired without it" in {
    new WorkerFleet(Seq(fresh, fresh), Seq(services.DatabaseBinding.Bound, recovered)).isAlive shouldBe false
    new WorkerFleet(Seq(fresh, fresh), Seq(services.DatabaseBinding.Bound, services.DatabaseBinding.Bound)).isAlive shouldBe true
  }

  "isAlive" should "be true while every country's heartbeat is fresh" in {
    new WorkerFleet(Seq(fresh, fresh, fresh)).isAlive shouldBe true
  }

  it should "be false when a NON-PRIMARY country's heartbeat is stale" in {
    // pl fresh, de fresh, uk wedged — the shape `wirings.head` reported as healthy
    new WorkerFleet(Seq(fresh, fresh, stale)).isAlive shouldBe false
  }

  it should "be false when the primary country's heartbeat is stale" in {
    new WorkerFleet(Seq(stale, fresh, fresh)).isAlive shouldBe false
  }

  it should "be true for a single-country worker with a fresh pulse" in {
    new WorkerFleet(Seq(fresh)).isAlive shouldBe true
  }
}
