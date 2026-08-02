package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{ExternalThrottleGate, LivenessWatchdog}

import scala.concurrent.duration._

/**
 * The multi-country folds behind `/health` and `/throttle`. Every case puts the
 * interesting country SECOND or LAST: a fleet that reads `wirings.head` (what
 * both endpoints did before) passes a first-country-only test by accident, so
 * only a non-primary country proves the fold actually happens.
 */
class WorkerFleetSpec extends AnyFlatSpec with Matchers {

  private val start = 1_750_000_000_000L // arbitrary fixed wall-clock millis

  /** A watchdog whose pulse is `ageMillis` old against a frozen clock. */
  private def watchdog(ageMillis: Long) =
    new LivenessWatchdog(
      lastBeatMillis     = () => start - ageMillis,
      stalenessThreshold = 5.minutes,
      onWedged           = () => (),
      now                = () => start)

  private def fresh = watchdog(1.minute.toMillis)
  private def stale = watchdog(6.minutes.toMillis)

  "isAlive" should "be true while every country's heartbeat is fresh" in {
    new WorkerFleet(Seq(fresh, fresh, fresh), Nil).isAlive shouldBe true
  }

  it should "be false when a NON-PRIMARY country's heartbeat is stale" in {
    // pl fresh, de fresh, uk wedged — the shape `wirings.head` reported as healthy
    new WorkerFleet(Seq(fresh, fresh, stale), Nil).isAlive shouldBe false
  }

  it should "be false when the primary country's heartbeat is stale" in {
    new WorkerFleet(Seq(stale, fresh, fresh), Nil).isAlive shouldBe false
  }

  it should "be true for a single-country worker with a fresh pulse" in {
    new WorkerFleet(Seq(fresh), Nil).isAlive shouldBe true
  }

  "setThrottled" should "back off EVERY country, not just the primary" in {
    val gates = Seq.fill(3)(new ExternalThrottleGate)
    new WorkerFleet(Nil, gates).setThrottled(true)
    gates.map(_.isThrottled) shouldBe Seq(true, true, true)
  }

  it should "clear the throttle on every country" in {
    val gates = Seq.fill(3)(new ExternalThrottleGate)
    gates.foreach(_.setThrottled(true))
    new WorkerFleet(Nil, gates).setThrottled(false)
    gates.map(_.isThrottled) shouldBe Seq(false, false, false)
  }

  it should "be a no-op on an empty fleet rather than throwing" in {
    noException should be thrownBy new WorkerFleet(Nil, Nil).setThrottled(true)
  }
}
