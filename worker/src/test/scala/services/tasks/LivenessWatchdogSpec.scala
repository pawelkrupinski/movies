package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class LivenessWatchdogSpec extends AnyFlatSpec with Matchers {

  private val start = 1_750_000_000_000L // arbitrary fixed wall-clock millis

  private def watchdog(beat: () => Long, clock: () => Long, onWedged: () => Unit = () => ()) =
    new LivenessWatchdog(
      lastBeatMillis = beat, stalenessThreshold = 5.minutes, onWedged = onWedged, now = clock)

  "isWedged" should "be false while the heartbeat is fresh" in {
    var clock = start
    val w = watchdog(beat = () => start, clock = () => clock)
    w.isWedged() shouldBe false
    clock = start + 4.minutes.toMillis // pulse 4 min old — under the 5-min threshold
    w.isWedged() shouldBe false
    w.isAlive shouldBe true
  }

  it should "be true once the heartbeat pulse is stale past the threshold" in {
    var clock = start
    val w = watchdog(beat = () => start, clock = () => clock) // pulse frozen at `start`
    clock = start + 5.minutes.toMillis + 1
    w.isWedged() shouldBe true
    w.isAlive shouldBe false
  }

  it should "treat a never-stamped pulse (0) as alive, not wedged" in {
    val w = watchdog(beat = () => 0L, clock = () => start + 1.hour.toMillis)
    w.isWedged() shouldBe false
  }

  it should "recover when the heartbeat resumes (a transient pause does not latch wedged)" in {
    var beat  = start
    var clock = start
    val w = watchdog(beat = () => beat, clock = () => clock)
    clock = start + 6.minutes.toMillis; w.isWedged() shouldBe true // pulse went stale
    beat  = clock                                                  // heartbeat ticks again
    w.isWedged() shouldBe false                                    // fresh pulse → alive
  }

  "check" should "fire onWedged exactly once when wedged, and never while alive" in {
    val calls = new AtomicInteger(0)
    var clock = start
    val w = watchdog(beat = () => start, clock = () => clock, onWedged = () => { calls.incrementAndGet(); () })
    w.check(); calls.get() shouldBe 0           // fresh — no restart
    clock = start + 5.minutes.toMillis + 1
    w.check(); w.check(); w.check()             // wedged — restart fires once, not per tick
    calls.get() shouldBe 1
  }
}
