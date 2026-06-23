package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class ThrottleStuckWatchdogSpec extends AnyFlatSpec with Matchers {

  private class MutableSignal(var throttled: Boolean) extends ScrapeThrottleSignal {
    def isThrottled: Boolean = throttled
    def slowScrapeMillis: Long = 0L
  }

  private val start = Instant.parse("2026-06-23T00:00:00Z")

  private def watchdog(signal: ScrapeThrottleSignal, clock: () => Instant, onStuck: () => Unit = () => ()) =
    new ThrottleStuckWatchdog(signal, stuckAfter = 30.minutes, onStuck = onStuck, now = clock)

  "isStuck" should "be false while the worker is healthy" in {
    watchdog(new MutableSignal(throttled = false), () => start).isStuck() shouldBe false
  }

  it should "stay false until throttle has been continuously engaged past the threshold" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isStuck() shouldBe false                 // just engaged
    clock = start.plusSeconds(29 * 60)         // 29 min — under 30
    w.isStuck() shouldBe false
    clock = start.plusSeconds(31 * 60)         // 31 min — over the threshold
    w.isStuck() shouldBe true
  }

  it should "reset the timer when throttle releases, so only an UNBROKEN stretch counts" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    clock = start.plusSeconds(20 * 60); w.isStuck() shouldBe false // 20 min into the first stretch
    sig.throttled = false; w.isStuck() shouldBe false             // released — timer cleared
    sig.throttled = true
    clock = start.plusSeconds(40 * 60); w.isStuck() shouldBe false // a NEW stretch starts here, not at minute 0
    clock = start.plusSeconds(40 * 60 + 31 * 60)
    w.isStuck() shouldBe true                                      // 31 min into the new stretch
  }

  "check" should "fire onStuck exactly once when wedged, and never while healthy" in {
    val calls = new AtomicInteger(0)
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () })
    w.check(); calls.get() shouldBe 0          // engaged, but not yet stuck — no restart
    clock = start.plusSeconds(31 * 60)
    w.check(); w.check(); w.check()            // wedged — but the restart fires once, not on every tick
    calls.get() shouldBe 1
  }
}
