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

  private def watchdog(
    signal:        ScrapeThrottleSignal,
    clock:         () => Instant,
    onStuck:       () => Unit            = () => (),
    creditBalance: () => Option[Double]  = () => None
  ) =
    new ThrottleStuckWatchdog(signal, stuckAfter = 30.minutes, onStuck = onStuck,
      creditBalance = creditBalance, now = clock)

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

  // `check` anchors the throttled-stretch start at its FIRST call, so each test
  // below anchors with a check at `start`, then drives the clock toward minute 31
  // (past the 30-min threshold). The trend is judged over the last 10 min, so the
  // pre-threshold samples (minutes 22, 26) are what populate the window at minute 31.

  it should "NOT restart while wedged if credit is trending up — recovery is underway" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    var bal   = 1000.0
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()                                                   // anchor the stretch at minute 0
    clock = start.plusSeconds(22 * 60); bal = 1000; w.check()
    clock = start.plusSeconds(26 * 60); bal = 800;  w.check()   // a dip — still net up overall
    clock = start.plusSeconds(31 * 60); bal = 3000; w.check()   // now stuck (31 > 30), net rise 1000→3000
    calls.get() shouldBe 0
  }

  it should "restart when wedged and credit is flat (a true wedge, not recovering)" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(2.0)) // pinned at the floor — no upward trend
    w.check()
    clock = start.plusSeconds(22 * 60); w.check()
    clock = start.plusSeconds(26 * 60); w.check()
    clock = start.plusSeconds(31 * 60); w.check()
    calls.get() shouldBe 1
  }

  it should "restart when wedged and credit is declining" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    var bal   = 3000.0
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()
    clock = start.plusSeconds(22 * 60); bal = 3000; w.check()
    clock = start.plusSeconds(26 * 60); bal = 2000; w.check()
    clock = start.plusSeconds(31 * 60); bal = 1000; w.check() // net DOWN → not recovering → restart
    calls.get() shouldBe 1
  }

  it should "not be fooled into deferring by two samples a minute apart (needs a real span)" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    var bal   = 1000.0
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()                                                  // anchor + first sample at minute 0 (pruned by minute 31)
    clock = start.plusSeconds(31 * 60); bal = 9000; w.check()  // stuck; a lone late spike is too short a span to trust
    calls.get() shouldBe 1
  }

  it should "fall back to restarting when no credit source is available" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () }) // creditBalance = None
    w.check()
    clock = start.plusSeconds(31 * 60); w.check()
    calls.get() shouldBe 1
  }
}
