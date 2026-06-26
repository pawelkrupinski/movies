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
    signal:         ScrapeThrottleSignal,
    clock:          () => Instant,
    onStuck:        () => Unit            = () => (),
    creditBalance:  () => Option[Double]  = () => None,
    floorThreshold: Double                = 1000.0,
    floorStuckAfter: FiniteDuration       = 15.minutes
  ) =
    new ThrottleStuckWatchdog(signal, stuckAfter = 30.minutes, onStuck = onStuck,
      creditBalance = creditBalance, floorThreshold = floorThreshold,
      floorStuckAfter = floorStuckAfter, now = clock)

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

  "isFloorStuck" should "be false while not throttled" in {
    val sig = new MutableSignal(throttled = false)
    val w = watchdog(sig, () => start)
    w.isFloorStuck(Some(2.0)) shouldBe false
  }

  it should "be false while throttled but credit above the floor threshold" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isFloorStuck(Some(5000.0)) shouldBe false
    clock = start.plusSeconds(20 * 60)
    w.isFloorStuck(Some(5000.0)) shouldBe false
  }

  it should "be false while credit is below threshold but not yet past floorStuckAfter" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isFloorStuck(Some(2.0)) shouldBe false       // just entered the floor zone
    clock = start.plusSeconds(14 * 60)
    w.isFloorStuck(Some(2.0)) shouldBe false       // 14 min — still under the 15-min threshold
  }

  it should "be true once credit has been at the floor for floorStuckAfter" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isFloorStuck(Some(2.0)) shouldBe false       // anchor
    clock = start.plusSeconds(16 * 60)
    w.isFloorStuck(Some(2.0)) shouldBe true        // 16 min at floor
  }

  it should "reset if credit rises above the floor threshold" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isFloorStuck(Some(2.0))                      // anchor at floor
    clock = start.plusSeconds(10 * 60)
    w.isFloorStuck(Some(8000.0)) shouldBe false    // credit recovered above threshold — reset
    // A new floor episode must wait another floorStuckAfter from scratch.
    clock = start.plusSeconds(11 * 60)
    w.isFloorStuck(Some(2.0)) shouldBe false       // only 1 min into the new floor episode
  }

  it should "reset if throttle releases" in {
    val sig = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock)
    w.isFloorStuck(Some(2.0))                      // anchor at floor
    clock = start.plusSeconds(10 * 60)
    sig.throttled = false
    w.isFloorStuck(Some(2.0)) shouldBe false       // throttle released — reset
    sig.throttled = true
    clock = start.plusSeconds(11 * 60)
    w.isFloorStuck(Some(2.0)) shouldBe false       // 1 min into the new floor episode
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
    // 2.0 is above the default floorThreshold=1000? No, 2.0 < 1000 — so floor fast-path
    // would fire at 15 min. We want to test the main stuck path here, so use a balance
    // in the hysteresis zone that isn't at the literal floor.
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(5000.0)) // in hysteresis band, pinned flat — no upward trend
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
    var bal   = 5000.0
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()                                                    // anchor + first sample at minute 0 (pruned by minute 31)
    clock = start.plusSeconds(31 * 60); bal = 9000; w.check()   // stuck; a lone late spike is too short a span to trust
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

  // Floor fast-path tests: these verify the 2026-06-26 incident scenario where
  // the machine sat at credit≈2 for 89 min. The fast-path restarts at 15 min
  // WITHOUT waiting for the 30-min stuckAfter or applying the trend guard.

  it should "restart via floor fast-path after floorStuckAfter at true-floor credit" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    var bal   = 2.0  // credit at the literal floor (well below default floorThreshold=1000)
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()                              // anchor; clock=0, bal=2 → floor episode starts
    clock = start.plusSeconds(14 * 60)
    w.check(); calls.get() shouldBe 0     // 14 min — not yet at floorStuckAfter=15
    clock = start.plusSeconds(16 * 60)
    w.check(); calls.get() shouldBe 1     // 16 min at floor → fast-path fires (no 30-min wait)
  }

  it should "fire floor fast-path BEFORE the main stuckAfter elapses" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(5.0))
    w.check()
    clock = start.plusSeconds(16 * 60)    // 16 min: floor fast-path fires; stuckAfter=30 not yet elapsed
    w.check(); calls.get() shouldBe 1
  }

  it should "NOT fire floor fast-path when credit is above floorThreshold in hysteresis zone" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    // 8000 is in the hysteresis zone (above enterBelow=6000, below exitAbove=12000) but
    // well above the floor threshold=1000 — floor path must not trigger here.
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(8000.0))
    w.check()
    clock = start.plusSeconds(20 * 60)
    w.check(); calls.get() shouldBe 0     // 20 min — above floor, only main path applies (30 min)
  }

  it should "fire floor fast-path even when credit oscillates (tiny spikes don't defer it)" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    // Simulate the 2→560→2→3→464→2→184→2 oscillations observed on 2026-06-26.
    val readings = Iterator.continually(Seq(2.0, 560.0, 2.0, 3.0, 464.0, 2.0, 184.0, 2.0)).flatten
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(readings.next()))
    w.check()                                   // anchor, bal=2
    // Drive 1-min ticks, credit oscillating 2-560-2-... below 1000 throughout.
    for (m <- 1 to 14) {
      clock = start.plusSeconds(m * 60)
      w.check()
    }
    calls.get() shouldBe 0                      // 14 min — not yet past floorStuckAfter=15
    clock = start.plusSeconds(16 * 60)
    w.check(); calls.get() shouldBe 1           // 16 min → fast-path fires despite oscillations
  }

  it should "NOT fire floor fast-path when credit temporarily dips below threshold then recovers" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    var bal   = 500.0
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(bal))
    w.check()                                   // anchor, floor episode starts at t=0
    clock = start.plusSeconds(5 * 60); bal = 5000.0
    w.check()                                   // recovered above threshold — floor timer resets
    clock = start.plusSeconds(16 * 60)          // 11 min since the NEW floor start (not enough)
    bal = 500.0; w.check()
    calls.get() shouldBe 0                      // still under 15 min since recovery reset the clock
  }

  it should "fire exactly once across both paths even when check is called many times" in {
    val calls = new AtomicInteger(0)
    val sig   = new MutableSignal(throttled = true)
    var clock = start
    val w = watchdog(sig, () => clock, onStuck = () => { calls.incrementAndGet(); () },
      creditBalance = () => Some(2.0))
    w.check()
    clock = start.plusSeconds(20 * 60)
    // Both paths are now eligible: floor (20 > 15) and stuck (20 < 30 — not quite, but floor fires)
    w.check(); w.check(); w.check(); w.check()
    calls.get() shouldBe 1
  }
}
