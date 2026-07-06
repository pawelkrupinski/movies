package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

import java.util.concurrent.atomic.AtomicInteger
import scala.io.Source

class CpuCreditPollerSpec extends AnyFlatSpec with Matchers {

  private def fixture(name: String): String =
    Source.fromResource(s"fixtures/fly-prometheus/$name").mkString

  // ── parseBalance: against REAL Fly Prometheus payloads ────────────────────

  "CpuCreditPoller.parseBalance" should "read the scalar from a real success response" in {
    CpuCreditPoller.parseBalance(fixture("cpu_balance_success.json")) shouldBe Some(148216.0187608)
  }

  it should "return None when the result vector is empty (series momentarily absent)" in {
    CpuCreditPoller.parseBalance(fixture("cpu_balance_empty.json")) shouldBe None
  }

  it should "return None on a non-success status, garbage, or empty body" in {
    CpuCreditPoller.parseBalance("""{"status":"error","error":"boom"}""") shouldBe None
    CpuCreditPoller.parseBalance("not json at all") shouldBe None
    CpuCreditPoller.parseBalance("") shouldBe None
  }

  // ── nextState: hysteresis around the 6000/12000 band ──────────────────────

  private val healthy = CpuCreditPoller.State(throttled = false, failures = 0)
  // step uses defaults (pollIntervalSeconds=30, lookaheadSeconds=600) so existing
  // floor-threshold tests are unaffected by the new projection parameter.
  private def step(prev: CpuCreditPoller.State, balance: Option[Double]) =
    CpuCreditPoller.nextState(prev, balance, enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3)

  "CpuCreditPoller.nextState" should "stay healthy while credit is abundant" in {
    // The exact false-positive case: ~100k credit must NEVER throttle, no matter
    // how slow scrapes ran (the poller doesn't read scrape timings at all).
    step(healthy, Some(106340)).throttled shouldBe false
  }

  it should "trip throttled once credit floors below the enter threshold" in {
    step(healthy, Some(5000)).throttled shouldBe true
  }

  it should "NOT trip when credit only dips into the hysteresis band from healthy" in {
    step(healthy, Some(9000)).throttled shouldBe false
  }

  it should "hold throttled through the hysteresis band, release above exitAbove when not projecting" in {
    val tripped = step(healthy, Some(5000))
    tripped.throttled shouldBe true
    step(tripped, Some(9000)).throttled shouldBe true   // still in band → hold
    step(tripped, Some(13000)).throttled shouldBe false // above exitAbove + gaining → release
  }

  it should "hold the prior decision on a single failed read, then fail open after the limit" in {
    val tripped = step(healthy, Some(5000))
    val f1 = step(tripped, None); f1.throttled shouldBe true; f1.failures shouldBe 1
    val f2 = step(f1, None);      f2.throttled shouldBe true; f2.failures shouldBe 2
    val f3 = step(f2, None);      f3.throttled shouldBe false // 3 failures → fail open
  }

  it should "reset the failure count on the next good read" in {
    val twoFails = step(step(healthy, None), None)
    twoFails.failures shouldBe 2
    step(twoFails, Some(5000)).failures shouldBe 0
  }

  // ── nextState: projection trigger (proactive, windowed/median-smoothed) ────

  // Fold a sequence of good readings through nextState from healthy, with the full
  // projection params (30 s poll, 600 s lookahead) so the median-of-deltas slope is
  // exercised over the real window.
  private def stepSeq(balances: Double*): CpuCreditPoller.State =
    balances.foldLeft(healthy) { (st, b) =>
      CpuCreditPoller.nextState(st, Some(b), enterBelow = 6000, exitAbove = 12000,
        maxConsecutiveFailures = 3, pollIntervalSeconds = 30.0, lookaheadSeconds = 600L)
    }

  it should "trip via projection on a SUSTAINED multi-poll downslope inside the lookahead window" in {
    // Reproduces the 2026-06-25 afternoon episode: ~3 500 ms/30 s = 116 ms/s sustained;
    // at 15 000 the floor is (15 000 − 6 000)/116 ≈ 77 s < 600 s.
    stepSeq(29000, 25500, 22000, 18500, 15000).throttled shouldBe true
  }

  it should "NOT trip on a single-poll drain spike at healthy credit (the 6 039 ms/s blip)" in {
    // The exact prod false positive: credit sat ~19 000, calm (~3 ms/s), then ONE poll
    // showed a ~6 000-unit drop (≈210 ms/s, floor in ~33 s by a single delta) before
    // recovering. The old single-delta projection tripped on that lone spike far above
    // the floor; the median-of-deltas over the window ignores it (median stays ~3 ms/s).
    stepSeq(19600, 19500, 19400, 19300, 13000).throttled shouldBe false
  }

  it should "NOT trip via projection before it has enough history to be robust" in {
    // Two steep-drop readings only (1 delta) — not enough for the median to reject a
    // spike, so projection stays disabled until the window fills; a single delta must
    // never trip it (that was the old bug).
    stepSeq(22000, 15000).throttled shouldBe false
  }

  it should "NOT trip via projection when the floor is still far away" in {
    // Sustained ~200 ms/30 s = 6.7 ms/s from 40 000; floor in ≈5 070 s >> 600 s.
    stepSeq(40800, 40600, 40400, 40200, 40000).throttled shouldBe false
  }

  it should "NOT trip via projection when balance is gaining (drain rate negative)" in {
    stepSeq(16000, 18000, 20000, 22000, 24000).throttled shouldBe false
  }

  it should "NOT trip via projection when lookahead is disabled (0)" in {
    // Same sustained drain as the first projection test, but lookahead=0 disables it.
    val st = Seq(29000.0, 25500, 22000, 18500, 15000).foldLeft(healthy) { (s, b) =>
      CpuCreditPoller.nextState(s, Some(b), 6000, 12000, 3, 30.0, lookaheadSeconds = 0L)
    }
    st.throttled shouldBe false
  }

  it should "preserve the slope window across a failed read so a transient gap doesn't reset it" in {
    // Build a sustained downslope, drop one read, then continue — the window must
    // survive so the projection still trips on the resumed downslope.
    val warm = stepSeq(29000, 25500, 22000)                       // 3 samples, 2 deltas — not yet tripping
    warm.recent shouldBe Vector(29000, 25500, 22000)
    val afterFail = CpuCreditPoller.nextState(warm, None, 6000, 12000, 3, 30.0, 600L)
    afterFail.recent shouldBe Vector(29000, 25500, 22000)          // window survives the gap
    afterFail.failures shouldBe 1
    // Good read after the gap completes a 3-delta sustained window → trips.
    val after = CpuCreditPoller.nextState(afterFail, Some(18500), 6000, 12000, 3, 30.0, 600L)
    after.throttled shouldBe true
  }

  it should "hold projection-triggered throttle while drain continues, release once it reverses (no oscillation)" in {
    // Bug guard: with the old exit-only-on-exitAbove logic, a projection-triggered
    // throttle above exitAbove would release on the very next poll then re-engage —
    // oscillating.  The fix ORs projectedLow into the exit condition so throttle holds
    // as long as the drain is still threatening.
    stepSeq(29000, 25500, 22000, 18500, 15000, 11500).throttled shouldBe true
    // Drain reverses and balance climbs back above exitAbove → releases.
    stepSeq(29000, 25500, 22000, 18500, 15000, 20000, 24000, 26000).throttled shouldBe false
  }

  // ── pollOnce: end-to-end through the HTTP seam with a real fixture ─────────

  private class StubFetch(body: => String) extends GetOnlyHttpFetch {
    override def get(url: String): String = body
  }

  "CpuCreditPoller.pollOnce" should "drive isThrottled from the live Prometheus read" in {
    val poller = new CpuCreditPoller(new StubFetch(fixture("cpu_balance_success.json")), token = "t")
    poller.isThrottled shouldBe false // healthy until first poll
    poller.pollOnce()
    poller.isThrottled shouldBe false // ~148k credit → healthy

    val starved = new CpuCreditPoller(new StubFetch("""{"status":"success","data":{"result":[{"value":[1,"4200"]}]}}"""), token = "t")
    starved.pollOnce()
    starved.isThrottled shouldBe true
  }

  it should "stay healthy when the HTTP read throws (fail-open, gate covers)" in {
    val poller = new CpuCreditPoller(new StubFetch(throw new RuntimeException("network down")), token = "t")
    poller.pollOnce()
    poller.isThrottled shouldBe false
  }

  // ── onProjectionThrottle: downslope restart callback ──────────────────────

  // Drives the poller through a SUSTAINED downslope (~116 ms/s over the window) so the
  // median-smoothed projection genuinely trips.  StubFetch's body is by-name, so each
  // get() advances the iterator to the next reading.
  private def projectingPoller(calls: AtomicInteger): CpuCreditPoller = {
    val readings = Iterator(29000, 25500, 22000, 18500, 15000)
      .map(b => s"""{"status":"success","data":{"result":[{"value":[1,"$b"]}]}}""")
    new CpuCreditPoller(
      new StubFetch(readings.next()),
      token = "t",
      onProjectionThrottle = () => { calls.incrementAndGet(); () }
    )
  }

  "CpuCreditPoller.pollOnce" should "call onProjectionThrottle exactly once when a sustained downslope trips projection" in {
    val calls = new AtomicInteger(0)
    val poller = projectingPoller(calls)
    poller.pollOnce(); poller.pollOnce(); poller.pollOnce()   // 29 000, 25 500, 22 000 — 2 deltas, warming
    calls.get() shouldBe 0
    poller.pollOnce()                // 18 500 — 3-delta sustained window → projection fires
    poller.isThrottled shouldBe true
    calls.get() shouldBe 1
    // A further draining poll stays throttled — no new transition, so no second alarm.
    poller.pollOnce()                // 15 000
    calls.get() shouldBe 1
  }

  it should "NOT call onProjectionThrottle on a floor-triggered throttle (balance below enterBelow)" in {
    val calls  = new AtomicInteger(0)
    val poller = new CpuCreditPoller(
      new StubFetch("""{"status":"success","data":{"result":[{"value":[1,"4200"]}]}}"""),
      token = "t",
      onProjectionThrottle = () => { calls.incrementAndGet(); () }
    )
    poller.pollOnce()               // balance 4200 < enterBelow=6000 — floor trigger, not projection
    poller.isThrottled shouldBe true
    calls.get() shouldBe 0         // callback must not fire on floor trigger
  }

  it should "NOT call onProjectionThrottle when still healthy (floor is far away)" in {
    val calls = new AtomicInteger(0)
    // Drain 200 ms/30 s = 6.7 ms/s; floor in ~5 070 s >> 600 s lookahead.
    val readings = Iterator(
      """{"status":"success","data":{"result":[{"value":[1,"40200"]}]}}""",
      """{"status":"success","data":{"result":[{"value":[1,"40000"]}]}}"""
    )
    val poller = new CpuCreditPoller(
      new StubFetch(readings.next()), token = "t",
      onProjectionThrottle = () => { calls.incrementAndGet(); () }
    )
    poller.pollOnce()
    poller.pollOnce()
    poller.isThrottled shouldBe false
    calls.get() shouldBe 0
  }
}
