package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

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

  it should "hold throttled through the band, then release only above the exit threshold" in {
    val tripped = step(healthy, Some(5000))
    tripped.throttled shouldBe true
    step(tripped, Some(9000)).throttled shouldBe true   // still in band → hold
    step(tripped, Some(13000)).throttled shouldBe false // cleared exitAbove → release
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

  // ── nextState: projection trigger (proactive) ─────────────────────────────

  // Helper that drives two consecutive readings with full projection params.
  private def stepWithProjection(
    prevBalance: Double, currBalance: Double,
    pollSecs: Double = 30.0, lookahead: Long = 600L
  ): CpuCreditPoller.State = {
    val after1 = CpuCreditPoller.nextState(
      healthy, Some(prevBalance),
      enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3,
      pollIntervalSeconds = pollSecs, lookaheadSeconds = lookahead
    )
    CpuCreditPoller.nextState(
      after1, Some(currBalance),
      enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3,
      pollIntervalSeconds = pollSecs, lookaheadSeconds = lookahead
    )
  }

  it should "trip via projection when drain rate puts the floor inside the lookahead window" in {
    // Reproduces the 2026-06-25 afternoon episode: balance 22 000 ms draining at
    // ~3 500 ms/30 s = 116 ms/s; time to 6 000 = (22 000 – 6 000)/116 ≈ 138 s < 600 s.
    stepWithProjection(prevBalance = 25500, currBalance = 22000).throttled shouldBe true
  }

  it should "NOT trip via projection when the floor is still far away" in {
    // Balance 40 000 ms dropping 200 ms/30 s = 6.7 ms/s; time to 6 000 ≈ 5 070 s >> 600 s.
    stepWithProjection(prevBalance = 40200, currBalance = 40000).throttled shouldBe false
  }

  it should "NOT trip via projection when balance is gaining (drain rate negative)" in {
    stepWithProjection(prevBalance = 20000, currBalance = 22000).throttled shouldBe false
  }

  it should "NOT trip via projection when lookahead is disabled (0)" in {
    // Same drain as the first projection test, but lookahead=0 disables projection.
    stepWithProjection(prevBalance = 25500, currBalance = 22000, lookahead = 0L).throttled shouldBe false
  }

  it should "preserve prevBalance across a failed read so the slope window survives a transient gap" in {
    val after1 = CpuCreditPoller.nextState(
      healthy, Some(25500),
      enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3
    )
    after1.prevBalance shouldBe Some(25500)
    // One failed read — prevBalance must survive.
    val afterFail = CpuCreditPoller.nextState(
      after1, None,
      enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3
    )
    afterFail.prevBalance shouldBe Some(25500)
    // Good read after the gap — projection still computes correctly.
    val after3 = CpuCreditPoller.nextState(
      afterFail, Some(22000),
      enterBelow = 6000, exitAbove = 12000, maxConsecutiveFailures = 3,
      pollIntervalSeconds = 30.0, lookaheadSeconds = 600L
    )
    after3.throttled shouldBe true
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
}
