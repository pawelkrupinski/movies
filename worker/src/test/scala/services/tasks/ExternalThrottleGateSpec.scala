package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExternalThrottleGateSpec extends AnyFlatSpec with Matchers {

  "ExternalThrottleGate" should "be off until pushed, then reflect the pushed state" in {
    val g = new ExternalThrottleGate
    g.isThrottled shouldBe false
    g.setThrottled(true);  g.isThrottled shouldBe true
    g.setThrottled(false); g.isThrottled shouldBe false
  }

  "ScrapeThrottleSignal.either" should "be throttled when EITHER source is (gate OR backstop)" in {
    val gate    = new ExternalThrottleGate
    val backstop = new ScrapeThrottleSignal { def isThrottled = false; def ewmaMillis = 0L }
    val both    = ScrapeThrottleSignal.either(gate, backstop)
    both.isThrottled shouldBe false
    gate.setThrottled(true)
    both.isThrottled shouldBe true // the external gate alone trips the reapers
  }

  // (The reaper-consumes-a-throttled-signal path is already covered by the per-reaper
  //  "back off … while throttled" tests; the gate is just another ScrapeThrottleSignal.)

  "ExternalThrottleGate.parse" should "read a state / throttled query param" in {
    ExternalThrottleGate.parse(Some("state=on"), "")        shouldBe Some(true)
    ExternalThrottleGate.parse(Some("state=off"), "")       shouldBe Some(false)
    ExternalThrottleGate.parse(Some("throttled=true"), "")  shouldBe Some(true)
    ExternalThrottleGate.parse(Some("throttled=false"), "") shouldBe Some(false)
  }

  it should "read a Grafana firing/resolved webhook body" in {
    ExternalThrottleGate.parse(None, """{"status":"firing","alerts":[]}""")   shouldBe Some(true)
    ExternalThrottleGate.parse(None, """{"status":"resolved","alerts":[]}""") shouldBe Some(false)
  }

  it should "return None for an unrecognised request (→ 400)" in {
    ExternalThrottleGate.parse(None, "")                shouldBe None
    ExternalThrottleGate.parse(Some("foo=bar"), "{}")   shouldBe None
  }
}
