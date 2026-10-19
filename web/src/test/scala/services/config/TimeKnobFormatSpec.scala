package services.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TimeKnobFormatSpec extends AnyFlatSpec with Matchers {

  "humanize" should "read a millisecond knob over 1s out in seconds" in {
    TimeKnobFormat.humanize("KINOWO_FOO_MS", "5000") shouldBe Some("5 s")
    TimeKnobFormat.humanize("KINOWO_FOO_MILLIS", "1500") shouldBe Some("1.5 s")
  }

  it should "read a second knob over 1min out in minutes" in {
    TimeKnobFormat.humanize("KINOWO_TICK_SECONDS", "60") shouldBe Some("1 min")
    TimeKnobFormat.humanize("KINOWO_TICK_SECONDS", "90") shouldBe Some("1.5 min")
  }

  it should "not annotate values below the threshold (raw is already clearest)" in {
    TimeKnobFormat.humanize("KINOWO_FOO_MS", "999") shouldBe None       // under 1s
    TimeKnobFormat.humanize("KINOWO_TICK_SECONDS", "30") shouldBe None  // under 1min
  }

  it should "not annotate non-time keys or non-numeric values" in {
    TimeKnobFormat.humanize("KINOWO_MAX_ENQUEUE_PER_TICK", "5000") shouldBe None
    TimeKnobFormat.humanize("KINOWO_TICK_SECONDS", "soon") shouldBe None
  }
}
