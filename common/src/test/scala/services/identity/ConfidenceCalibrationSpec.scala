package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ConfidenceCalibration.{Calibration, Sample}

/** The rating gate's threshold comes from the labelled shadow diff, not from a constant: it is
 *  the cut that best separates the decisions the resolver got wrong from the ones it got right. */
class ConfidenceCalibrationSpec extends AnyFlatSpec with Matchers {

  private def right(c: Double*) = c.map(Sample(_, correct = true))
  private def wrong(c: Double*) = c.map(Sample(_, correct = false))

  "calibration" should "put the threshold where the wrong decisions end, when the data separates cleanly" in {
    val cal = ConfidenceCalibration.calibrate(wrong(0.1, 0.2, 0.3) ++ right(0.6, 0.8, 0.9)).get
    cal.threshold shouldBe 0.6
    cal.gates(0.3) shouldBe true
    cal.gates(0.6) shouldBe false
    (cal.gatedWrong, cal.gatedCorrect, cal.shownWrong, cal.shownCorrect) shouldBe (3, 0, 0, 3)
  }

  it should "minimise the misclassified decisions where the classes overlap, preferring to gate fewer on a tie" in {
    // Cuts (gate below): 0.2 → 2 wrong shown; 0.4 → 1 wrong shown; 0.5 → 1 right gated + 1 wrong
    // shown; 0.7 → 1 right gated; 0.9 → 2 right gated. 0.4 and 0.7 tie at one; 0.4 gates fewer.
    val cal = ConfidenceCalibration.calibrate(wrong(0.2, 0.5) ++ right(0.4, 0.7, 0.9)).get
    cal.threshold shouldBe 0.4
    (cal.gatedWrong, cal.gatedCorrect, cal.shownWrong, cal.shownCorrect) shouldBe (1, 0, 1, 3)
    // A tie between gating nothing (1 wrong shown) and gating one right one prefers showing.
    ConfidenceCalibration.calibrate(wrong(0.5) ++ right(0.5, 0.9)).get.gatedCorrect shouldBe 0
  }

  it should "gate nothing when the resolver was never wrong" in {
    val cal = ConfidenceCalibration.calibrate(right(0.2, 0.9)).get
    cal.gates(0.2) shouldBe false
    cal.gatedCorrect shouldBe 0
  }

  it should "have no threshold at all without labelled data — the gate then withholds nothing" in {
    ConfidenceCalibration.calibrate(Nil) shouldBe None
  }

  it should "be a function of the sample set, not its order" in {
    val samples = wrong(0.2, 0.5, 0.35) ++ right(0.4, 0.7, 0.9, 0.35)
    ConfidenceCalibration.calibrate(samples.reverse) shouldBe ConfidenceCalibration.calibrate(samples)
    ConfidenceCalibration.calibrate(scala.util.Random(7).shuffle(samples)) shouldBe ConfidenceCalibration.calibrate(samples)
  }

  "a calibration" should "gate every decision below the threshold" in {
    Calibration(0.5, 0, 0, 0, 0).gates(0.49) shouldBe true
    Calibration(0.5, 0, 0, 0, 0).gates(0.5) shouldBe false
  }
}
