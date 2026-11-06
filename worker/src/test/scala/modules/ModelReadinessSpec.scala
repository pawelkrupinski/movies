package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ListingReadiness

/** The model's input gate is off (2026-11-06): prod held listings for hours on asks queued behind the agreement's own. */
class ModelReadinessSpec extends AnyFlatSpec with Matchers {
  "the model's input gate" should "be off: every version taken in at once, whatever its questions" in {
    WorkerWiring.ModelReadiness shouldBe ListingReadiness.Always
  }
}
