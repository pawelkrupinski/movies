package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ListingReadiness

/** The model's input gate is off (2026-11-06): its holds in DE and US ended only at the 10-minute bound. */
class ModelReadinessSpec extends AnyFlatSpec with Matchers {
  "the model's input gate" should "be off: every version taken in at once, whatever its questions" in {
    WorkerWiring.modelReadiness(ListingReadiness.Always, ListingReadiness.Always) shouldBe ListingReadiness.Always
  }
}
