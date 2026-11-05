package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The counterfactual predictor ([[FamilyCounterfactual]]) over the committed Lichtblick "Buddies" slice, a known flip:
 *  some listing of a multi-listing family CAN flip, and each row's trigger names a perturbation. */
class FamilyCounterfactualSpec extends AnyFlatSpec with Matchers {
  "The predictor" should "mark listings of a known-flip family as able to flip, naming the trigger" in {
    val corpus = FamilyConvergence.sliceCorpus(Path.of("test", "resources", "fixtures", "drift-slices", "de/programmkino-lichtblick-e-v-buddies-69181215"), "r")
    val rows   = FamilyCounterfactual.rows(corpus.listings, corpus.lookups, corpus.reads.normalizer, corpus.inputs, threads = 2, maxAdds = 10)
    rows should not be empty
    rows.filter(_.canFlip).foreach(_.firstTrigger.get should (startWith("-") or startWith("+")))
    rows.forall(r => r.canFlip == r.firstTrigger.isDefined) shouldBe true
  }
}
