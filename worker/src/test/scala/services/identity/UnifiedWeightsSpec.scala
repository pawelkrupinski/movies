package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The unified evidence model's fitted weights (`UnifiedWeights`, offline): how a probability and a decision explain themselves. */
class UnifiedWeightsSpec extends AnyFlatSpec with Matchers {

  "the unified weights" should "explain a probability as its signals' contributions" in {
    val weights = UnifiedWeights("t", Seq("model.logit", "family.imdb.took"), Seq(-1.0, 0.5, 2.0), cut = 0.9, l2 = 1.0, folds = 5, rows = 1)
    val features = Map("model.logit" -> 2.0, "family.imdb.took" -> 1.0)
    weights.logOdds(features) shouldBe 2.0
    weights.explain(features) shouldBe "family.imdb.took=1.00 +2.00 model.logit=2.00 +1.00"
  }

  "the hybrid" should "veto a contender a guard trips whatever it scores, unless the model's own rule took it, and explain both" in {
    val hybrid = UnifiedWeights("h", Seq("family.imdb.took", "rule.title"), Seq(0.0, 5.0, 5.0), cut = 0.9, l2 = 1.0, folds = 5, rows = 1,
      guards = UnifiedEvidence.Guards)
    val contradicted = Map("family.imdb.took" -> 1.0, "listing.contradicts" -> 1.0)
    hybrid.vetoes(contradicted) shouldBe Seq("listing.contradicts")
    hybrid.decision(contradicted) should startWith("vetoed by listing.contradicts; guards passed: bill.several")
    hybrid.vetoes(contradicted + ("rule.title" -> 1.0)) shouldBe empty
    hybrid.decision(Map("family.imdb.took" -> 1.0)) should startWith("taken 99.3% ≥ 90.0%; guards passed: bill.several, stage.work")
  }
}
