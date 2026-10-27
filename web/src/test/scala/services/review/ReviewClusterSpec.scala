package services.review

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReviewClusterSpec extends AnyFlatSpec with Matchers {
  import ReviewFixtures._

  private val clusters = Seq(heldDecision, vetoedDecision, nothingDecision, matchedDecision).map(ReviewCluster.of(Country.Poland, _))
  private def titles(cs: Seq[ReviewCluster]) = cs.map(_.title)

  "a decision" should "read its candidates from the resolver's explanation, best first" in {
    val held = ReviewCluster.of(Country.Poland, heldDecision)
    held.candidates.head shouldBe ReviewCandidate(1157322, Some(0.864))
    held.candidates.map(_.film) shouldBe Seq(1157322)        // own match and stored candidate are the same film
    val vetoed = ReviewCluster.of(Country.Poland, vetoedDecision)
    vetoed.best shouldBe Some(ReviewCandidate(1703622, Some(0.625), vetoed = true, Some("another house's season production")))
    ReviewCluster.of(Country.Poland, nothingDecision).best shouldBe None
    ReviewCluster.of(Country.Poland, matchedDecision).candidates shouldBe empty   // the match is not its own candidate
  }

  it should "lead with the best candidate no veto denied, never the vetoed one, and keep the vetoed apart with their reasons" in {
    val odblask = ReviewCluster.of(Country.Poland, odblaskDecision)
    odblask.lead.map(_.film) shouldBe Some(16878)
    odblask.shown shouldBe Some(16878)
    odblask.vetoedCandidates shouldBe Seq(ReviewCandidate(850957, Some(0.289), vetoed = true,
      Some("its title names it only by a programme tag billed beside many titles")))
  }

  it should "put no film forward when every candidate it weighed was vetoed" in {
    val macbeth = ReviewCluster.of(Country.Poland, vetoedDecision)
    macbeth.lead shouldBe None
    macbeth.shown shouldBe None
    macbeth.vetoedCandidates.map(_.film) shouldBe Seq(1703622)
    macbeth.best.map(_.film) shouldBe Some(1703622)   // still what the queue sorts by
  }

  it should "keep a stable id however its members are ordered" in {
    val a = heldDecision.copy(members = Seq(Held, Matched))(services.identity.DecisionTrace.Empty)
    val b = heldDecision.copy(members = Seq(Matched, Held))(services.identity.DecisionTrace.Empty)
    ReviewCluster.of(Country.Poland, a).id shouldBe ReviewCluster.of(Country.Poland, b).id
    ReviewCluster.of(Country.Poland, a).id should have length 16
  }

  "the queue" should "list unmatched clusters, of one listing each the most confident candidate first, and leave an own take out" in {
    titles(ReviewSelection.queue(clusters, Map.empty, JudgedListings.none).map(_._1)) shouldBe
      Seq("FRANZ KAFKA", "Macbeth", "Filmowe popołudnie dla dzieci: Robaczki")
  }
}
