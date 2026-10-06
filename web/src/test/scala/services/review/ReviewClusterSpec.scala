package services.review

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ResolverDecision.Basis
import services.movies.ListingKey

import java.time.Instant

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
    val card = ReviewCards.build(odblaskSource, Seq(odblask -> None), Nil, new ReviewAnswers.Index(Nil)).head
    card.shown shouldBe Some(16878)
    card.otherCandidates.map(_.film) should not contain 850957
    card.vetoedCandidates.map(_.film) shouldBe Seq(850957)
    (card.payload(ReviewPage.Queue) \ "shown" \ "ref").as[String] shouldBe "tmdb:16878"
  }

  it should "put no film forward when every candidate it weighed was vetoed" in {
    val macbeth = ReviewCluster.of(Country.Poland, vetoedDecision)
    macbeth.lead shouldBe None
    macbeth.shown shouldBe None
    macbeth.vetoedCandidates.map(_.film) shouldBe Seq(1703622)
    macbeth.best.map(_.film) shouldBe Some(1703622)   // still what the queue and the matchable page sort by
  }

  it should "keep a stable id however its members are ordered" in {
    val a = heldDecision.copy(members = Seq(Held, Matched))(services.identity.DecisionTrace.Empty)
    val b = heldDecision.copy(members = Seq(Matched, Held))(services.identity.DecisionTrace.Empty)
    ReviewCluster.of(Country.Poland, a).id shouldBe ReviewCluster.of(Country.Poland, b).id
    ReviewCluster.of(Country.Poland, a).id should have length 16
  }

  "the queue" should "list unmatched clusters, the most confident candidate first" in {
    titles(ReviewSelection.queue(clusters)) shouldBe Seq("FRANZ KAFKA", "Macbeth", "Filmowe popołudnie dla dzieci: Robaczki")
  }

  "the matchable page" should "list unmatched clusters whose best candidate the resolver rated at the threshold or more" in {
    titles(ReviewSelection.matchable(clusters, 0.5, None)) shouldBe Seq("FRANZ KAFKA", "Macbeth")
    titles(ReviewSelection.matchable(clusters, 0.7, None)) shouldBe Seq("FRANZ KAFKA")
    titles(ReviewSelection.matchable(clusters, 0.5, Some(Basis.Vetoed))) shouldBe Seq("Macbeth")
    ReviewSelection.matchableByBasis(clusters, 0.5).toMap shouldBe Map(Basis.BelowThreshold -> 1, Basis.Vetoed -> 1)
  }

  it should "leave out a cluster that fell back to another database's film" in {
    val fellBack = heldDecision.copy(fallback = Some(services.identity.ResolverDecision.Fallback("imdb", "tt1", 0.9)))(services.identity.DecisionTrace.Empty)
    ReviewSelection.matchable(Seq(ReviewCluster.of(Country.Poland, fellBack)), 0.5, None) shouldBe empty
  }

  "the recently matched page" should "list matched clusters whose listing was written since, least confident first" in {
    val now     = Instant.parse("2026-10-06T10:00:00Z")
    val second  = ListingKey.Native("Kino X", "https://x/y", "Y")
    val sure    = matchedDecision.copy(members = Seq(second), confidence = 0.99)(services.identity.DecisionTrace.Empty)
    val all     = clusters :+ ReviewCluster.of(Country.Poland, sure)
    val updated = Map(ListingKey.serialised(Matched) -> now.minusSeconds(3600), ListingKey.serialised(second) -> now.minusSeconds(60),
      ListingKey.serialised(Held) -> now.minusSeconds(60))
    ReviewSelection.recent(all, updated, now.minusSeconds(48 * 3600)).map { case (c, at) => c.title -> at } shouldBe
      Seq("Klondike" -> now.minusSeconds(3600), "Y" -> now.minusSeconds(60))
    ReviewSelection.recent(all, updated, now.minusSeconds(600)).map(_._1.title) shouldBe Seq("Y")
  }

  "a card" should "warn when the venue's own year or director contradicts the film it puts forward" in {
    val now   = Instant.parse("2026-10-06T10:00:00Z")
    val wrong = kafka.copy(year = Some(1991), directors = Seq("Steven Soderbergh"))
    val agreeing = ReviewCards.build(ReviewFixtures.source(now), Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil,
      new ReviewAnswers.Index(Nil)).head
    agreeing.disagreements shouldBe empty
    FactCheck.warnings(agreeing.reviewMembers, wrong.facts) shouldBe Seq(
      "Kino Opalenica states FRANZ KAFKA is from 2025; Franz (1991) is from 1991",
      "Kino Opalenica credits Agnieszka Holland; Franz (1991) is directed by Steven Soderbergh")
    FactCheck.warnings(agreeing.reviewMembers, wrong.facts.copy(year = Some(2024), directors = Seq("A. Holland"))) shouldBe empty
  }
}
