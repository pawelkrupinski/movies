package services.review

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ResolverDecision
import services.identity.ResolverDecision.Basis

/** The review pages' reading of the resolver's explanation lines, against REAL prod decisions ([[ProdReviewSample]]). */
class ProdExplanationParseSpec extends AnyFlatSpec with Matchers {

  private val Best   = """best (vetoed|rejected) candidate (\d+) at ([\d.]+)%""".r.unanchored
  private val clusters = ProdReviewSample.decisions.map { case (country, decision) => decision -> ReviewCluster.of(country, decision) }

  "the recorded sample" should "hold matched, below-threshold and vetoed decisions in every country it covers" in {
    val bases = ProdReviewSample.decisions.map(_._2.basis).toSet
    bases should contain allOf (Basis.OwnMatch, Basis.BelowThreshold, Basis.Vetoed)
    ProdReviewSample.decisions.map(_._1).toSet should have size 5
  }

  "every real 'best … candidate' line" should "become the cluster's best candidate, its probability and its veto" in {
    val withBest = clusters.flatMap { case (d, c) => d.explanation.collectFirst { case Best(kind, film, pct) => (c, kind, film.toInt, pct) } }
    withBest.size should be >= 15
    withBest.foreach { case (c, kind, film, pct) =>
      withClue(c.title) {
        c.best.map(_.film) shouldBe Some(film)
        c.best.flatMap(_.probability).map(p => BigDecimal(p * 100).setScale(1, BigDecimal.RoundingMode.HALF_UP)) shouldBe Some(BigDecimal(pct))
        c.best.exists(_.vetoed) shouldBe (kind == "vetoed")
        c.best.exists(_.denial.isDefined) shouldBe (kind == "vetoed")
      }
    }
  }

  it should "keep a denial whole, commas inside its rule and all" in {
    val denials = clusters.flatMap(_._2.best).flatMap(_.denial).toSet
    denials should contain ("Learned(runtime.delta >= 11 AND title in {none,overlap})")
    denials should contain ("its title names it only by the venue's own name")
  }

  "a real 'own match' line" should "give a matched cluster no candidate of its own film, and an unmatched one that candidate" in {
    val line = "'Santosh' {Sandhya Suri} ×1: own match 1233208 at 62.5% by favoured-calibrated (director=same_person(+4.22) " +
      "search.rank=1(+3.48) title=exact(+1.50) venues.corroborating=0(-1.38) runtime.delta=8(-0.93))"
    clusters.exists(_._1.explanation.contains(line)) shouldBe true
    val unmatched = ResolverDecision(Nil, None, 0.5, Basis.BelowThreshold, Seq(line))()
    ReviewCluster.of(Country.Poland, unmatched).candidates shouldBe Seq(ReviewCandidate(1233208, Some(0.625)))
    clusters.filter(_._1.film.isDefined).foreach { case (d, c) => c.candidates.map(_.film) should not contain d.film.get }
  }

  "a 'pooled evidence' line" should "name the film the members' pooled evidence chose, at its probability" in {
    // The format ResolverDecisions writes for a pooled match; the recorded sample happens to hold none.
    val pooled = ResolverDecision(Nil, None, 0.4, Basis.BelowThreshold, Seq("pooled evidence of 3 node(s) → 677558 at 86.4%"))()
    ReviewCluster.of(Country.Poland, pooled).candidates shouldBe Seq(ReviewCandidate(677558, Some(0.864)))
  }

  "the matchable page" should "list the real decisions the resolver rated at the threshold or more, and only those" in {
    val all = clusters.map(_._2)
    ReviewSelection.matchable(all, 0.5, None).map(c => (c.basis, c.best.map(_.film))) shouldBe Seq((Basis.BelowThreshold, Some(1599768)))
    ReviewSelection.matchable(all, 0.4, Some(Basis.Vetoed)).map(_.best.map(_.film)) shouldBe Seq(Some(167073))
  }
}
