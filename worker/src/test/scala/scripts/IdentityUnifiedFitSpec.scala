package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{UnifiedEvidence, UnifiedWeights}

/** The unified evidence model's weights are DATA a refit regenerates: what [[IdentityUnifiedFit]] fits from the
 *  checked-in rows, every weight held to its signal's direction. */
class IdentityUnifiedFitSpec extends AnyFlatSpec with Matchers {
  import IdentityUnifiedFit._

  private lazy val shipped = UnifiedWeights.fromResource().getOrElse(fail(s"${UnifiedWeights.ResourcePath} is not on the classpath"))

  "the shipped unified weights" should "be what the checked-in rows fit (re-run scripts.IdentityUnifiedFit when the rows move)" in {
    val refit = fit(read(Training), versionOf(Training))
    // the ablation is the tool's report beside the weights, measured by its main, not re-measured here
    sameFit(refit, shipped.copy(ablation = Nil))
  }

  "the shipped hybrid weights" should "be what the checked-in rows passing the hard guards fit" in {
    val hybrid = UnifiedWeights.fromResource(UnifiedWeights.HybridResourcePath).getOrElse(fail("no hybrid weights"))
    hybrid.guards shouldBe UnifiedEvidence.Guards
    sameFit(fit(read(Training), versionOf(Training), UnifiedEvidence.Guards), hybrid.copy(ablation = Nil))
    UnifiedEvidence.Guards.foreach(guard => hybrid.weights(UnifiedEvidence.Names.indexOf(guard) + 1) shouldBe 0.0)
  }

  "the shipped unified weights" should "weigh every signal the unified evidence reads, each in its direction" in {
    shipped.signals shouldBe UnifiedEvidence.Names
    UnifiedEvidence.Signals.zip(shipped.weights.drop(1)).filter { case (signal, weight) => signal.direction * weight < 0 } shouldBe empty
  }

  "the shipped identity rules" should "be the rules every fold selects from the checked-in rows" in {
    val shipped  = services.identity.UnifiedRules.fromResource().getOrElse(fail("no identity-unified-rules.json"))
    val clusters = read(Training).groupBy(_.cluster).toSeq.sortBy(_._1).map { case (id, rs) => IdentityUnifiedStrategies.Cluster(id, rs.sortBy(_.film)) }
    val stable   = IdentityUnifiedStrategies.stableSteps(clusters)
    (shipped.fill, shipped.correct, shipped.guards) shouldBe
      ((stable.filter(_.role == "fill").map(_.signal), stable.filter(_.role == "correct").map(_.signal), UnifiedEvidence.Guards))
    // the hand-pinned rules read only signals a contender carries, each measured beside them
    shipped.pinned.flatMap(_.split('&')).map(_.stripPrefix("!")).filterNot((UnifiedEvidence.Names ++ UnifiedEvidence.PinnedSignals).contains) shouldBe empty
    shipped.pinned.filterNot(rule => shipped.measured.contains(s"pinned.$rule.wrong")) shouldBe empty
  }

  /** The same fit up to floating-point rounding. A near-separable signal drives its weight to ~1e12, where
   *  the last digits differ between the Mac a refit runs on and CI's Linux runners (1.9290000032865635e12 against
   *  1.929000003286336e12, 2026-10-05): exact equality there is a platform check, not a regression check. */
  private def sameFit(actual: UnifiedWeights, expected: UnifiedWeights): Unit = {
    actual.copy(weights = Nil) shouldBe expected.copy(weights = Nil)
    actual.weights should have size expected.weights.size.toLong
    actual.weights.zip(expected.weights).foreach { case (got, want) => got shouldBe want +- (math.abs(want) * 1e-9 + 1e-12) }
  }

  private def row(cluster: String, film: String, today: Boolean = false, label: Option[Boolean] = None, source: String = "") =
    Row("pl", cluster, "corpus", "Kino", 1, "t", film, "", today, label, source, 0, 0, IndexedSeq.fill(UnifiedEvidence.Names.size)(0.0))

  "the cut" should "sit above every take a hand label calls wrong and every take moving today's film, at the lowest take above them" in {
    val scored = Seq(
      row("a", "tmdb:1", label = Some(false), source = "hand") -> 0.70, row("a", "tmdb:2") -> 0.10,
      row("b", "tmdb:3", today = true) -> 0.20, row("b", "tmdb:4") -> 0.80,
      row("c", "tmdb:5", label = Some(true), source = "hand") -> 0.85,
      row("d", "tmdb:6") -> 0.75)
    cutOf(scored) shouldBe 0.85
    Measure.takes(scored, 0.85).map(_.film) shouldBe Seq("tmdb:5")
  }

  "an outcome" should "count a cluster's listings switched, lost and gained against today's take" in {
    val rows  = Seq(row("a", "tmdb:1", today = true), row("a", "tmdb:2"), row("b", "tmdb:3", today = true), row("c", "tmdb:4"))
    val takes = Seq(row("a", "tmdb:2"), row("c", "tmdb:4"))
    Measure.outcome(rows, takes) shouldBe Measure.Outcome(0, 0, 2, switched = 1, lost = 1, gained = 1)
  }
}
