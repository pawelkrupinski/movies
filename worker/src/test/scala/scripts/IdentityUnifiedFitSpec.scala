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
    refit shouldBe shipped.copy(ablation = Nil)
  }

  "the shipped hybrid weights" should "be what the checked-in rows passing the hard guards fit" in {
    val hybrid = UnifiedWeights.fromResource(UnifiedWeights.HybridResourcePath).getOrElse(fail("no hybrid weights"))
    hybrid.guards shouldBe UnifiedEvidence.Guards
    fit(read(Training), versionOf(Training), UnifiedEvidence.Guards) shouldBe hybrid.copy(ablation = Nil)
    UnifiedEvidence.Guards.foreach(guard => hybrid.weights(UnifiedEvidence.Names.indexOf(guard) + 1) shouldBe 0.0)
  }

  "the shipped unified weights" should "weigh every signal the unified evidence reads, each in its direction" in {
    shipped.signals shouldBe UnifiedEvidence.Names
    UnifiedEvidence.Signals.zip(shipped.weights.drop(1)).filter { case (signal, weight) => signal.direction * weight < 0 } shouldBe empty
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
