package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The fitted candidate-decoration score (`DecorationScore`): its fit, its cross-validated cut, and determinism. The
 *  rows below are test data, not measured outcomes. */
class DecorationScoreSpec extends AnyFlatSpec with Matchers {

  private def row(key: String, aligned: Int, format: Double, good: Boolean, bad: Boolean) =
    DecorationScore.Row(key, DecorationScore.Features(aligned, recurring = 3, venues = 2, prefix = false, tokens = 2, formatShare = format,
      innerResolved = 0.5, matchedShare = 0.5), good, bad)
  // good candidates are aligned around several films; bad ones are seen around none aligned
  private val rows = (1 to 30).map(i => row(s"good-$i", aligned = 3 + i % 4, format = 0.5, good = true, bad = false)) ++
    (1 to 30).map(i => row(s"bad-$i", aligned = 0, format = 0.0, good = false, bad = true)) ++
    (1 to 60).map(i => row(s"none-$i", aligned = i % 2, format = 0.0, good = false, bad = false))

  "The fit" should "weigh up what tells good candidates from bad, and cut above every held-out bad one" in {
    val score = DecorationScore.fit(rows, "test")
    score.weights(DecorationScore.Names.indexOf("log1p(aligned)")) should be > 0.0
    score.heldOut.acceptedBad shouldBe 0
    score.heldOut.acceptedGood should be > 0
    rows.filter(_.bad).foreach(r => score.accepts(r.features) shouldBe false)
    score.accepts(row("new", aligned = 5, format = 0.5, good = false, bad = false).features) shouldBe true
  }

  it should "fit the same model from any order of its rows" in {
    val reference = DecorationScore.fit(rows, "test")
    DecorationScore.fit(new scala.util.Random(7).shuffle(rows), "test") shouldBe reference
  }

  it should "accept nothing when a bad candidate scores above every good one" in {
    val inverted = rows.map(r => r.copy(good = r.bad, bad = r.good))
    val score = DecorationScore.fit(inverted.map(r => if (r.key == "good-1") r.copy(bad = true, good = false) else r), "test")
    score.heldOut.acceptedBad shouldBe 0
  }
}
