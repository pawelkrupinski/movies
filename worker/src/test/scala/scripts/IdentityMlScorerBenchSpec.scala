package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

/** The bench's plain-Scala LightGBM evaluator reads a `dump_model()` tree as LightGBM does: `<=` goes left, a NaN
 *  follows `default_left`, and the probability is the logistic of the summed leaves. */
class IdentityMlScorerBenchSpec extends AnyFlatSpec with Matchers {

  private val dump =
    """{"features": ["a", "b"], "model": {"tree_info": [
      |  {"tree_structure": {"split_feature": 0, "threshold": 0.5, "decision_type": "<=", "default_left": false,
      |    "left_child": {"leaf_value": -1.0},
      |    "right_child": {"split_feature": 1, "threshold": 2.0, "decision_type": "<=", "default_left": true,
      |      "left_child": {"leaf_value": 0.5}, "right_child": {"leaf_value": 2.0}}}},
      |  {"tree_structure": {"leaf_value": 0.25}}]}}""".stripMargin

  private def sigmoid(x: Double) = 1.0 / (1.0 + math.exp(-x))

  "IdentityMlScorerBench.load" should "score a dumped ensemble as LightGBM does" in {
    val file = Files.createTempFile("gbm", ".json")
    try {
      Files.writeString(file, dump)
      val model = IdentityMlScorerBench.load(file)
      model.features shouldBe IndexedSeq("a", "b")
      model.probability(Array(0.5, 9.0)) shouldBe sigmoid(-1.0 + 0.25)        // a <= 0.5: left leaf
      model.probability(Array(1.0, 2.0)) shouldBe sigmoid(0.5 + 0.25)         // b <= 2.0: left leaf
      model.probability(Array(1.0, 3.0)) shouldBe sigmoid(2.0 + 0.25)
      model.probability(Array(Double.NaN, Double.NaN)) shouldBe sigmoid(0.5 + 0.25) // NaN: right at the root, left below
    } finally Files.delete(file)
  }
}
