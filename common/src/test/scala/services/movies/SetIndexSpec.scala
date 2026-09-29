package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SetIndexSpec extends AnyFlatSpec with Matchers {

  "a set index" should "collect each key's values and answer an unknown key with nothing" in {
    val index = new SetIndex[String, Int]
    index.add("a", 1); index.add("a", 2); index.add("a", 1); index.add("b", 3)
    index.get("a") shouldBe Set(1, 2)
    index.get("b") shouldBe Set(3)
    index.get("c") shouldBe Set.empty
    index.toMap shouldBe Map("a" -> Set(1, 2), "b" -> Set(3))
  }

  it should "drop a key with its last value, so it holds no empty sets" in {
    val index = new SetIndex[String, Int]
    index.add("a", 1); index.add("a", 2)
    index.remove("a", 1)
    index.holds("a") shouldBe true
    index.remove("a", 2)
    index.holds("a") shouldBe false
    index.toMap shouldBe Map.empty
    index.remove("missing", 9)
    index.toMap shouldBe Map.empty
  }

  it should "drop a whole key at once" in {
    val index = new SetIndex[String, Int]
    index.add("a", 1); index.add("a", 2); index.add("b", 3)
    index.removeAll("a")
    index.toMap shouldBe Map("b" -> Set(3))
  }

  it should "hand out snapshots a later write does not change" in {
    val index = new SetIndex[String, Int]
    index.add("a", 1)
    val before = index.get("a")
    index.add("a", 2)
    before shouldBe Set(1)
  }
}

class PairSetIndexSpec extends AnyFlatSpec with Matchers {

  "a pair set index" should "answer each pair as a set index keyed on the tuple would" in {
    val index = new PairSetIndex[String, Int, Char]
    index.add("a", 1, 'x'); index.add("a", 1, 'y'); index.add("a", 2, 'z'); index.add("b", 1, 'x')
    index.get("a", 1) shouldBe Set('x', 'y')
    index.get("a", 3) shouldBe Set.empty
    index.get("c", 1) shouldBe Set.empty
    index.holds("a", 2) shouldBe true
    index.toMap shouldBe Map(("a", 1) -> Set('x', 'y'), ("a", 2) -> Set('z'), ("b", 1) -> Set('x'))
  }

  it should "drop a pair with its last value, and the first component with its last pair" in {
    val index = new PairSetIndex[String, Int, Char]
    index.add("a", 1, 'x'); index.add("a", 2, 'y')
    index.remove("a", 1, 'x')
    index.holds("a", 1) shouldBe false
    index.toMap shouldBe Map(("a", 2) -> Set('y'))
    index.removeAll("a", 2)
    index.toMap shouldBe Map.empty
    index.remove("missing", 1, 'x'); index.removeAll("missing", 1)
    index.toMap shouldBe Map.empty
  }
}
