package tools

import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A refactor's equivalence phase ([[Equivalence]]): a frozen implementation and its rewrite compared over a corpus,
 *  line for line, after proving the comparison catches a deliberately broken reading. */
class EquivalenceSpec extends AnyFlatSpec with Matchers {

  private val titles = Seq("Obcy", "Diuna: Część druga", "Amator", "")
  // the frozen side and a rewrite of it reading alike: the length and the words, as lines
  private def frozen(title: String): Seq[String] = Seq(s"length ${title.length}") ++ title.split(" ").filter(_.nonEmpty).map("word " + _)
  private def rewritten(title: String): Seq[String] = Seq(s"length ${title.length}") ++ title.split("\\s+").toSeq.filter(_.nonEmpty).map(w => s"word $w")

  "two implementations reading every input alike" should "pass, and say how many inputs and distinct readings it compared" in {
    val verdict = Equivalence.assertEquivalent(titles, frozen, rewritten, minimum = 4)
    (verdict.compared, verdict.distinct, verdict.mismatched) shouldBe ((4, 4, 0))
  }

  "an input read differently" should "fail, naming it and the lines each side alone read" in {
    val drifted: String => Seq[String] = t => if (t == "Amator") Seq("length 6", "word amator") else rewritten(t)
    val failure = the[TestFailedException] thrownBy Equivalence.assertEquivalent(titles, frozen, drifted, minimum = 1)
    failure.getMessage should include("1 of 4 inputs read differently")
    failure.getMessage should include("Amator\n    was word Amator\n    now word amator")
  }

  "a comparison that cannot fail" should "be refused before it is trusted: one side handed twice" in {
    val same: String => Seq[String] = rewritten
    val failure = the[TestFailedException] thrownBy Equivalence.assertEquivalent(titles, same, same, minimum = 1)
    failure.getMessage should include("it compares a side with itself")
  }

  "the self-check" should "catch each side's deliberately broken reading of the first input, a mismatch apiece" in {
    Equivalence.compare(titles.take(1), frozen, Equivalence.broken(rewritten, titles.head)).mismatched shouldBe 1
    Equivalence.compare(titles, Equivalence.broken(frozen, titles.head), rewritten).mismatched shouldBe 1
  }

  "a corpus that went missing, or a reading that says nothing" should "fail too" in {
    (the[TestFailedException] thrownBy Equivalence.assertEquivalent(Seq.empty[String], frozen, rewritten, minimum = 3)).getMessage should
      include("holds 0 inputs, fewer than the 3 it must")
    (the[TestFailedException] thrownBy Equivalence.assertEquivalent(titles, (_: String) => Seq("ok"), (_: String) => Seq("ok"), minimum = 1)).getMessage should
      include("every one of the 4 inputs reads the same")
  }

  "a deliberately broken reading" should "change the broken input's first line alone, or add one to an empty reading" in {
    val broken = Equivalence.broken(frozen, "Obcy")
    broken("Obcy") should not equal frozen("Obcy")
    broken("Obcy").tail shouldBe frozen("Obcy").tail
    broken("Amator") shouldBe frozen("Amator")
    Equivalence.broken((_: String) => Seq.empty[String], "x")("x") shouldBe Seq("<deliberately broken>")
  }
}
