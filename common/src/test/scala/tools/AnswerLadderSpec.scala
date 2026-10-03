package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnswerLadderSpec extends AnyFlatSpec with Matchers {

  private val down = new HttpStatusException(503, "GET", "https://a/", None)

  "firstAnswer" should "take the first answer and not ask the rungs after it" in {
    var askedLast = false
    AnswerLadder.firstAnswer(() => None, () => Some(1), () => { askedLast = true; Some(2) }) shouldBe Some(1)
    askedLast shouldBe false
  }

  it should "answer None only when every rung was asked and answered None" in {
    AnswerLadder.firstAnswer[Int](() => None, () => None) shouldBe None
  }

  it should "carry on past a failed rung and take a later answer" in {
    AnswerLadder.firstAnswer(() => throw down, () => Some(2)) shouldBe Some(2)
  }

  it should "throw the first failure, with the rest suppressed, when nobody answered" in {
    val later  = new java.io.IOException("reset")
    val thrown = the[HttpStatusException] thrownBy AnswerLadder.firstAnswer[Int](() => throw down, () => None, () => throw later)
    thrown shouldBe down
    thrown.getSuppressed.toSeq shouldBe Seq(later)
  }
}
