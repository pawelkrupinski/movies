package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A lookup's answer kept Unknown through every combination: a gap is never read as "no film". */
class AnswerSpec extends AnyFlatSpec with Matchers {
  "an answer" should "map what is known and leave a gap a gap" in {
    Answer.Known(2).map(_ * 3) shouldBe Answer.Known(6)
    (Answer.Unknown: Answer[Int]).map(_ * 3) shouldBe Answer.Unknown
  }

  it should "fall back to another source only when it does not know" in {
    var asked = 0
    def live = { asked += 1; Answer.Known(Seq(7)) }
    Answer.Known(Nil).orElse(live) shouldBe Answer.Known(Nil)
    asked shouldBe 0
    (Answer.Unknown: Answer[Seq[Int]]).orElse(live) shouldBe Answer.Known(Seq(7))
  }

  it should "be known as a whole only when every part is" in {
    Answer.sequence(Seq(Answer.Known(1), Answer.Known(2))) shouldBe Answer.Known(Seq(1, 2))
    Answer.sequence(Seq(Answer.Known(1), Answer.Unknown)) shouldBe Answer.Unknown
    Answer.sequence(Seq.empty[Answer[Int]]) shouldBe Answer.Known(Nil)
  }

  "a held value" should "be known, and nothing held a gap" in {
    Some(None).toAnswer shouldBe Answer.Known(None)
    Option.empty[Int].toAnswer shouldBe Answer.Unknown
  }
}
