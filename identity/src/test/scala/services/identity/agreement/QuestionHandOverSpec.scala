package services.identity.agreement

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.Question

import scala.collection.mutable

/** The agreement stage's questions on their way to the queue ([[QuestionHandOver]]): each handed to `ask` once per
 *  families' version, and an attempt since — told by the question memory's marks — what reads the decisions again. */
class QuestionHandOverSpec extends AnyFlatSpec with Matchers {
  private val (find, record) = (Question.Find("tt16315948"), Question.Record(438631))

  "A question met again at the same version" should "be handed to ask once, and again once the version moved" in {
    val asked    = mutable.ArrayBuffer.empty[Set[Question]]
    val handOver = new QuestionHandOver(asked += _, None)
    handOver.handOver(Set(find), version = 1, tried = _ => false)
    handOver.handOver(Set(find, record), version = 1, tried = _ => false)
    handOver.handOver(Set(find, record), version = 1, tried = _ => false)
    handOver.handOver(Set(find), version = 2, tried = _ => false)
    asked.toSeq shouldBe Seq(Set(find), Set(record), Set(find))
    handOver.wanted shouldBe Set(find)
  }

  "The questions awaited" should "be those not tried when handed over, and one tried since read the decisions again" in {
    var tried    = Set(record)
    val handOver = new QuestionHandOver(_ => (), None)
    handOver.handOver(Set(find, record), version = 1, tried = tried)
    handOver.awaiting shouldBe Set(find)
    handOver.attemptedSince(tried) shouldBe false
    tried += find
    handOver.attemptedSince(tried) shouldBe true
  }

  "The question memory's marks" should "spare reading the awaited questions while none was marked since" in {
    var marks    = 0L
    var read     = 0
    val handOver = new QuestionHandOver(_ => (), Some(() => marks))
    val tried: Question => Boolean = { _ => read += 1; false }
    handOver.handOver(Set(find), version = 1, tried = tried)
    read = 0
    handOver.attemptedSince(tried) shouldBe false
    read shouldBe 0
    marks += 1
    handOver.attemptedSince(tried) shouldBe false   // marked, but not one of these: read once, then spared again
    handOver.attemptedSince(tried) shouldBe false
    read shouldBe 1
  }
}
