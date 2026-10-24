package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.VoterFamily
import tools.MutableClock

import java.time.{Duration, Instant}

/** A question the stage waits on that was handed to the queue and never attempted holds its cluster under the settle
 *  rule (docs/design/identity-questions.md, step 4): the reaper asks it again, and a failed one past its retry, and
 *  leaves alone what was answered, durably unanswered or is still inside its wait. */
class OutstandingQuestionsSpec extends AnyFlatSpec with Matchers {
  private val (lost, answered, absent, failed) = (Question.Family(VoterFamily.Imdb, "title|Lalka"), Question.Family(VoterFamily.Wiki, "title|Lalka"),
    Question.Family(VoterFamily.Filmweb, "title|Lalka"), Question.Find("tt0063208"))

  "the outstanding questions" should "be the open ones never attempted, and those failed past their retry, asked again" in {
    val clock  = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    memory.answered(Seq(answered)); memory.unanswered(Seq(absent)); memory.failed(Seq(failed))
    var open  = Set[Question](lost, answered, absent, failed, Question.FilmRecord(7))   // a model question is not the reaper's
    val asked = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val reaper = new OutstandingQuestions(() => open, memory, asked += _, clock)
    reaper.reask() shouldBe 1
    asked.toSeq shouldBe Seq(Set(lost))                       // the failed find still inside its 5-minute wait
    clock.advance(Duration.ofMinutes(5))
    reaper.outstanding() shouldBe Set(lost, failed)           // past its retry: asked again too
    open = Set.empty
    asked.clear()
    reaper.reask() shouldBe 0                                 // a quiet tick asks nothing
    asked shouldBe empty
  }

  it should "queue again a question whose task was lost, and one still queued not twice" in {
    val clock  = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val memory = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    var queue  = new services.tasks.InMemoryTaskQueue
    val reaper = new OutstandingQuestions(() => Set(lost), memory, AgreementQuestions.enqueue(queue, _, memory, clock), clock)
    def queued() = Iterator.continually(queue.claim("w", scala.concurrent.duration.Duration(1, "minute"), clock.instant())).takeWhile(_.isDefined)
      .flatten.toSeq.flatMap(AgreementQuestions.questionsOf)
    AgreementQuestions.enqueue(queue, Set(lost), memory, clock)
    reaper.reask()
    queued() shouldBe Seq(lost)                               // still queued: not queued twice
    queue = new services.tasks.InMemoryTaskQueue              // the queue wiped, its task lost: nothing attempted — asked again
    reaper.reask()
    queued() shouldBe Seq(lost)
  }
}
