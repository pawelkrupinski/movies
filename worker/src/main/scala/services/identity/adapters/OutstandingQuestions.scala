package services.identity
package adapters

import java.time.Clock

/**
 * The agreement's OUTSTANDING questions: those the stage waits on (`open`, `AgreementStage.wanted`) that were handed to
 * the queue and never attempted — their task lost, the queue wiped, the worker restarted mid-task — or that failed and
 * are past their retry ([[QuestionMemory.Outcome.Failed]]'s `nextRetryAt`). The settle rule holds a cluster on a question
 * never attempted (docs/design/identity-questions.md, step 4), so such a question would hold it for good: [[reask]]
 * hands them to `ask` again (`AgreementQuestions.enqueue`, which queues none already queued). Run by a periodic task
 * ([[OutstandingQuestions.Interval]]); a quiet tick, nothing open, reads nothing.
 */
final class OutstandingQuestions(open: () => Set[Question], memory: QuestionMemory, ask: Set[Question] => services.tasks.Enqueued,
                                 clock: Clock) {

  /** The open questions never attempted, or failed and due again. */
  def outstanding(): Set[Question] = {
    val asked = open().filter(_.kind.system == QuestionSystem.Agreement)
    if (asked.isEmpty) Set.empty
    else {
      val known = memory.outcomes(asked)
      val now   = clock.instant()
      asked.filter(question => known.get(question).forall {
        case QuestionMemory.Outcome.Failed(_, nextRetryAt, _) => !nextRetryAt.isAfter(now)
        case _                                               => false
      })
    }
  }

  /** Asks the outstanding questions again: how many there were, and what their enqueues came to. */
  def reask(): OutstandingQuestions.Reasked = {
    val lost = outstanding()
    OutstandingQuestions.Reasked(lost.size, if (lost.nonEmpty) ask(lost) else services.tasks.Enqueued.Zero)
  }
}

object OutstandingQuestions {
  /** One [[OutstandingQuestions.reask]]: `outstanding` questions found, `enqueued` what asking them came to — a run
   *  whose enqueue failed is not a completed one ([[services.tasks.Enqueued.run]]). */
  final case class Reasked(outstanding: Int, enqueued: services.tasks.Enqueued)

  /** How often the outstanding questions are asked again: a lost task holds its cluster this long at most. */
  val Interval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, java.util.concurrent.TimeUnit.MINUTES)
}
