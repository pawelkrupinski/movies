package services.identityadapters

import play.api.Logging
import services.identity.vocabulary.{Answer, AnswersChanged, Question}

/** What the paced live lookup fill ([[LookupFill]]) keeps of its questions, in the question memory: which of the model's
 *  gaps are due ([[due]]), what each ask of a round came to ([[asked]]), and, the round's asks done, what to remember of
 *  them ([[remember]]) and of each host's round ([[roundEnded]]). Its rules change with the memory's policies
 *  ([[QuestionPolicies]]), the fill's with its pacing and rate: a round's budget it reads only for what its asks came to. `fileAbsent` files a TMDB film's
 *  record absent, as a 404 is (`TmdbNormalizer.recordAbsent`): one whose reads keep failing while its host answers
 *  others. */
final class LookupFillLedger(memory: QuestionMemory, fileAbsent: Int => Unit) extends Logging {
  /** Of the questions the identity model found unanswered, and the records it lacks, the ones due to be asked. */
  def due(gaps: AnswersChanged): AnswersChanged = memory.due(gaps)

  /** What one ask of the round over `budget` came to, and the host a failed read went to. Asked and still unanswered —
   *  not deferred for want of budget — is remembered: a durable non-answer is asked again a day later, one whose read
   *  failed on a short backoff, marked against the host that read went to (an IMDb suggestion's is IMDb's), whose
   *  recovery releases it. */
  def asked[A](budget: LookupFillBudget)(answer: => Answer[A]): (LookupFill.Asked, Option[String]) = {
    val (deferred, failed) = (budget.deferred.get, budget.failed.get)
    val (known, failedAt) = AgreementQuestions.hostsNoted(answer.isAnswered)
    // A read that failed wins over the deferrals its back-off then caused within the same question.
    if (known) (LookupFill.Asked.Answered, None)
    else if (budget.failed.get != failed) (LookupFill.Asked.Failed, failedAt(budget.lastFailure.orNull))
    else if (budget.deferred.get != deferred) (LookupFill.Asked.Settled, None)
    else (LookupFill.Asked.Unanswered, None)
  }

  /** What the round's asks came to, into the question memory: the unanswered remembered, a run of failures ended by an
   *  answer, each failure marked against its host -- and a record whose reads keep failing filed absent. */
  def remember(outcomes: Seq[(Question, (LookupFill.Asked, Option[String]))], budget: LookupFillBudget): Unit = {
    memory.unanswered(outcomes.collect { case (question, (LookupFill.Asked.Unanswered, _)) => question })
    // an answer ends a run of failures: else a record that failed seven rounds, then answered, was given up on at its
    // next failed read a month on (QuestionPolicies.RecordGiveUpFailures counts failures IN A ROW)
    val answered = outcomes.collect { case (question, (LookupFill.Asked.Answered, _)) => question }.toSet
    if (answered.nonEmpty) memory.answered(memory.outcomes(answered).collect { case (question, QuestionMemory.Outcome.Failed(_, _, _)) => question })
    val failures = outcomes.collect { case (question, (LookupFill.Asked.Failed, host)) => (question, host) }.groupMap(_._2)(_._1)
      .toSeq.flatMap { case (host, failed) => memory.failed(failed, host).map { case (question, times) => (question, host, times) } }
    givenUp(failures, budget)
  }

  /** Each host's round over `budget`, on its own: one host down says nothing of another's health. */
  def roundEnded(budget: LookupFillBudget): Unit =
    budget.hosts.foreach { case (host, (answered, failed, deferred)) => memory.roundEnded(answered, failed, deferred, host) }

  /** A record whose read keeps failing (`failures`: each question, its host and its failures in a row) while its host
   *  answers others (`budget`'s) is filed absent, as a 404 is: never pending for good. */
  private def givenUp(failures: Seq[(Question, Option[String], Int)], budget: LookupFillBudget): Unit = {
    val answering = budget.hosts.collect { case (host, (answered, _, _)) if answered > 0 => host }.toSet
    failures.foreach {
      case (Question.FilmRecord(id), host, times)
          if times >= QuestionPolicies.RecordGiveUpFailures && answering(host.getOrElse(QuestionMemory.TmdbHost)) =>
        logger.warn(s"identity lookup fill: film $id's record failed $times reads in a row; filed absent")
        fileAbsent(id)
      case _ => ()
    }
  }
}
