package services.identityadapters

import services.identity.*
import play.api.Logging
import services.identity.vocabulary.VoterFamily
import services.tasks.HandlerOutcome.{Done, Skipped}
import services.tasks.{HandlerOutcome, Task, TaskHandler}
import services.tasktypes.TaskType
import tools.CircuitOpenException
import services.identity.vocabulary.{CatalogueQuestion, Question}
import services.identity.vocabulary.PosterAnswers

/**
 * Asks the agreement's questions, each by its kind: a family's search, record or programme of the family's live source
 * (`sources`), filed; TMDB's find of an agreed IMDb id (`find`) or its record of a film read again (`reread`), filed by
 * the TMDB store as it files every TMDB answer; a poster hashed (`hashing`) and filed; a batch of catalogue ids mapped
 * at once (`mapping`), a catalogue page's links read (`pages`). A question whose store already holds a fresh answer is
 * skipped ([[AgreementQuestionMetrics.Fresh]]), and so is one still inside the wait a failed ask left it
 * ([[AgreementQuestionMetrics.Waiting]]); once one is filed — or its ask failed, which may settle a cluster waiting on it
 * — a projection is asked for (`filed`), which reads it.
 *
 * Every ask that comes to something is recorded in `memory` ([[QuestionMemory]]): an answer, an empty answer filed (a
 * durable non-answer), a failure with the host it went to when the failure names it. A failed ask is remembered (5 min
 * doubling to 6 h, its kind's policy) and the task is
 * done: the stage hands the question over again once the families' answers move, and it is queued and asked once its
 * wait has ended — the queue's own backoff no longer asks it a second time. A host whose breaker is open was never asked:
 * the ask waits on that host (counted [[AgreementQuestionMetrics.Deferred]]), held back as a failure is and released early
 * when the host is back ([[HostRecovery]]), but counted as no failure until it has been turned away for
 * [[QuestionPolicies.TurnAwayGiveUp]] ([[QuestionMemory.turnedAway]]). A poster whose fetch has
 * failed [[QuestionPolicies.PosterGiveUpAttempts]] times in a row is given up on: filed as unread, as the stage reads it,
 * and remembered for [[QuestionPolicies.PosterUnreadAge]].
 *
 * The queue's handler of [[TaskType.AskQuestion]] — the four task types before it [[retired]] — and
 * what [[InlineFamilyAsks]] asks a new cluster's family questions through.
 */
final class AgreementAsker(families: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], find: String => Unit, reread: Int => Unit,
                           posters: PosterAnswerStore, hashing: PosterHashing, catalogue: CatalogueAnswerStore, mapping: CatalogueMapping,
                           pages: CatalogueLinkReader, filed: () => Unit, memory: QuestionMemory,
                           metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler with Logging {
  import AgreementQuestionMetrics.{Fresh, Answered, Waiting}

  val taskType: TaskType = TaskType.AskQuestion

  def handle(task: Task): HandlerOutcome = ask(AgreementQuestions.questionsOf(task))

  /** The four task types before [[TaskType.AskQuestion]], RETIRED: a task of one still queued is completed unasked —
   *  never left unhandled, which the queue would hand straight back (2026-10-04: one leftover task claimed 24,759 times)
   *  — and whatever it asked the stage asks again as an AskQuestion while it still wants it. */
  def retired: Seq[TaskHandler] =
    Seq(TaskType.AgreementQuestion, TaskType.AgreementFind, TaskType.AgreementPoster, TaskType.AgreementCatalogue).map { old =>
      new TaskHandler {
        val taskType: TaskType = old
        def handle(task: Task): HandlerOutcome = HandlerOutcome.Done
      }
    }

  /** One task's questions asked — its one question, or a batch of catalogue questions as one — those of them still due.
   *  `announce`: whether a filing asks for a projection — always on the queue; inline, only once the resolve stopped
   *  waiting for it. */
  def ask(questions: Seq[Question], announce: => Boolean = true): HandlerOutcome = {
    val announced: () => Unit = () => if (announce) filed()
    val due = memory.due(questions.toSet)
    questions.filterNot(due).foreach(question => metrics.asked(AgreementQuestions.labelOf(question), Waiting))
    questions.filter(due) match {
      case Seq(question) => ask(question, announced)
      case batch if batch.nonEmpty && batch.forall(_.isInstanceOf[Question.Catalogue]) =>
        askCatalogue(batch.collect { case Question.Catalogue(question) => question }, announced)
      case _ => Skipped
    }
  }

  private def ask(question: Question, filed: () => Unit): HandlerOutcome = question match {
    case Question.Family(family, asked) => askFamily(question, family, asked, filed)
    case Question.Find(imdbId) =>
      answered(question, AgreementQuestionMetrics.TmdbFind, s"TMDB find '$imdbId'", filed)(find(imdbId))
    case Question.Record(film) =>
      answered(question, AgreementQuestionMetrics.TmdbRecord, s"TMDB record $film", filed)(reread(film))
    case Question.Poster(poster) =>
      if (!posters.wanted(poster)) fresh(AgreementQuestionMetrics.Poster)
      else answered(question, AgreementQuestionMetrics.Poster, s"poster ${PosterAnswers.idOf(poster)}", filed)(posters.file(poster, hashing.of(poster, posters.filed(poster))))
    case Question.Catalogue(asked) => askCatalogue(Seq(asked), filed)
    case Question.Candidate(_) | Question.FilmRecord(_) | Question.Person(_) | Question.VenuePage(_) => Skipped
  }

  /** A family's question asked of its source -- skipped while its answer is fresh; a family this worker has no source for
   *  answers nothing: filed once as no film, as an empty answer is, so the question is attempted and fresh for the
   *  family's age, never left open for a cluster to wait on. */
  private def askFamily(question: Question, family: VoterFamily, asked: String, filed: () => Unit): HandlerOutcome =
    if (!families.wanted(FamilyAnswerStore.questionId(family, asked))) fresh(family.label)
    else sources.get(family) match {
      case Some(source) =>
        asking(Seq(question), family.label, s"${family.label} '$asked'", filed)(AgreementQuestions.file(families, family, source, asked))
      case None =>
        asking(Seq(question), family.label, s"${family.label} '$asked' (no source)", filed) {
          AgreementQuestions.file(families, family, FamilySource.answeringNothing(family), asked)
          AgreementQuestionMetrics.Nothing
        }
    }

  /** Catalogue questions asked as one: the ids mapped in one batch, each page read — skipped when the store holds fresh
   *  answers to them all. */
  private def askCatalogue(questions: Seq[CatalogueQuestion], filed: () => Unit): HandlerOutcome =
    if (!questions.exists(catalogue.wanted)) fresh(AgreementQuestionMetrics.Catalogue)
    else {
      val asked = questions.map(Question.Catalogue(_))
      asking(asked, AgreementQuestionMetrics.Catalogue, s"catalogue ${asked.flatMap(_.id).mkString(" ")}", filed) {
        AgreementCatalogueQuestions.file(catalogue, mapping, pages, questions); Answered
      }
    }

  private def fresh(label: String): HandlerOutcome = { metrics.asked(label, Fresh); Skipped }

  private def answered(question: Question, label: String, what: String, filed: () => Unit)(ask: => Unit): HandlerOutcome =
    asking(Seq(question), label, what, filed) { ask; Answered }

  /** `questions` asked as one: `ask` run — it says how it came out — then a projection asked for (`filed`). A failure is
   *  remembered against the host it went to ([[AgreementQuestions.hostOf]]: the hosts `ask` requests are noted as it
   *  runs); an ask a host's open breaker or its fleet pace turned away is remembered as a WAIT, no failure counted
   *  ([[QuestionMemory.turnedAway]]) until it has been turned away for [[QuestionPolicies.TurnAwayGiveUp]]. A poster
   *  reaching its give-up count is given up on, and a projection asked for too: the questions were attempted. Counted
   *  under `label` as the ask said, [[AgreementQuestionMetrics.Deferred]] or [[AgreementQuestionMetrics.Failed]]. */
  private def asking(questions: Seq[Question], label: String, what: String, filed: () => Unit)(ask: => String): HandlerOutcome = {
    val (asked, failedAt) = AgreementQuestions.hostsNoted(scala.util.Try(ask))
    try {
      val outcome = asked.get
      // attempted: what the memory answers of it from now on — an empty answer filed is a durable non-answer
      if (outcome == AgreementQuestionMetrics.Nothing) memory.unanswered(questions) else memory.answered(questions)
      filed(); metrics.asked(label, outcome); Done
    }
    catch {
      case scala.util.control.NonFatal(e) =>
        val failures = remembered(questions, label, e, failedAt(e))
        logger.debug(s"agreement: $what failed (${e.getMessage}), asked again once its wait ends")
        givenUpPosters(failures)
        // attempted, though nothing was filed: a cluster waiting on it may settle now (AgreementStage.settles)
        filed()
        Done
    }
  }

  /** `questions`' ask that failed with `e` at `host`, remembered and counted under `label`: a host's open breaker or the
   *  fleet pacer turning the ask away (CircuitOpenException) is a WAIT against that host -- remembered and held back as a
   *  failure is (and released when the host is back), but counted as no failure, so a flapping host spends no poster's
   *  attempts; one blocked for TurnAwayGiveUp still ends, as a failure given up on. Each question's failures in a row. */
  private def remembered(questions: Seq[Question], label: String, e: Throwable, host: Option[String]): Map[Question, Int] =
    if (e.isInstanceOf[CircuitOpenException]) { metrics.asked(label, AgreementQuestionMetrics.Deferred); memory.turnedAway(questions, host) }
    else { metrics.asked(label, AgreementQuestionMetrics.Failed); memory.failed(questions, host) }

  /** The posters among `failures` whose last attempt failed too, reaching their give-up count: given up on, read as
   *  unread rather than waited on for ever. */
  private def givenUpPosters(failures: Map[Question, Int]): Unit = {
    val givenUp = failures.collect {
      case (Question.Poster(poster), times) if times >= QuestionPolicies.PosterGiveUpAttempts => poster
    }
    givenUp.foreach(posters.giveUp)
    if (givenUp.nonEmpty) memory.unanswered(givenUp.map(Question.Poster(_)))
  }
}
