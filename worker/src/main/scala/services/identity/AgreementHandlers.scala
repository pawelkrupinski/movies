package services.identity

import play.api.Logging
import services.identity.agreement.VoterFamily
import services.tasks.HandlerOutcome.{Deferred, Done, Reschedule, Skipped}
import services.tasks.{HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}
import tools.{CircuitOpenException, HttpStatusException}

import java.time.Clock

/**
 * The agreement's questions as QUEUE TASKS: what the other film database families have not answered yet for the
 * clusters the model left unmatched (`agreement.AgreementStage.wanted`), and TMDB's `find` of an agreed IMDb id
 * (`wantedFinds`) — enqueued by the stage the moment it meets them ([[AgreementQuestions.enqueueOpen]]), each asked on
 * the task pool with every other lookup's retries, backoff, breaker and metrics.
 */
object AgreementQuestions {
  private val Family   = "family"
  private val Question = "question"
  private val ImdbId   = "imdbId"

  /** Every open question and find, one task each — one already queued is not queued again. */
  def enqueueOpen(queue: TaskQueue, wanted: Set[(VoterFamily, String)], finds: Set[String], clock: Clock): Unit = {
    wanted.toSeq.sortBy { case (family, question) => (family.ordinal, question) }.foreach { case (family, question) =>
      queue.enqueue(TaskType.AgreementQuestion, s"agreement|${family.label}|$question",
        Map(Family -> family.label, Question -> question), submittedAt = clock.instant())
    }
    finds.toSeq.sorted.foreach(imdbId =>
      queue.enqueue(TaskType.AgreementFind, s"agreement-find|$imdbId", Map(ImdbId -> imdbId), submittedAt = clock.instant()))
  }

  /** The statuses that answer a question with nothing — a query the site refuses (Filmweb's 400 for an overlong title),
   *  a page that is not there — filed as such. */
  val NothingThere: Set[Int] = Set(400, 404, 410)

  /** `family`'s answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`), asked of `source` and filed. */
  def file(store: FamilyAnswerStore, family: VoterFamily, source: FamilySource, question: String): Unit = {
    def answer[A](read: => A, nothing: A): A = try read catch { case e: HttpStatusException if NothingThere(e.code) => nothing }
    question.split("\\|", 2) match {
      case Array("title", text)    => store.fileTitled(family, text, answer(source.titled(text), Nil))
      case Array("director", name) => store.fileDirected(family, name, answer(source.directedBy(name), Nil))
      case Array("record", id)     => store.fileRecord(family, id, answer(source.record(id), None))
      case _                       => throw new IllegalArgumentException(s"no such question: $question")
    }
  }

  /** A failed ask as the queue takes it: a host whose breaker is open was never asked — the attempt is given back and
   *  the task waits the block out — and anything else is asked again on the queue's backoff. */
  def failed(e: Throwable, what: String, clock: Clock): HandlerOutcome = e match {
    case open: CircuitOpenException => Deferred(Some(s"$what: ${open.getMessage}"), Some(clock.instant().plusMillis(open.openForMs)))
    case other                      => Reschedule(Some(s"$what: ${other.getMessage}"))
  }

  def familyOf(task: Task): Option[VoterFamily] = task.payload.get(Family).flatMap(label => VoterFamily.values.find(_.label == label))
  def questionOf(task: Task): Option[String]    = task.payload.get(Question)
  def imdbIdOf(task: Task): Option[String]      = task.payload.get(ImdbId)
}

/** One family question: asked of the family's live source and filed — skipped when the store already holds a fresh
 *  answer — and, once filed, a projection asked for (`filed`), which reads it. */
final class AgreementQuestionHandler(store: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], filed: () => Unit, clock: Clock)
    extends TaskHandler with Logging {
  val taskType: TaskType = TaskType.AgreementQuestion

  def handle(task: Task): HandlerOutcome = {
    val asked = for { family <- AgreementQuestions.familyOf(task); question <- AgreementQuestions.questionOf(task); source <- sources.get(family) }
      yield (family, question, source)
    asked.fold[HandlerOutcome](Skipped) { case (family, question, source) =>
      if (!store.wanted(FamilyAnswerStore.questionId(family, question))) Skipped
      else try { AgreementQuestions.file(store, family, source, question); filed(); Done }
      catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"${family.label} '$question'", clock) }
    }
  }
}

/** TMDB's `find` of an agreed IMDb id: asked through the TMDB client, whose answer the TMDB store files as it files
 *  every TMDB answer — and, once asked, a projection asked for. */
final class AgreementFindHandler(find: String => Unit, filed: () => Unit, clock: Clock) extends TaskHandler {
  val taskType: TaskType = TaskType.AgreementFind

  def handle(task: Task): HandlerOutcome = AgreementQuestions.imdbIdOf(task).fold[HandlerOutcome](Skipped) { imdbId =>
    try { find(imdbId); filed(); Done }
    catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"TMDB find '$imdbId'", clock) }
  }
}
