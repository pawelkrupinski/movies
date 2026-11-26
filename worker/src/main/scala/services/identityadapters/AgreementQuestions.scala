package services.identityadapters

import services.tmdb.{FamilyAnswerStore, QuestionMemory}
import java.time.Clock

import services.identity.*
import services.identity.vocabulary.VoterFamily
import services.identity.vocabulary.{CatalogueQuestion, PosterAnswers, Question, QuestionSystem}
import services.tasks.{Enqueued, Task, TaskQueue}
import services.tasktypes.TaskType
import tools.{CircuitOpenException, HttpStatusException, UnexpectedBodyException}

/**
 * The agreement's questions as QUEUE TASKS (`agreement.AgreementStage.wanted`): what the other film database families
 * have not answered yet for the clusters the model left unmatched, TMDB's `find` of an agreed IMDb id, a TMDB record to
 * read again for its release day, a poster to hash and a catalogue page or id — enqueued by the stage the moment it
 * meets them ([[AgreementQuestions.enqueue]]) as one task type ([[TaskType.AskQuestion]]) naming each by its id
 * ([[Question.id]]), and each asked on the task pool ([[AgreementAsker]]) with every other lookup's breaker and metrics.
 * A question its source failed to answer is remembered ([[QuestionMemory]]) and neither queued nor asked again until its
 * wait ends.
 */
object AgreementQuestions {
  private val QuestionsKey = "questions"
  /** Between the ids of a task's questions: a catalogue batch's are many, and no id holds a line break. */
  private val Separator    = "\n"

  /** How far behind every other task an agreement question is claimed: the pipeline's own work — scrapes, ratings, share
   *  cards — always first, the agreement's backlog on what the pool has spare (prod PL 2026-10-04: 5,864 questions queued
   *  at boot ahead of 36 scrape chunks, an hour's drain at the pool's pace). */
  val Behind: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(7, java.util.concurrent.TimeUnit.DAYS)

  /** Every agreement question among `questions` still due (`memory`: not inside the wait a failed ask left it) as an
   *  [[TaskType.AskQuestion]] task — one question each, catalogue ids a batch each ([[tasks]]) — its ids its payload and,
   *  after `agreement|`, its dedup key: one already queued is not queued again. Claimed after every other task
   *  ([[Behind]]; a listing the model's input gate holds asks its own at `claimAhead` 0, [[AgreementReadiness]]) and counted under the question's label ([[labelOf]]). What the enqueues came to: how many tasks
   *  were added, and how many the queue could not answer. */
  def enqueue(queue: TaskQueue, questions: Set[Question], memory: QuestionMemory, clock: Clock,
              metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent,
              claimAhead: scala.concurrent.duration.FiniteDuration = -Behind): Enqueued =
    tasks(memory.due(questions.filter(_.kind.system == QuestionSystem.Agreement))).foldLeft(Enqueued.Zero) { (sum, asked) =>
      val payload = payloadOf(asked)
      val key     = s"agreement|${payload(QuestionsKey)}"
      val queued  = queue.enqueue(TaskType.AskQuestion, key, payload, submittedAt = clock.instant(), claimAhead = claimAhead)
      // an ask ahead of the agreement's own, its question already queued behind them: the queued task moved up to this
      // ask's place -- else it kept its place 7 days behind, and a listing the model's input gate held on it waited for an
      // idle queue (prod, 2026-11-06: held 1.6-2.8 h)
      if (queued == services.tasks.EnqueueResult.Duplicate && claimAhead > -Behind)
        queue.raise(key, clock.instant().minusMillis(claimAhead.toMillis))
      val result  = Enqueued.of(queued)
      metrics.enqueued(labelOf(asked.head), result.added > 0)
      sum + result
    }

  /** The payload of the [[TaskType.AskQuestion]] task asking `questions`: their ids. */
  def payloadOf(questions: Seq[Question]): Map[String, String] = Map(QuestionsKey -> questions.flatMap(_.id).mkString(Separator))

  /** The agreement questions among `questions` as the tasks that ask them, in the order they are queued: each family's
   *  questions, finds, records, posters, a source's catalogue ids [[AgreementCatalogueQuestions.Batch]] at a time (one
   *  SPARQL query), catalogue pages. */
  private[services] def tasks(questions: Set[Question]): Seq[Seq[Question]] = {
    def each[K](key: PartialFunction[Question, K])(using Ordering[K]): Seq[Seq[Question]] =
      questions.toSeq.collect { case question if key.isDefinedAt(question) => key(question) -> question }.sortBy(_._1).map(asked => Seq(asked._2))
    val ids = questions.toSeq.collect { case Question.Catalogue(CatalogueQuestion.Id(id)) => id }
    each { case Question.Family(family, question) => (family.ordinal, question) } ++
      each { case Question.Find(imdbId) => imdbId } ++
      each { case Question.Record(tmdbId) => tmdbId } ++
      each { case Question.Poster(poster) => PosterAnswers.idOf(poster) } ++
      ids.groupBy(_.source).toSeq.sortBy(_._1).flatMap { case (_, of) =>
        of.sortBy(_.id).grouped(AgreementCatalogueQuestions.Batch).map(_.map(id => Question.Catalogue(CatalogueQuestion.Id(id))))
      } ++
      each { case Question.Catalogue(CatalogueQuestion.Page(url)) => url }
  }

  /** The host a failed ask's request went to: the one its failure names (a status, a breaker that is open, a body that
   *  could not be read, through any wrapping), else the last host the ask requested (`requested`, noted by the host
   *  breaker: a timeout or a refused connection names none). Lower-cased, as the host breakers name it
   *  ([[HostRecovery]] releases a host's failures by that name). */
  def hostOf(failure: Throwable, requested: Option[String] = None): Option[String] = {
    def urlHost(url: String) = tools.UrlHost.of(url)
    def named(e: Throwable, depth: Int): Option[String] = e match {
      case null                          => None
      case status: HttpStatusException   => urlHost(status.url)
      case open: CircuitOpenException    => Some(open.host)
      case body: UnexpectedBodyException => urlHost(body.url)
      case other if depth < 4            => named(other.getCause, depth + 1)
      case _                             => None
    }
    named(failure, 0).orElse(requested).map(_.toLowerCase(java.util.Locale.ROOT))
  }

  /** `ask` run with each host it requests noted (`tools.RequestedHost`): what it came to, and the host a failure of it
   *  went to -- [[hostOf]] the failure, else the last host it requested. ONE noting for the agreement's asks, the fill's
   *  and the model's live ones. */
  def hostsNoted[A](ask: => A): (A, Throwable => Option[String]) = {
    val requested = new tools.RequestedHost.Recording
    (tools.RequestedHost.within(requested)(ask), hostOf(_, requested.last))
  }

  /** What a question is counted under ([[AgreementQuestionMetrics]]): its family's label, else its kind's. */
  def labelOf(question: Question): String = question match {
    case Question.Family(family, _) => family.label
    case Question.Find(_)           => AgreementQuestionMetrics.TmdbFind
    case Question.Record(_)         => AgreementQuestionMetrics.TmdbRecord
    case Question.Poster(_)         => AgreementQuestionMetrics.Poster
    case Question.Catalogue(_)      => AgreementQuestionMetrics.Catalogue
    case Question.Candidate(_) | Question.FilmRecord(_) | Question.Person(_) | Question.VenuePage(_) => question.kind.toString
  }

  /** The questions a task asks: an [[TaskType.AskQuestion]]'s by their ids, and a task of the four types before it by
   *  their payloads. */
  def questionsOf(task: Task): Seq[Question] = task.taskType match {
    case TaskType.AskQuestion        => task.payload.get(QuestionsKey).toSeq.flatMap(_.split(Separator)).flatMap(Question.parse)
    case _                           => Nil
  }

  /** The statuses that answer a question with nothing — a query the site refuses (Filmweb's 400 for an overlong title),
   *  a page that is not there — filed as such. */
  val NothingThere: Set[Int] = Set(400, 404, 410)

  /** `family`'s answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`, `showing|<venue>`), asked of
   *  `source` and filed — saying whether the family answered or said there was nothing there. */
  def file(store: FamilyAnswerStore, family: VoterFamily, source: FamilySource, question: String): String = {
    var outcome = AgreementQuestionMetrics.Answered
    def answer[A](read: => A, nothing: A): A =
      try read catch { case e: HttpStatusException if NothingThere(e.code) => outcome = AgreementQuestionMetrics.Nothing; nothing }
    question.split("\\|", 2) match {
      case Array("title", text)    => store.fileTitled(family, text, answer(source.titled(text), Nil))
      case Array("director", name) => store.fileDirected(family, name, answer(source.directedBy(name), Nil))
      case Array("record", id)     => store.fileRecord(family, id, answer(source.record(id), None))
      case Array("showing", venue) => store.fileShowing(family, venue, answer(source.showing(venue), Nil))
      case _                       => throw new IllegalArgumentException(s"no such question: $question")
    }
    outcome
  }
}
