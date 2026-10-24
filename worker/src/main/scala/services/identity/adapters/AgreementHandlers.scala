package services.identity
package adapters

import play.api.Logging
import services.identity.agreement.AgreementStage.PosterQuestion
import services.identity.agreement.VoterFamily
import services.tasks.HandlerOutcome.{Deferred, Done, Reschedule, Skipped}
import services.tasks.{EnqueueResult, HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}
import tools.{CircuitOpenException, HttpStatusException}

import java.time.Clock

/**
 * The agreement's questions as QUEUE TASKS (`agreement.AgreementStage.wanted`): what the other film database families
 * have not answered yet for the clusters the model left unmatched, TMDB's `find` of an agreed IMDb id, a TMDB record to
 * read again for its release day, a poster to hash and a catalogue page or id — enqueued by the stage the moment it
 * meets them ([[AgreementQuestions.enqueue]]) as one task type ([[TaskType.AskQuestion]]) naming each by its id
 * ([[Question.id]]), and each asked on the task pool ([[AgreementAsker]]) with every other lookup's retries, backoff,
 * breaker and metrics.
 */
object AgreementQuestions {
  private val QuestionsKey = "questions"
  /** Between the ids of a task's questions: a catalogue batch's are many, and no id holds a line break. */
  private val Separator    = "\n"
  private val FamilyKey   = "family"
  private val QuestionKey = "question"
  private val ImdbId   = "imdbId"
  private val TmdbRecord = "tmdbRecord"
  private val VenuePoster = "venuePoster"
  private val FilmPoster  = "filmPoster"

  /** How far behind every other task an agreement question is claimed: the pipeline's own work — scrapes, ratings, share
   *  cards — always first, the agreement's backlog on what the pool has spare (prod PL 2026-10-04: 5,864 questions queued
   *  at boot ahead of 36 scrape chunks, an hour's drain at the pool's pace). */
  val Behind: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(7, java.util.concurrent.TimeUnit.DAYS)

  /** Every agreement question among `questions` as an [[TaskType.AskQuestion]] task — one question each, catalogue ids a
   *  batch each ([[tasks]]) — its ids its payload and, after `agreement|`, its dedup key: one already queued is not
   *  queued again. Claimed after every other task ([[Behind]]) and counted under the question's label ([[labelOf]]). */
  def enqueue(queue: TaskQueue, questions: Set[Question], clock: Clock,
              metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent): Unit =
    tasks(questions).foreach { asked =>
      val payload = payloadOf(asked)
      metrics.enqueued(labelOf(asked.head), queue.enqueue(TaskType.AskQuestion, s"agreement|${payload(QuestionsKey)}", payload,
        submittedAt = clock.instant(), claimAhead = -Behind) == EnqueueResult.Added)
    }

  /** The payload of the [[TaskType.AskQuestion]] task asking `questions`: their ids. */
  def payloadOf(questions: Seq[Question]): Map[String, String] = Map(QuestionsKey -> questions.flatMap(_.id).mkString(Separator))

  /** The agreement questions among `questions` as the tasks that ask them, in the order they are queued: each family's
   *  questions, finds, records, posters, a source's catalogue ids [[AgreementCatalogueQuestions.Batch]] at a time (one
   *  SPARQL query), catalogue pages. */
  private[adapters] def tasks(questions: Set[Question]): Seq[Seq[Question]] = {
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

  /** What a question is counted under ([[AgreementQuestionMetrics]]): its family's label, else its kind's. */
  def labelOf(question: Question): String = question match {
    case Question.Family(family, _) => family.label
    case Question.Find(_)           => AgreementQuestionMetrics.TmdbFind
    case Question.Record(_)         => AgreementQuestionMetrics.TmdbRecord
    case Question.Poster(_)         => AgreementQuestionMetrics.Poster
    case Question.Catalogue(_)      => AgreementQuestionMetrics.Catalogue
    case Question.Candidate(_) | Question.FilmRecord(_) | Question.VenuePage(_) => question.kind.toString
  }

  /** The questions a task asks: an [[TaskType.AskQuestion]]'s by their ids, and a task of the four types before it by
   *  their payloads. */
  def questionsOf(task: Task): Seq[Question] = task.taskType match {
    case TaskType.AskQuestion        => task.payload.get(QuestionsKey).toSeq.flatMap(_.split(Separator)).flatMap(Question.parse)
    case TaskType.AgreementQuestion  => (for { family <- familyOf(task); question <- questionOf(task) } yield Question.Family(family, question)).toSeq
    case TaskType.AgreementFind      => imdbIdOf(task).map(Question.Find(_)).orElse(tmdbRecordOf(task).map(Question.Record(_))).toSeq
    case TaskType.AgreementPoster    => posterOf(task).map(Question.Poster(_)).toSeq
    case TaskType.AgreementCatalogue => AgreementCatalogueQuestions.of(task).map(Question.Catalogue(_))
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

  /** A failed ask as the queue takes it: a host whose breaker is open was never asked — the attempt is given back and
   *  the task waits the block out — and anything else is asked again on the queue's backoff. */
  def failed(e: Throwable, what: String, clock: Clock): HandlerOutcome = e match {
    case open: CircuitOpenException => Deferred(Some(s"$what: ${open.getMessage}"), Some(clock.instant().plusMillis(open.openForMs)))
    case other                      => Reschedule(Some(s"$what: ${other.getMessage}"))
  }

  /** One question asked as the asker asks every one: `ask` run — it says how it came out — then a projection asked for
   *  (`filed`); a failure [[failed]]. Counted under `label` as the ask said, [[AgreementQuestionMetrics.Deferred]] or
   *  [[AgreementQuestionMetrics.Failed]]. */
  def asked(label: String, what: String, metrics: AgreementQuestionMetrics, clock: Clock, filed: () => Unit)(ask: => String): HandlerOutcome =
    try { val outcome = ask; filed(); metrics.asked(label, outcome); Done }
    catch { case scala.util.control.NonFatal(e) =>
      val outcome = failed(e, what, clock)
      metrics.asked(label, outcome match {
        case _: Deferred => AgreementQuestionMetrics.Deferred
        case _           => AgreementQuestionMetrics.Failed
      })
      outcome
    }

  // The payloads of the four task types before AskQuestion. Old queued tasks only; remove after 2026-11-07.
  def familyOf(task: Task): Option[VoterFamily] = task.payload.get(FamilyKey).flatMap(label => VoterFamily.values.find(_.label == label))
  def questionOf(task: Task): Option[String]    = task.payload.get(QuestionKey)
  def imdbIdOf(task: Task): Option[String]      = task.payload.get(ImdbId)
  def tmdbRecordOf(task: Task): Option[Int]     = task.payload.get(TmdbRecord).flatMap(_.toIntOption)
  def posterOf(task: Task): Option[PosterQuestion] =
    task.payload.get(VenuePoster).map(PosterQuestion.Venue(_)).orElse(task.payload.get(FilmPoster).flatMap(_.toIntOption).map(PosterQuestion.Film(_)))
}

/**
 * Asks the agreement's questions, each by its kind: a family's search, record or programme of the family's live source
 * (`sources`), filed; TMDB's find of an agreed IMDb id (`find`) or its record of a film read again (`reread`), filed by
 * the TMDB store as it files every TMDB answer; a poster hashed (`hashing`) and filed, and given up on once its fetch
 * has failed every attempt the queue allows it ([[PosterAnswerStore.GiveUpAttempts]]); a batch of catalogue ids mapped
 * at once (`mapping`), a catalogue page's links read (`pages`). A question whose store already holds a fresh answer is
 * skipped ([[AgreementQuestionMetrics.Fresh]]); once one is filed a projection is asked for (`filed`), which reads it;
 * a failure is the queue's to ask again ([[AgreementQuestions.failed]]).
 *
 * The queue's handler of [[TaskType.AskQuestion]] — and, through [[oldQueued]], of the four task types before it — and
 * what [[InlineFamilyAsks]] asks a new cluster's family questions through.
 */
final class AgreementAsker(families: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], find: String => Unit, reread: Int => Unit,
                           posters: PosterAnswerStore, hashing: PosterHashing, catalogue: CatalogueAnswerStore, mapping: CatalogueMapping,
                           pages: CatalogueLinkReader, filed: () => Unit, clock: Clock,
                           metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler {
  import AgreementQuestionMetrics.{Fresh, Answered}

  val taskType: TaskType = TaskType.AskQuestion

  def handle(task: Task): HandlerOutcome = ask(AgreementQuestions.questionsOf(task), task.attempts)

  /** The four task types before [[TaskType.AskQuestion]], their tasks still queued when it was deployed: each decoded
   *  into its questions and asked as one. Old queued tasks only; remove after 2026-11-07. */
  def oldQueued: Seq[TaskHandler] =
    Seq(TaskType.AgreementQuestion, TaskType.AgreementFind, TaskType.AgreementPoster, TaskType.AgreementCatalogue).map { old =>
      new TaskHandler {
        val taskType: TaskType = old
        def handle(task: Task): HandlerOutcome = AgreementAsker.this.handle(task)
      }
    }

  /** One task's questions asked — its one question, or a batch of catalogue questions as one — on its `attempts`th
   *  attempt. `announce`: whether a filing asks for a projection — always on the queue; inline, only once the resolve
   *  stopped waiting for it. */
  def ask(questions: Seq[Question], attempts: Int = 1, announce: => Boolean = true): HandlerOutcome = {
    val announced: () => Unit = () => if (announce) filed()
    questions match {
      case Seq(question) => ask(question, attempts, announced)
      case batch if batch.nonEmpty && batch.forall(_.isInstanceOf[Question.Catalogue]) =>
        askCatalogue(batch.collect { case Question.Catalogue(question) => question }, announced)
      case _ => Skipped
    }
  }

  private def ask(question: Question, attempts: Int, filed: () => Unit): HandlerOutcome = question match {
    case Question.Family(family, asked) =>
      sources.get(family).fold[HandlerOutcome](Skipped) { source =>
        if (!families.wanted(FamilyAnswerStore.questionId(family, asked))) fresh(family.label)
        else AgreementQuestions.asked(family.label, s"${family.label} '$asked'", metrics, clock, filed)(
          AgreementQuestions.file(families, family, source, asked))
      }
    case Question.Find(imdbId) =>
      answered(AgreementQuestionMetrics.TmdbFind, s"TMDB find '$imdbId'", filed)(find(imdbId))
    case Question.Record(film) =>
      answered(AgreementQuestionMetrics.TmdbRecord, s"TMDB record $film", filed)(reread(film))
    case Question.Poster(poster) =>
      if (!posters.wanted(poster)) fresh(AgreementQuestionMetrics.Poster)
      else answered(AgreementQuestionMetrics.Poster, s"poster ${PosterAnswers.idOf(poster)}", filed)(posters.file(poster, hashing.of(poster))) match {
        // its last attempt failed too: given up on, read as unread rather than waited on for ever
        case _: Reschedule if attempts >= PosterAnswerStore.GiveUpAttempts => posters.giveUp(poster); filed(); Done
        case other => other
      }
    case Question.Catalogue(asked) => askCatalogue(Seq(asked), filed)
    case Question.Candidate(_) | Question.FilmRecord(_) | Question.VenuePage(_) => Skipped
  }

  /** Catalogue questions asked as one: the ids mapped in one batch, each page read — skipped when the store holds fresh
   *  answers to them all. */
  private def askCatalogue(questions: Seq[CatalogueQuestion], filed: () => Unit): HandlerOutcome =
    if (!questions.exists(catalogue.wanted)) fresh(AgreementQuestionMetrics.Catalogue)
    else answered(AgreementQuestionMetrics.Catalogue, s"catalogue ${questions.flatMap(Question.Catalogue(_).id).mkString(" ")}", filed) {
      AgreementCatalogueQuestions.file(catalogue, mapping, pages, questions); ()
    }

  private def fresh(label: String): HandlerOutcome = { metrics.asked(label, Fresh); Skipped }

  private def answered(label: String, what: String, filed: () => Unit)(ask: => Unit): HandlerOutcome =
    AgreementQuestions.asked(label, what, metrics, clock, filed) { ask; Answered }
}

/** Where the agreement's questions report: each one enqueued (`added`, or already queued), and each one asked, by how it
 *  came out — [[Answered]], [[Nothing]] there (a refused query, a missing page: filed as no film), [[Fresh]] (an answer
 *  already filed: skipped), [[Deferred]] (the host's breaker open) or [[Failed]] (asked again on the queue's backoff). A
 *  family is its label (`imdb`, `rt` …), TMDB's find of an agreed IMDb id [[TmdbFind]], a TMDB record read again for its
 *  release day [[TmdbRecord]]. */
trait AgreementQuestionMetrics {
  def enqueued(family: String, added: Boolean): Unit
  def asked(family: String, outcome: String): Unit
}

object AgreementQuestionMetrics {
  val Answered = "answered"; val Nothing = "nothing"; val Fresh = "fresh"; val Deferred = "deferred"; val Failed = "failed"
  val Outcomes: Seq[String] = Seq(Answered, Nothing, Fresh, Deferred, Failed)
  val TmdbFind = "tmdb-find"
  val TmdbRecord = "tmdb-record"
  /** A poster hashed for the agreement's poster evidence. */
  val Poster = "poster"
  /** A catalogue id batch mapped, or a venue page's catalogue links read, for the agreement's catalogue take. */
  val Catalogue = "catalogue"
  val Silent: AgreementQuestionMetrics = new AgreementQuestionMetrics {
    def enqueued(family: String, added: Boolean): Unit = ()
    def asked(family: String, outcome: String): Unit = ()
  }
}

/** The agreement's [[services.identity.agreement.AgreementStage.AskNow]]: a new cluster's family questions, each asked
 *  through `asker` — as the queue asks it — on `pool` at once, waited on for at most `within`. One still out by then is
 *  filed when it comes and asks for a projection, as on the queue; the queue holds it too, and skips it once it is
 *  filed. A failure is the queue's to ask again. */
final class InlineFamilyAsks(asker: AgreementAsker, pool: java.util.concurrent.ExecutorService, stopwatch: tools.Stopwatch)
    extends services.identity.agreement.AgreementStage.AskNow with Logging {
  def apply(questions: Set[Question], within: scala.concurrent.duration.FiniteDuration): Unit = {
    val started = stopwatch.start()
    val late    = new java.util.concurrent.atomic.AtomicBoolean(false)
    val asked = questions.toSeq.collect { case question: Question.Family =>
      pool.submit(new Runnable {
        def run(): Unit = tools.HttpPath.within(tools.HttpPath.Agreement)(asker.ask(Seq(question), announce = late.get)) match {
          case outcome @ (_: Reschedule | _: Deferred) =>
            logger.info(s"agreement: ${question.family.label} '${question.question}' asked inline failed ($outcome), the queue's to ask again")
          case _ => ()
        }
      })
    }
    asked.foreach { future =>
      try future.get((within - started.elapsed).toNanos.max(0L), java.util.concurrent.TimeUnit.NANOSECONDS)
      catch { case _: java.util.concurrent.TimeoutException | _: java.util.concurrent.ExecutionException => () }
    }
    late.set(true)
  }
}
