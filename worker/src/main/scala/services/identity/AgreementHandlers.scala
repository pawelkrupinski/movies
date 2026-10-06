package services.identity

import play.api.Logging
import services.identity.agreement.AgreementStage.PosterQuestion
import services.identity.agreement.VoterFamily
import services.tasks.HandlerOutcome.{Deferred, Done, Reschedule, Skipped}
import services.tasks.{EnqueueResult, HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}
import tools.{CircuitOpenException, HttpStatusException}

import java.time.Clock

/**
 * The agreement's questions as QUEUE TASKS: what the other film database families have not answered yet for the
 * clusters the model left unmatched (`agreement.AgreementStage.wanted`), TMDB's `find` of an agreed IMDb id
 * (`wantedFinds`), and a TMDB record to read again for its release day (`wantedRecords`) — enqueued by the stage the moment it meets them ([[AgreementQuestions.enqueueOpen]]), each asked on
 * the task pool with every other lookup's retries, backoff, breaker and metrics.
 */
object AgreementQuestions {
  private val Family   = "family"
  private val Question = "question"
  private val ImdbId   = "imdbId"
  private val TmdbRecord = "tmdbRecord"
  private val VenuePoster = "venuePoster"
  private val FilmPoster  = "filmPoster"

  /** How far behind every other task an agreement question is claimed: the pipeline's own work — scrapes, ratings, share
   *  cards — always first, the agreement's backlog on what the pool has spare (prod PL 2026-10-04: 5,864 questions queued
   *  at boot ahead of 36 scrape chunks, an hour's drain at the pool's pace). */
  val Behind: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(7, java.util.concurrent.TimeUnit.DAYS)

  /** Every open question, find, poster, catalogue question and record, one task each (catalogue ids a batch each) — one
   *  already queued is not queued again — claimed after every other task ([[Behind]]). */
  def enqueueOpen(queue: TaskQueue, wanted: Set[(VoterFamily, String)], finds: Set[String], clock: Clock,
                  metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent, posters: Set[PosterQuestion] = Set.empty,
                  catalogue: Set[CatalogueQuestion] = Set.empty, records: Set[Int] = Set.empty): Unit = {
    wanted.toSeq.sortBy { case (family, question) => (family.ordinal, question) }.foreach { case (family, question) =>
      metrics.enqueued(family.label, queue.enqueue(TaskType.AgreementQuestion, s"agreement|${family.label}|$question",
        Map(Family -> family.label, Question -> question), submittedAt = clock.instant(), claimAhead = -Behind) == EnqueueResult.Added)
    }
    finds.toSeq.sorted.foreach(imdbId =>
      metrics.enqueued(AgreementQuestionMetrics.TmdbFind, queue.enqueue(TaskType.AgreementFind, s"agreement-find|$imdbId",
        Map(ImdbId -> imdbId), submittedAt = clock.instant(), claimAhead = -Behind) == EnqueueResult.Added))
    // a TMDB question as a find is: one task type, so a worker that predates records skips the task rather than failing it
    records.toSeq.sorted.foreach(tmdbId =>
      metrics.enqueued(AgreementQuestionMetrics.TmdbRecord, queue.enqueue(TaskType.AgreementFind, s"agreement-record|$tmdbId",
        Map(TmdbRecord -> tmdbId.toString), submittedAt = clock.instant(), claimAhead = -Behind) == EnqueueResult.Added))
    posters.toSeq.map(question => question -> PosterAnswers.idOf(question)).sortBy(_._2).foreach { case (question, id) =>
      val payload = question match {
        case PosterQuestion.Venue(url)   => Map(VenuePoster -> url)
        case PosterQuestion.Film(tmdbId) => Map(FilmPoster -> tmdbId.toString)
      }
      metrics.enqueued(AgreementQuestionMetrics.Poster, queue.enqueue(TaskType.AgreementPoster, s"agreement-$id", payload,
        submittedAt = clock.instant(), claimAhead = -Behind) == EnqueueResult.Added)
    }
    AgreementCatalogueQuestions.enqueue(queue, catalogue, clock, metrics)
  }

  /** The statuses that answer a question with nothing — a query the site refuses (Filmweb's 400 for an overlong title),
   *  a page that is not there — filed as such. */
  val NothingThere: Set[Int] = Set(400, 404, 410)

  /** `family`'s answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`, `showing|<venue>`), asked of
   *  `source` and filed. */
  def file(store: FamilyAnswerStore, family: VoterFamily, source: FamilySource, question: String): Unit = { fileAs(store, family, source, question); () }

  /** [[file]], saying whether the family answered or said there was nothing there. */
  def fileAs(store: FamilyAnswerStore, family: VoterFamily, source: FamilySource, question: String): String = {
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

  def familyOf(task: Task): Option[VoterFamily] = task.payload.get(Family).flatMap(label => VoterFamily.values.find(_.label == label))
  def questionOf(task: Task): Option[String]    = task.payload.get(Question)
  def imdbIdOf(task: Task): Option[String]      = task.payload.get(ImdbId)
  def tmdbRecordOf(task: Task): Option[Int]     = task.payload.get(TmdbRecord).flatMap(_.toIntOption)
  def posterOf(task: Task): Option[PosterQuestion] =
    task.payload.get(VenuePoster).map(PosterQuestion.Venue(_)).orElse(task.payload.get(FilmPoster).flatMap(_.toIntOption).map(PosterQuestion.Film(_)))
}

/** One family question: asked of the family's live source and filed — skipped when the store already holds a fresh
 *  answer — and, once filed, a projection asked for (`filed`), which reads it. */
final class AgreementQuestionHandler(store: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], filed: () => Unit, clock: Clock,
                                     metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler with Logging {
  val taskType: TaskType = TaskType.AgreementQuestion

  def handle(task: Task): HandlerOutcome = {
    val asked = for { family <- AgreementQuestions.familyOf(task); question <- AgreementQuestions.questionOf(task); source <- sources.get(family) }
      yield (family, question, source)
    asked.fold[HandlerOutcome](Skipped) { case (family, question, source) =>
      val outcome: HandlerOutcome =
        if (!store.wanted(FamilyAnswerStore.questionId(family, question))) { metrics.asked(family.label, AgreementQuestionMetrics.Fresh); Skipped }
        else try { metrics.asked(family.label, AgreementQuestions.fileAs(store, family, source, question)); filed(); Done }
        catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"${family.label} '$question'", clock) }
      outcome match {
        case _: Deferred   => metrics.asked(family.label, AgreementQuestionMetrics.Deferred)
        case _: Reschedule => metrics.asked(family.label, AgreementQuestionMetrics.Failed)
        case _             => ()
      }
      outcome
    }
  }
}

/** TMDB's `find` of an agreed IMDb id, or its record of a film read again (`reread`): asked through the TMDB client,
 *  whose answer the TMDB store files as it files every TMDB answer — and, once asked, a projection asked for. */
final class AgreementFindHandler(find: String => Unit, reread: Int => Unit, filed: () => Unit, clock: Clock,
                                 metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler {
  val taskType: TaskType = TaskType.AgreementFind

  def handle(task: Task): HandlerOutcome =
    AgreementQuestions.imdbIdOf(task).map(imdbId => asked(AgreementQuestionMetrics.TmdbFind, s"TMDB find '$imdbId'")(find(imdbId)))
      .orElse(AgreementQuestions.tmdbRecordOf(task).map(film => asked(AgreementQuestionMetrics.TmdbRecord, s"TMDB record $film")(reread(film))))
      .getOrElse(Skipped)

  private def asked(label: String, what: String)(ask: => Unit): HandlerOutcome = {
    val outcome = try { ask; filed(); Done }
    catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, what, clock) }
    metrics.asked(label, outcome match {
      case Done          => AgreementQuestionMetrics.Answered
      case _: Deferred   => AgreementQuestionMetrics.Deferred
      case _             => AgreementQuestionMetrics.Failed
    })
    outcome
  }
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
