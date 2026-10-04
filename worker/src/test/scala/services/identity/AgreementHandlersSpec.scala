package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskType}
import tools.{CircuitOpenException, HttpStatusException, MutableClock}

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/** The agreement's open questions go on the task queue once each; a handler asks the family's source, files the
 *  answer and asks for a projection — a refused query or a missing page filed as no film, a host whose breaker is open
 *  waited out, any other failure asked again. */
class AgreementHandlersSpec extends AnyFlatSpec with Matchers {
  private val clock = new MutableClock(Instant.parse("2026-10-04T00:00:00Z"))
  private def world() = new FamilyAnswerStore(new InMemoryTmdbDocuments, clock)

  /** A family answering every title, or failing every ask with `failure`. */
  private final class Answering(val family: VoterFamily, failure: Option[Throwable] = None) extends FamilySource {
    val asked = new AtomicInteger
    private def answering[A](value: => A): A = { asked.incrementAndGet(); failure.fold(value)(e => throw e) }
    def titled(text: String): Seq[SourceHit]     = answering(Seq(SourceHit("x1", text, None, Some(2020))))
    def directedBy(name: String): Seq[SourceHit] = answering(Nil)
    def record(id: String): Option[SourceRecord] = answering(Some(SourceRecord(IdentityMeasures.Film(id), Map("imdb" -> id))))
  }
  private def task(family: VoterFamily, question: String) =
    Task("t1", TaskType.AgreementQuestion, s"agreement|${family.label}|$question", Map("family" -> family.label, "question" -> question), 1)

  "the agreement's open questions" should "go on the queue once each, however often the stage meets them" in {
    val queue = new InMemoryTaskQueue
    val open  = Set(VoterFamily.Imdb -> "title|Klondike", VoterFamily.RottenTomatoes -> "record|klondike_2022")
    AgreementQuestions.enqueueOpen(queue, open, Set("tt16315948"), clock)
    AgreementQuestions.enqueueOpen(queue, open, Set("tt16315948"), clock)
    queue.monitor().counts.values.sum shouldBe 3
  }

  it should "be claimed after every other task, one enqueued after them included" in {
    val queue = new InMemoryTaskQueue
    AgreementQuestions.enqueueOpen(queue, Set(VoterFamily.Imdb -> "title|Klondike"), Set.empty, clock)
    queue.enqueue(TaskType.ScrapeCinema, "scrape|Kino Muza", submittedAt = clock.instant().plusSeconds(60))
    queue.claim("w1", scala.concurrent.duration.Duration(1, "minute"), clock.instant().plusSeconds(61)).map(_.taskType) shouldBe Some(TaskType.ScrapeCinema)
    queue.claim("w1", scala.concurrent.duration.Duration(1, "minute"), clock.instant().plusSeconds(61)).map(_.taskType) shouldBe Some(TaskType.AgreementQuestion)
  }

  "a question's handler" should "file the family's answer and ask for a projection, then skip it while it is fresh" in {
    val store = world(); val projections = new AtomicInteger
    val imdb  = new Answering(VoterFamily.Imdb)
    val handler = new AgreementQuestionHandler(store, Map(VoterFamily.Imdb -> imdb), () => { projections.incrementAndGet(); () }, clock)
    handler.handle(task(VoterFamily.Imdb, "title|Klondike")) shouldBe HandlerOutcome.Done
    store.answers(VoterFamily.Imdb).titled("Klondike") shouldBe Answer.Known(Seq(SourceHit("x1", "Klondike", None, Some(2020))))
    handler.handle(task(VoterFamily.Imdb, "title|Klondike")) shouldBe HandlerOutcome.Skipped
    (imdb.asked.get, projections.get) shouldBe ((1, 1))
  }

  it should "file a query the site refuses or a page not there as no film" in {
    val store = world()
    val refusing = new Answering(VoterFamily.Filmweb, Some(new HttpStatusException(400, "GET", "https://www.filmweb.pl/api/v1/live/search", None)))
    new AgreementQuestionHandler(store, Map(VoterFamily.Filmweb -> refusing), () => (), clock)
      .handle(task(VoterFamily.Filmweb, "title|Akademia Polskiego Filmu: wykład")) shouldBe HandlerOutcome.Done
    store.answers(VoterFamily.Filmweb).titled("Akademia Polskiego Filmu: wykład") shouldBe Answer.Known(Nil)
  }

  it should "wait out a host whose breaker is open, ask again after any other failure, and file nothing either way" in {
    val store = world()
    val blocked = new Answering(VoterFamily.RottenTomatoes, Some(new CircuitOpenException("www.rottentomatoes.com", 60000L)))
    val down    = new Answering(VoterFamily.Metacritic, Some(new HttpStatusException(503, "GET", "https://www.metacritic.com/search/x", None)))
    val handler = new AgreementQuestionHandler(store, Map(VoterFamily.RottenTomatoes -> blocked, VoterFamily.Metacritic -> down), () => (), clock)
    handler.handle(task(VoterFamily.RottenTomatoes, "title|Dune")) shouldBe a[HandlerOutcome.Deferred]
    handler.handle(task(VoterFamily.Metacritic, "title|Dune")) shouldBe a[HandlerOutcome.Reschedule]
    store.answers(VoterFamily.RottenTomatoes).titled("Dune") shouldBe Answer.Unknown
    store.answers(VoterFamily.Metacritic).titled("Dune") shouldBe Answer.Unknown
  }

  "a find's handler" should "ask TMDB about the agreed IMDb id and ask for a projection" in {
    val found = new java.util.concurrent.ConcurrentLinkedQueue[String]; val projections = new AtomicInteger
    new AgreementFindHandler(id => { found.add(id); () }, () => { projections.incrementAndGet(); () }, clock)
      .handle(Task("t2", TaskType.AgreementFind, "agreement-find|tt16315948", Map("imdbId" -> "tt16315948"), 1)) shouldBe HandlerOutcome.Done
    (found.toArray.toSeq, projections.get) shouldBe ((Seq("tt16315948"), 1))
  }
}
