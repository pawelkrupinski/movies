package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskType}
import tools.{CircuitOpenException, HttpStatusException, MutableClock}

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/** The agreement's open questions go on the task queue once each, one task type naming them by id; the asker asks the
 *  family's source, files the answer and asks for a projection — a refused query or a missing page filed as no film, a
 *  host whose breaker is open waited out, any other failure asked again — and asks the tasks of the types before it
 *  still queued as it asks its own. */
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
  private def task(family: VoterFamily, question: String) = AgreementAskers.task(Question.Family(family, question))()
  private def asker(store: FamilyAnswerStore, sources: Map[VoterFamily, FamilySource], filed: () => Unit = () => (),
                    metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) =
    AgreementAskers.over(store, clock)(sources = sources, filed = filed, metrics = metrics)

  "the agreement's open questions" should "go on the queue once each, however often the stage meets them" in {
    val queue = new InMemoryTaskQueue
    val open  = Set(Question.Family(VoterFamily.Imdb, "title|Klondike"), Question.Family(VoterFamily.RottenTomatoes, "record|klondike_2022"),
      Question.Find("tt16315948"))
    AgreementQuestions.enqueue(queue, open, clock)
    AgreementQuestions.enqueue(queue, open, clock)
    queue.monitor().counts.values.sum shouldBe 3
  }

  it should "be claimed after every other task, one enqueued after them included" in {
    val queue = new InMemoryTaskQueue
    AgreementQuestions.enqueue(queue, Set(Question.Family(VoterFamily.Imdb, "title|Klondike")), clock)
    queue.enqueue(TaskType.ScrapeCinema, "scrape|Kino Muza", submittedAt = clock.instant().plusSeconds(60))
    queue.claim("w1", scala.concurrent.duration.Duration(1, "minute"), clock.instant().plusSeconds(61)).map(_.taskType) shouldBe Some(TaskType.ScrapeCinema)
    queue.claim("w1", scala.concurrent.duration.Duration(1, "minute"), clock.instant().plusSeconds(61)).map(_.taskType) shouldBe Some(TaskType.AskQuestion)
  }

  "a family question" should "file the family's answer and ask for a projection, then skip it while it is fresh" in {
    val store = world(); val projections = new AtomicInteger
    val imdb  = new Answering(VoterFamily.Imdb)
    val handler = asker(store, Map(VoterFamily.Imdb -> imdb), () => { projections.incrementAndGet(); () })
    handler.handle(task(VoterFamily.Imdb, "title|Klondike")) shouldBe HandlerOutcome.Done
    store.answers(VoterFamily.Imdb).titled("Klondike") shouldBe Answer.Known(Seq(SourceHit("x1", "Klondike", None, Some(2020))))
    handler.handle(task(VoterFamily.Imdb, "title|Klondike")) shouldBe HandlerOutcome.Skipped
    (imdb.asked.get, projections.get) shouldBe ((1, 1))
  }

  it should "file a venue's programme the family's site lists" in {
    val store     = world()
    val programme = Seq(services.identity.agreement.Showing("10085635", ScreeningDays.of(Seq(java.time.LocalDate.parse("2026-10-07")))))
    val filmweb   = new FilmwebFamily(new services.enrichment.FilmwebClient(new clients.tools.FakeHttpFetch("filmweb-programmes", strict = true)),
      venue => if (venue == "Kozienicki Dom Kultury") programme else Nil)
    asker(store, Map(VoterFamily.Filmweb -> filmweb))
      .handle(task(VoterFamily.Filmweb, "showing|Kozienicki Dom Kultury")) shouldBe HandlerOutcome.Done
    store.answers(VoterFamily.Filmweb).showing("Kozienicki Dom Kultury") shouldBe Answer.Known(programme)
  }

  it should "file a query the site refuses or a page not there as no film" in {
    val store = world()
    val refusing = new Answering(VoterFamily.Filmweb, Some(new HttpStatusException(400, "GET", "https://www.filmweb.pl/api/v1/live/search", None)))
    asker(store, Map(VoterFamily.Filmweb -> refusing))
      .handle(task(VoterFamily.Filmweb, "title|Akademia Polskiego Filmu: wykład")) shouldBe HandlerOutcome.Done
    store.answers(VoterFamily.Filmweb).titled("Akademia Polskiego Filmu: wykład") shouldBe Answer.Known(Nil)
  }

  it should "wait out a host whose breaker is open, ask again after any other failure, and file nothing either way" in {
    val store = world()
    val blocked = new Answering(VoterFamily.RottenTomatoes, Some(new CircuitOpenException("www.rottentomatoes.com", 60000L)))
    val down    = new Answering(VoterFamily.Metacritic, Some(new HttpStatusException(503, "GET", "https://www.metacritic.com/search/x", None)))
    val handler = asker(store, Map(VoterFamily.RottenTomatoes -> blocked, VoterFamily.Metacritic -> down))
    handler.handle(task(VoterFamily.RottenTomatoes, "title|Dune")) shouldBe a[HandlerOutcome.Deferred]
    handler.handle(task(VoterFamily.Metacritic, "title|Dune")) shouldBe a[HandlerOutcome.Reschedule]
    store.answers(VoterFamily.RottenTomatoes).titled("Dune") shouldBe Answer.Unknown
    store.answers(VoterFamily.Metacritic).titled("Dune") shouldBe Answer.Unknown
  }

  "the agreement's questions" should "report each enqueue's result and each question's outcome" in {
    val enqueues = scala.collection.mutable.ArrayBuffer.empty[(String, Boolean)]
    val outcomes = scala.collection.mutable.ArrayBuffer.empty[(String, String)]
    val metrics  = new AgreementQuestionMetrics {
      def enqueued(family: String, added: Boolean): Unit = { enqueues += family -> added; () }
      def asked(family: String, outcome: String): Unit   = { outcomes += family -> outcome; () }
    }
    val queue = new InMemoryTaskQueue
    AgreementQuestions.enqueue(queue, Set(Question.Family(VoterFamily.Imdb, "title|Klondike"), Question.Find("tt1")), clock, metrics)
    AgreementQuestions.enqueue(queue, Set(Question.Family(VoterFamily.Imdb, "title|Klondike")), clock, metrics)
    enqueues.toSeq shouldBe Seq("imdb" -> true, "tmdb-find" -> true, "imdb" -> false)
    val store = world()
    val handler = asker(store, Map(VoterFamily.Imdb -> new Answering(VoterFamily.Imdb),
      VoterFamily.Metacritic -> new Answering(VoterFamily.Metacritic, Some(new HttpStatusException(503, "GET", "https://www.metacritic.com/x", None))),
      VoterFamily.Filmweb -> new Answering(VoterFamily.Filmweb, Some(new HttpStatusException(404, "GET", "https://www.filmweb.pl/x", None))),
      VoterFamily.RottenTomatoes -> new Answering(VoterFamily.RottenTomatoes, Some(new CircuitOpenException("www.rottentomatoes.com", 60000L)))),
      metrics = metrics)
    handler.handle(task(VoterFamily.Imdb, "title|Klondike")); handler.handle(task(VoterFamily.Imdb, "title|Klondike"))
    handler.handle(task(VoterFamily.Metacritic, "title|Dune")); handler.handle(task(VoterFamily.Filmweb, "title|Dune"))
    handler.handle(task(VoterFamily.RottenTomatoes, "title|Dune"))
    val finds = AgreementAskers.over(store, clock)(find = id => if (id == "tt2") throw new CircuitOpenException("api.themoviedb.org", 60000L) else
      if (id == "tt3") throw new java.io.IOException("reset") else (), metrics = metrics)
    Seq("tt1", "tt2", "tt3").foreach(id => finds.handle(AgreementAskers.task(Question.Find(id))()))
    outcomes.toSeq shouldBe Seq("imdb" -> "answered", "imdb" -> "fresh", "metacritic" -> "failed", "filmweb" -> "nothing",
      "rt" -> "deferred", "tmdb-find" -> "answered", "tmdb-find" -> "deferred", "tmdb-find" -> "failed")
  }

  "a find's handler" should "ask TMDB about the agreed IMDb id and ask for a projection" in {
    val found = new java.util.concurrent.ConcurrentLinkedQueue[String]; val projections = new AtomicInteger
    AgreementAskers.over(world(), clock)(find = id => { found.add(id); () }, filed = () => { projections.incrementAndGet(); () })
      .handle(AgreementAskers.task(Question.Find("tt16315948"))()) shouldBe HandlerOutcome.Done
    (found.toArray.toSeq, projections.get) shouldBe ((Seq("tt16315948"), 1))
  }

  "a record the broadcast take waits on" should "be queued once, read again through TMDB, and ask for a projection" in {
    val queue = new InMemoryTaskQueue
    val enqueues = scala.collection.mutable.Buffer.empty[(String, Boolean)]
    val metrics = new AgreementQuestionMetrics {
      def enqueued(family: String, added: Boolean): Unit = { enqueues += family -> added; () }
      def asked(family: String, outcome: String): Unit   = { enqueues += family -> (outcome == AgreementQuestionMetrics.Answered); () }
    }
    AgreementQuestions.enqueue(queue, Set(Question.Record(1703624)), clock, metrics)
    AgreementQuestions.enqueue(queue, Set(Question.Record(1703624)), clock, metrics)
    queue.monitor().counts.values.sum shouldBe 1
    val task = queue.claim("w1", scala.concurrent.duration.Duration(1, "minute"), clock.instant()).get
    val reread = scala.collection.mutable.Buffer.empty[Int]; val projections = new AtomicInteger
    AgreementAskers.over(world(), clock)(reread = film => { reread += film; () }, filed = () => { projections.incrementAndGet(); () }, metrics = metrics)
      .handle(task) shouldBe HandlerOutcome.Done
    (reread.toSeq, projections.get) shouldBe ((Seq(1703624), 1))
    enqueues.toSeq shouldBe Seq("tmdb-record" -> true, "tmdb-record" -> false, "tmdb-record" -> true)
  }

  // the four task types AskQuestion replaced: their tasks still queued at its deploy are asked as its own are
  "a task of a type before AskQuestion" should "be asked as the question its payload names" in {
    val store = world()
    val found = scala.collection.mutable.Buffer.empty[String]; val reread = scala.collection.mutable.Buffer.empty[Int]
    val catalogue = new CatalogueAnswerStore(store, clock, Nil)
    val mapped = scala.collection.mutable.Buffer.empty[Seq[CatalogueId]]
    val old = AgreementAskers.over(store, clock)(sources = Map(VoterFamily.Imdb -> new Answering(VoterFamily.Imdb)),
      find = id => { found += id; () }, reread = film => { reread += film; () }, catalogue = catalogue,
      mapping = ids => { mapped += ids; Map.empty }).oldQueued.map(handler => handler.taskType -> handler).toMap
    old.keySet shouldBe Set(TaskType.AgreementQuestion, TaskType.AgreementFind, TaskType.AgreementPoster, TaskType.AgreementCatalogue)
    def asked(taskType: TaskType, payload: (String, String)*) = old(taskType).handle(Task("t", taskType, "old", payload.toMap, 1))
    asked(TaskType.AgreementQuestion, "family" -> "imdb", "question" -> "title|Klondike") shouldBe HandlerOutcome.Done
    asked(TaskType.AgreementFind, "imdbId" -> "tt16315948") shouldBe HandlerOutcome.Done
    asked(TaskType.AgreementFind, "tmdbRecord" -> "1703624") shouldBe HandlerOutcome.Done
    asked(TaskType.AgreementCatalogue, "source" -> "webedia", "ids" -> "1,2") shouldBe HandlerOutcome.Done
    (store.answers(VoterFamily.Imdb).titled("Klondike").toOption.map(_.size), found.toSeq, reread.toSeq, mapped.toSeq) shouldBe
      ((Some(1), Seq("tt16315948"), Seq(1703624), Seq(Seq(CatalogueId("webedia", "1"), CatalogueId("webedia", "2")))))
    AgreementQuestions.questionsOf(Task("t", TaskType.AgreementPoster, "old", Map("filmPoster" -> "7"), 1)) shouldBe
      Seq(Question.Poster(services.identity.agreement.AgreementStage.PosterQuestion.Film(7)))
  }
}
