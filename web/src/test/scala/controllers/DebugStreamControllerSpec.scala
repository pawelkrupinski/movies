package controllers

import tools.SpecClock.given

import tools.SpecTimeouts

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Sink
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers
import play.api.test.Helpers._
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}

import scala.concurrent.Await

/**
 * The dev-only /debug live SSE feed. It must (a) 404 in prod so the worker's
 * `movies` collection is never watched from the web in production, and (b) push
 * a per-change frame — an upsert rendered as the row's HTML, a delete as just
 * the id — so the page can make a merged-away row disappear and a new film
 * appear without a manual refresh. Driven through the real
 * `InMemoryMovieRepository.watchChanges` so the controller↔repository contract is exercised.
 */
class DebugStreamControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("debug-stream-spec")
  private implicit val mat: Materializer = Materializer(sys)

  override def afterAll(): Unit = Await.result(sys.terminate(), SpecTimeouts.Io)

  private def controller(repository: InMemoryMovieRepository, mode: Mode = Mode.Dev) =
    new DebugStreamController(Helpers.stubControllerComponents(),
      DebugCountries.single(new DebugStack(models.Country.default, repository,
        new services.tasks.InMemoryTaskQueue, services.cadence.RatingCadenceReader.empty,
        services.attempts.EnrichmentAttemptReader.empty,
        () => DebugSnapshot(ReadModelDump.empty, None), _ => Some(Seq.empty))),
      mode)

  private def record(title: String) =
    MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title))))

  private val Sentinel   = "Feed Sentinel"
  private val SentinelId = StoredMovieRecord.keyFor(Sentinel, None, titleNormalizer)

  /** Every frame the feed pushes for `act`, and nothing else: the watches are attached when
   *  `eventSource` returns (its queue is pre-materialized), so no write can be missed; and the
   *  feed is read up to a sentinel film written after `act`, so the frames are exactly those
   *  that came before it — no wall-clock window that a slow machine could close early or a
   *  stray frame could fall outside of. */
  private def framesOf(feed: org.apache.pekko.stream.scaladsl.Source[String, ?], movies: InMemoryMovieRepository)(act: => Unit): Seq[String] = {
    val collecting = feed.takeWhile(frame => !frame.contains(s"\"$SentinelId\"")).runWith(Sink.seq)
    act
    movies.upsert(Sentinel, None, record(Sentinel))
    Await.result(collecting, SpecTimeouts.Io)
  }

  "GET /debug/stream" should "404 in production (the collection is never watched from the web there)" in {
    val result = controller(new InMemoryMovieRepository(normalizer = titleNormalizer), Mode.Prod).stream.apply(FakeRequest())
    status(result) shouldBe NOT_FOUND
  }

  it should "serve a text/event-stream in dev" in {
    val result = controller(new InMemoryMovieRepository(normalizer = titleNormalizer), Mode.Dev).stream.apply(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/event-stream")
  }

  "the live feed" should "push an upsert frame carrying the rendered row when a film appears" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val frames = framesOf(controller(repository).eventSource(FakeRequest()), repository)(
      repository.upsert("Belle", Some(2021), record("Belle")))
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "upsert"
    (message \ "id").as[String]   shouldBe StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val html = (message \ "html").as[String]
    html should include("""data-id="""" + StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer))
    html should include("Belle")
  }

  it should "push a delete frame with just the id when a row is removed (a merge)" in {
    val repository = new InMemoryMovieRepository(Seq(("Belle", Some(2021), record("Belle"))), normalizer = titleNormalizer)
    val frames = framesOf(controller(repository).eventSource(FakeRequest()), repository)(
      repository.delete("Belle", Some(2021)))
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "delete"
    (message \ "id").as[String]   shouldBe StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
  }

  it should "emit nothing while the collection is idle" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    framesOf(controller(repository).eventSource(FakeRequest()), repository)(()) shouldBe empty
  }
}
