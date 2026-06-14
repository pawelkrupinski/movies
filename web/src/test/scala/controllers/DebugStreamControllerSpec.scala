package controllers

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
import services.staging.{InMemoryStagingRepository, StagingRecord, StagingRepository}

import scala.concurrent.Await
import scala.concurrent.duration._

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

  override def afterAll(): Unit = Await.result(sys.terminate(), 10.seconds)

  private def controller(repository: InMemoryMovieRepository, mode: Mode = Mode.Dev,
                         staging: StagingRepository = StagingRepository.empty) =
    new DebugStreamController(Helpers.stubControllerComponents(), repository, staging, mode, () => Map.empty)

  private def record(title: String) =
    MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title))))

  "GET /debug/stream" should "404 in production (the collection is never watched from the web there)" in {
    val result = controller(new InMemoryMovieRepository, Mode.Prod).stream.apply(FakeRequest())
    status(result) shouldBe NOT_FOUND
  }

  it should "serve a text/event-stream in dev" in {
    val result = controller(new InMemoryMovieRepository, Mode.Dev).stream.apply(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/event-stream")
  }

  "the live feed" should "push an upsert frame carrying the rendered row when a film appears" in {
    val repository = new InMemoryMovieRepository()
    val collecting = controller(repository).eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100) // let the stream materialize + subscribe before we write
    repository.upsert("Belle", Some(2021), record("Belle"))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "upsert"
    (message \ "id").as[String]   shouldBe StoredMovieRecord.idFor("Belle", Some(2021))
    val html = (message \ "html").as[String]
    html should include("""data-id="""" + StoredMovieRecord.idFor("Belle", Some(2021)))
    html should include("Belle")
    html should include("class=\"reenrich\"") // the row's re-enrich button is present
  }

  it should "push a delete frame with just the id when a row is removed (a merge)" in {
    val repository = new InMemoryMovieRepository(Seq(("Belle", Some(2021), record("Belle"))))
    val collecting = controller(repository).eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100)
    repository.delete("Belle", Some(2021))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "delete"
    (message \ "id").as[String]   shouldBe StoredMovieRecord.idFor("Belle", Some(2021))
  }

  it should "emit nothing while the collection is idle" in {
    val repository = new InMemoryMovieRepository()
    val frames = Await.result(
      controller(repository).eventSource().takeWithin(500.millis).runWith(Sink.seq), 3.seconds)
    frames shouldBe empty
  }

  // The same feed also watches `pending_movies`, tagged `staging-*` so the page
  // routes them to the staging table: a newcomer INSERTs a staging row, a
  // graduation DELETEs it.
  "the live feed" should "push a staging-upsert frame (rendered row) when a pending_movies row appears" in {
    val staging = new InMemoryStagingRepository()
    val collecting = controller(new InMemoryMovieRepository(), staging = staging)
      .eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100)
    staging.upsert(CinemaCityWroclavia, "Newcomer", Some(2026), record("Newcomer"))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "staging-upsert"
    (message \ "id").as[String]   shouldBe StagingRecord.idFor(CinemaCityWroclavia, "Newcomer", Some(2026))
    val html = (message \ "html").as[String]
    html should include ("Newcomer")
    html should include ("""data-anchor="newcomer"""") // hidden source row the page folds by film
  }

  it should "push a staging-delete frame with just the id when a row graduates" in {
    val staging = new InMemoryStagingRepository(Seq(
      (CinemaCityWroclavia, "Newcomer", Some(2026), record("Newcomer"))))
    val collecting = controller(new InMemoryMovieRepository(), staging = staging)
      .eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100)
    staging.delete(CinemaCityWroclavia, "Newcomer", Some(2026))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val message = Json.parse(frames.head.stripPrefix("data: ").trim)
    (message \ "type").as[String] shouldBe "staging-delete"
    (message \ "id").as[String]   shouldBe StagingRecord.idFor(CinemaCityWroclavia, "Newcomer", Some(2026))
  }
}
