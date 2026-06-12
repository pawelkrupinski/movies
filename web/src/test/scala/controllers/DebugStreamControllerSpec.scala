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
import services.movies.{InMemoryMovieRepo, StoredMovieRecord}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The dev-only /debug live SSE feed. It must (a) 404 in prod so the worker's
 * `movies` collection is never watched from the web in production, and (b) push
 * a per-change frame — an upsert rendered as the row's HTML, a delete as just
 * the id — so the page can make a merged-away row disappear and a new film
 * appear without a manual refresh. Driven through the real
 * `InMemoryMovieRepo.watchChanges` so the controller↔repo contract is exercised.
 */
class DebugStreamControllerSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("debug-stream-spec")
  private implicit val mat: Materializer = Materializer(sys)

  override def afterAll(): Unit = Await.result(sys.terminate(), 10.seconds)

  private def controller(repo: InMemoryMovieRepo, mode: Mode = Mode.Dev) =
    new DebugStreamController(Helpers.stubControllerComponents(), repo, mode, () => Map.empty)

  private def record(title: String) =
    MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title))))

  "GET /debug/stream" should "404 in production (the collection is never watched from the web there)" in {
    val result = controller(new InMemoryMovieRepo, Mode.Prod).stream.apply(FakeRequest())
    status(result) shouldBe NOT_FOUND
  }

  it should "serve a text/event-stream in dev" in {
    val result = controller(new InMemoryMovieRepo, Mode.Dev).stream.apply(FakeRequest())
    status(result) shouldBe OK
    contentType(result) shouldBe Some("text/event-stream")
  }

  "the live feed" should "push an upsert frame carrying the rendered row when a film appears" in {
    val repo = new InMemoryMovieRepo()
    val collecting = controller(repo).eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100) // let the stream materialize + subscribe before we write
    repo.upsert("Belle", Some(2021), record("Belle"))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val msg = Json.parse(frames.head.stripPrefix("data: ").trim)
    (msg \ "type").as[String] shouldBe "upsert"
    (msg \ "id").as[String]   shouldBe StoredMovieRecord.idFor("Belle", Some(2021))
    val html = (msg \ "html").as[String]
    html should include("""data-id="""" + StoredMovieRecord.idFor("Belle", Some(2021)))
    html should include("Belle")
    html should include("class=\"reenrich\"") // the row's re-enrich button is present
  }

  it should "push a delete frame with just the id when a row is removed (a merge)" in {
    val repo = new InMemoryMovieRepo(Seq(("Belle", Some(2021), record("Belle"))))
    val collecting = controller(repo).eventSource().takeWithin(1.second).runWith(Sink.seq)

    Thread.sleep(100)
    repo.delete("Belle", Some(2021))

    val frames = Await.result(collecting, 3.seconds)
    frames should have size 1
    val msg = Json.parse(frames.head.stripPrefix("data: ").trim)
    (msg \ "type").as[String] shouldBe "delete"
    (msg \ "id").as[String]   shouldBe StoredMovieRecord.idFor("Belle", Some(2021))
  }

  it should "emit nothing while the collection is idle" in {
    val repo = new InMemoryMovieRepo()
    val frames = Await.result(
      controller(repo).eventSource().takeWithin(500.millis).runWith(Sink.seq), 3.seconds)
    frames shouldBe empty
  }
}
