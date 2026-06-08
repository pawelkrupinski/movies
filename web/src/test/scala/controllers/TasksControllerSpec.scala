package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.tasks.{InMemoryTaskQueue, TaskState, TaskType}

import java.time.Instant
import scala.concurrent.duration._

/**
 * The /tasks monitor page serves a static shell and exposes the live queue via
 * the `/tasks/data` JSON endpoint. These lock that the JSON carries the actual
 * task *contents* (not just counts) the page renders — type, dedup key, state,
 * timestamps, attempts, worker, lease, error — driven through the real
 * `InMemoryTaskQueue` so the controller↔queue contract is exercised end to end.
 */
class TasksControllerSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  private def controller(queue: InMemoryTaskQueue) =
    new TasksController(Helpers.stubControllerComponents(), queue)

  private def dataJson(queue: InMemoryTaskQueue): JsObject = {
    val result = controller(queue).data.apply(FakeRequest())
    contentType(result) shouldBe Some("application/json")
    Json.parse(contentAsString(result)).as[JsObject]
  }

  "GET /tasks" should "render the monitor page shell" in {
    val result = controller(new InMemoryTaskQueue).index.apply(FakeRequest())
    status(result) shouldBe OK
    contentAsString(result) should include ("Task queue")
    contentAsString(result) should include ("/tasks/data") // the page polls this
  }

  "GET /tasks/data" should "return per-state counts and the live active task contents" in {
    val q = new InMemoryTaskQueue
    q.enqueue(ScrapeCinema, "scrape|kino-x", submittedAt = t0)
    q.enqueue(ImdbRating, "imdb|film-y", Map("docId" -> "film-y"), submittedAt = t0.plusSeconds(10))
    val claimed = q.claim("worker-1", 1.minute, t0).get // scrape|kino-x → worked_on

    val json = dataJson(q)

    (json \ "counts" \ TaskState.Waiting).as[Long] shouldBe 1L
    (json \ "counts" \ TaskState.WorkedOn).as[Long] shouldBe 1L

    val active = (json \ "active").as[JsArray].value
    active.map(t => (t \ "dedupKey").as[String]) shouldBe Seq("scrape|kino-x", "imdb|film-y")

    val worked = active.head
    (worked \ "taskType").as[String] shouldBe "ScrapeCinema"
    (worked \ "state").as[String] shouldBe TaskState.WorkedOn
    (worked \ "workerId").as[String] shouldBe "worker-1"
    (worked \ "attempts").as[Int] shouldBe 1
    (worked \ "id").as[String] shouldBe claimed.id
    (worked \ "submittedAt").as[Long] shouldBe t0.toEpochMilli
    (worked \ "leaseExpiresAt").as[Long] shouldBe t0.plusSeconds(60).toEpochMilli

    val waiting = active(1)
    (waiting \ "state").as[String] shouldBe TaskState.Waiting
    (waiting \ "workerId").asOpt[String] shouldBe None
    (waiting \ "leaseExpiresAt").asOpt[Long] shouldBe None
  }

  it should "carry a server timestamp so the page can tick task ages locally" in {
    val before = System.currentTimeMillis()
    val json = dataJson(new InMemoryTaskQueue)
    val now = (json \ "now").as[Long]
    now should be >= before
    now should be <= System.currentTimeMillis()
  }

  it should "report an empty queue as zero active tasks" in {
    val json = dataJson(new InMemoryTaskQueue)
    (json \ "active").as[JsArray].value shouldBe empty
    (json \ "shown").as[Int] shouldBe 0
  }
}
