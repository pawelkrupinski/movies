package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.tasks.{BulkTaskResult, BulkTaskResultStore, InMemoryBulkTaskResultStore, InMemoryTaskQueue, TaskState, TaskType}

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

  private def controller(queue: InMemoryTaskQueue, gate: AdminAction = TestAdminAction(),
                         results: BulkTaskResultStore = new InMemoryBulkTaskResultStore) =
    new TasksController(Helpers.stubControllerComponents(), gate, queue, results)

  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)

  private def dataJson(queue: InMemoryTaskQueue): JsObject = {
    val result = controller(queue).data.apply(adminSession)
    contentType(result) shouldBe Some("application/json")
    Json.parse(contentAsString(result)).as[JsObject]
  }

  "GET /tasks" should "render the monitor page shell" in {
    val result = controller(new InMemoryTaskQueue).index.apply(adminSession)
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

  // ── Last bulk-run results: the outcome that outlives the deleted task doc ─────
  it should "surface each bulk job's last result — counts + when — keyed by taskType" in {
    val store = new InMemoryBulkTaskResultStore
    store.record(BulkTaskResult(TaskType.RefreshAllImdb, t0, succeeded = true,
      "tick done — 4 changed, 0 failed.", walked = Some(578), changed = Some(4), discovered = Some(0), failed = Some(0)))
    store.record(BulkTaskResult(TaskType.SettleNow, t0, succeeded = false, "failed: boom"))

    val result = controller(new InMemoryTaskQueue, results = store).data.apply(adminSession)
    val json   = Json.parse(contentAsString(result)).as[JsObject]

    val imdb = (json \ "lastResults" \ "RefreshAllImdb").as[JsObject]
    (imdb \ "succeeded").as[Boolean] shouldBe true
    (imdb \ "changed").as[Int]       shouldBe 4
    (imdb \ "ranAt").as[Long]        shouldBe t0.toEpochMilli
    (imdb \ "message").as[String]    should include ("4 changed")

    val settle = (json \ "lastResults" \ "SettleNow").as[JsObject]
    (settle \ "succeeded").as[Boolean] shouldBe false
    (settle \ "message").as[String]    shouldBe "failed: boom"
    (settle \ "changed").asOpt[Int]    shouldBe None // a message-only job carries no count
  }

  it should "report an empty lastResults object when no bulk job has ever run" in {
    val json = dataJson(new InMemoryTaskQueue)
    (json \ "lastResults").as[JsObject].keys shouldBe empty
  }

  // ── Auth gate: /tasks is an operational page, closed like /uptime and the
  //    title-rules editor (login session + ADMIN_ALLOWLIST). ──────────────────
  "the /tasks gate" should "401 an anonymous request to the page and the data feed" in {
    status(controller(new InMemoryTaskQueue).index.apply(FakeRequest())) shouldBe UNAUTHORIZED
    status(controller(new InMemoryTaskQueue).data.apply(FakeRequest())) shouldBe UNAUTHORIZED
  }

  it should "403 a logged-in user whose email is not on the allowlist" in {
    val gate = TestAdminAction(allow = Set("someone-else@example.com"))
    status(controller(new InMemoryTaskQueue, gate).data.apply(adminSession)) shouldBe FORBIDDEN
  }

  // ── Run buttons: enqueue a corpus-wide refresh ───────────────────────────────
  private def runJson(queue: InMemoryTaskQueue, job: String): JsObject = {
    val result = controller(queue).run(job).apply(adminSession)
    status(result) shouldBe OK
    Json.parse(contentAsString(result)).as[JsObject]
  }

  "POST /tasks/run/:job" should "enqueue the matching bulk task for each known job" in {
    val cases = Map(
      "tmdb"       -> RefreshAllTmdb,
      "imdb"       -> RefreshAllImdb,
      "filmweb"    -> RefreshAllFilmweb,
      "metacritic" -> RefreshAllMetacritic,
      "rt"         -> RefreshAllRt,
      "settle"     -> SettleNow
    )
    cases.foreach { case (job, expected) =>
      val q    = new InMemoryTaskQueue
      val json = runJson(q, job)
      (json \ "enqueued").as[Boolean]  shouldBe true
      (json \ "taskType").as[String]   shouldBe expected.name
      // The task actually landed in the queue under its constant dedup key.
      val active = q.monitor().active
      active.map(_.taskType) shouldBe Seq(expected.name)
    }
  }

  it should "report duplicate (not stack a second run) when one is already queued" in {
    val q = new InMemoryTaskQueue
    (runJson(q, "imdb") \ "enqueued").as[Boolean]  shouldBe true
    val second = runJson(q, "imdb")
    (second \ "enqueued").as[Boolean]  shouldBe false
    (second \ "duplicate").as[Boolean] shouldBe true
    q.monitor().active.size shouldBe 1
  }

  it should "400 an unknown job slug without enqueuing anything" in {
    val q      = new InMemoryTaskQueue
    val result = controller(q).run("bogus").apply(adminSession)
    status(result) shouldBe BAD_REQUEST
    q.monitor().active shouldBe empty
  }

  it should "401 an anonymous run request" in {
    status(controller(new InMemoryTaskQueue).run("tmdb").apply(FakeRequest())) shouldBe UNAUTHORIZED
  }
}
