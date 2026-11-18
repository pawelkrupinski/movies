package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers._
import services.tasks.InMemoryTaskQueue
import services.tasktypes.TaskType

import java.time.Instant

/** `POST /tasks/run/:job`: the `/tasks` page's run buttons, enqueueing a corpus-wide run through the real
 *  `InMemoryTaskQueue`. */
class TaskRunControllerSpec extends AnyFlatSpec with Matchers {
  import TaskType._

  private def controller(queue: InMemoryTaskQueue, gate: AdminAction = TestAdminAction()) =
    new TaskRunController(Helpers.stubControllerComponents(), gate, queue,
      java.time.Clock.fixed(Instant.parse("2026-06-07T12:01:00Z"), java.time.ZoneOffset.UTC))

  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)

  // ── Run buttons: enqueue a corpus-wide refresh ───────────────────────────────
  private def runJson(queue: InMemoryTaskQueue, job: String): JsObject = {
    val result = controller(queue).run(job).apply(adminSession)
    status(result) shouldBe OK
    Json.parse(contentAsString(result)).as[JsObject]
  }

  "POST /tasks/run/:job" should "enqueue the matching bulk task for each known job" in {
    val cases = Map(
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

  // The TMDB re-enrich ran the old pipeline's resolve; with it gone a queued run had no handler.
  it should "400 the retired tmdb job rather than queue a run nothing handles" in {
    val q = new InMemoryTaskQueue
    status(controller(q).run("tmdb").apply(adminSession)) shouldBe BAD_REQUEST
    q.monitor().active shouldBe empty
  }

  it should "401 an anonymous run request" in {
    status(controller(new InMemoryTaskQueue).run("imdb").apply(FakeRequest())) shouldBe UNAUTHORIZED
  }
}
