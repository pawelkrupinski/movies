package controllers

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.tasks.{EnrichTaskKeys, InMemoryTaskQueue, TaskType}

/**
 * `/debug` is the dev-only global-corpus table. It used to be city-scoped
 * (`/:city/debug`) even though the data is the whole corpus regardless of city,
 * so it's now a top-level `GET /debug` taking no city param. It renders (200) in
 * Dev/Test and 404s in Prod via the same `devOnly` gate as `/debug/tune`.
 */
class MovieControllerDebugSpec extends AnyFlatSpec with Matchers {

  private val records = Seq(
    ("Belle", Some(2021), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))),
    ("Incepcja", Some(2010), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Incepcja"))))),
  )

  private def buildController(mode: Mode): MovieController =
    TestMovieController.build(records, mode)._1

  "GET /debug" should "render the whole corpus in dev mode" in {
    val result = buildController(Mode.Dev).debug().apply(FakeRequest(GET, "/debug"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    // Both films listed regardless of any city — the table is the global corpus.
    html should include("Belle")
    html should include("Incepcja")
    // Deep-links into a city the film actually plays in (Wrocław), never a
    // city slug from the URL (there isn't one anymore).
    html should include("""href="/wroclaw/film?title=Belle"""")
  }

  it should "404 in production" in {
    val result = buildController(Mode.Prod).debug().apply(FakeRequest(GET, "/debug"))
    status(result) shouldBe NOT_FOUND
  }

  "GET /debug/readmodel" should "dump the warm read cache in dev mode" in {
    val result = buildController(Mode.Dev).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("Read cache")
    // Reflects what the web serves from — the read cache's resolved movies.
    html should include("Belle")
    html should include("Incepcja")
  }

  it should "404 in production like the rest of /debug" in {
    val result = buildController(Mode.Prod).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))
    status(result) shouldBe NOT_FOUND
  }

  // Unlike the other /debug pages, rehydrate runs in every mode (it reconciles a
  // live prod instance's caches) and mutates state — so it's gated by the admin
  // allowlist instead of left open, even in prod.
  private val rehydrateReq = FakeRequest(POST, "/wroclaw/debug/rehydrate")

  "POST /…/debug/rehydrate" should "401 an anonymous request" in {
    val ctrl = TestMovieController.build(records, Mode.Prod)._1
    status(ctrl.rehydrate("wroclaw").apply(rehydrateReq)) shouldBe UNAUTHORIZED
  }

  it should "403 a logged-in user not on the allowlist" in {
    val ctrl = TestMovieController.build(records, Mode.Prod,
      adminAction = TestAdminAction(allow = Set("someone-else@example.com")))._1
    status(ctrl.rehydrate("wroclaw").apply(
      rehydrateReq.withSession("userId" -> TestAdminAction.AdminUserId))) shouldBe FORBIDDEN
  }

  it should "reload the caches for an allowlisted admin" in {
    val ctrl = TestMovieController.build(records, Mode.Prod)._1
    val result = ctrl.rehydrate("wroclaw").apply(
      rehydrateReq.withSession("userId" -> TestAdminAction.AdminUserId))
    status(result) shouldBe OK
    contentAsString(result) should include("rehydrated")
  }

  // ── Per-row re-enrich button (dev-only) ──────────────────────────────────────
  private val reenrichReq = FakeRequest(POST, "/debug/reenrich")

  "POST /debug/reenrich" should "enqueue a ResolveTmdb task with the row's title + year in dev" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1

    val result = ctrl.reenrich("Belle", Some(2021)).apply(reenrichReq)
    status(result) shouldBe OK
    (Json.parse(contentAsString(result)) \ "enqueued").as[Boolean] shouldBe true

    val active = q.monitor().active
    active.map(_.taskType) shouldBe Seq(TaskType.ResolveTmdb.name)
    active.head.dedupKey shouldBe EnrichTaskKeys.resolveTmdbDedup("Belle", Some(2021))
  }

  it should "report duplicate on a second enqueue for the same film" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1
    ctrl.reenrich("Belle", Some(2021)).apply(reenrichReq)
    val second = ctrl.reenrich("Belle", Some(2021)).apply(reenrichReq)
    (Json.parse(contentAsString(second)) \ "duplicate").as[Boolean] shouldBe true
    q.monitor().active.size shouldBe 1
  }

  it should "400 a request with no title (and enqueue nothing)" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1
    status(ctrl.reenrich("", None).apply(reenrichReq)) shouldBe BAD_REQUEST
    q.monitor().active shouldBe empty
  }

  it should "404 in production (no enqueue) like the rest of /debug" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Prod, taskQueue = q)._1
    status(ctrl.reenrich("Belle", Some(2021)).apply(reenrichReq)) shouldBe NOT_FOUND
    q.monitor().active shouldBe empty
  }
}
