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

  // The top-of-page "pending work" sections derive their membership client-side
  // from per-row data-* flags the change stream keeps live, so the server only
  // emits the flags + the (initially empty) section scaffolding. Assert the
  // flags resolve correctly per row: a detail-pending row is `unenriched`; a row
  // with no TMDB id that isn't a concluded no-match is `tmdb-unresolved`; a
  // no-match row is neither (it's done, not pending).
  private val flagRecords = Seq(
    ("Pending",    Some(2024), MovieRecord(detailPending = true, tmdbId = Some(1),
                                 data = Map(CinemaCityWroclavia -> SourceData(title = Some("Pending"))))),
    ("Unresolved", Some(2023), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Unresolved"))))),
    ("NoMatch",    Some(2022), MovieRecord(tmdbNoMatch = true,
                                 data = Map(CinemaCityWroclavia -> SourceData(title = Some("NoMatch"))))),
  )

  /** The value of `attr` on the `<tr class="data">` whose `data-title` is `title`.
   *  Scoped to that one opening tag (`.*?>` stops at the first `>`). */
  private def rowAttr(html: String, title: String, attr: String): Option[String] = {
    val row = ("(?s)data-title=\"" + java.util.regex.Pattern.quote(title) + "\".*?>").r.findFirstIn(html)
    row.flatMap((attr + "=\"([^\"]*)\"").r.findFirstMatchIn(_).map(_.group(1)))
  }

  "GET /debug" should "carry per-row pending flags and the two section shells" in {
    val html = contentAsString(
      TestMovieController.build(flagRecords, Mode.Dev)._1.debug().apply(FakeRequest(GET, "/debug")))

    // Both section shells present (JS fills the lists).
    html should include ("""data-flag="unenriched"""")
    html should include ("""data-flag="tmdbUnresolved"""")

    // detailPending → unenriched; the others have run their detail step.
    rowAttr(html, "Pending",    "data-unenriched") shouldBe Some("true")
    rowAttr(html, "Unresolved", "data-unenriched") shouldBe Some("false")

    // No id and not a no-match → tmdb-unresolved; a resolved id or a concluded
    // no-match → not.
    rowAttr(html, "Unresolved", "data-tmdb-unresolved") shouldBe Some("true")
    rowAttr(html, "Pending",    "data-tmdb-unresolved") shouldBe Some("false")
    rowAttr(html, "NoMatch",    "data-tmdb-unresolved") shouldBe Some("false")

    // The identity year the queue dedup keys match on rides along.
    rowAttr(html, "Pending", "data-queue-year") shouldBe Some("2024")
  }

  // ── /debug/queue snapshot the pending sections poll for queue places ─────────
  "GET /debug/queue" should "return the active tasks oldest-first in dev" in {
    val q  = new InMemoryTaskQueue
    val t0 = java.time.Instant.parse("2026-06-13T10:00:00Z")
    q.enqueue(TaskType.EnrichDetails, "detail|cc|Belle|2021", submittedAt = t0)
    q.enqueue(TaskType.ResolveTmdb, EnrichTaskKeys.resolveTmdbDedup("Incepcja", Some(2010)),
      submittedAt = t0.plusSeconds(1))
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1

    val result = ctrl.debugQueue().apply(FakeRequest(GET, "/debug/queue"))
    status(result) shouldBe OK
    val active = (Json.parse(contentAsString(result)) \ "active").as[Seq[play.api.libs.json.JsValue]]
    active.map(j => (j \ "taskType").as[String]) shouldBe Seq("EnrichDetails", "ResolveTmdb")
    active.map(j => (j \ "dedupKey").as[String]) shouldBe
      Seq("detail|cc|Belle|2021", EnrichTaskKeys.resolveTmdbDedup("Incepcja", Some(2010)))
    active.foreach(j => (j \ "state").as[String] shouldBe "waiting")
  }

  it should "404 in production like the rest of /debug" in {
    val ctrl = TestMovieController.build(records, Mode.Prod, taskQueue = new InMemoryTaskQueue)._1
    status(ctrl.debugQueue().apply(FakeRequest(GET, "/debug/queue"))) shouldBe NOT_FOUND
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
