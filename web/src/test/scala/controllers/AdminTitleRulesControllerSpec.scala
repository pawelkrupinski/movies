package controllers

import models.{MovieRecord, User}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.movies.{InMemoryNormalizationReportRepo, MovieRepo, NormalizationReport, NormalizationRebuilder}
import services.titlerules.{InMemoryTitleRulesRepo, RuleScope, TitleRule, TitleRuleRecord}
import services.users.InMemoryUserRepo

import java.time.Instant

/**
 * Locks the admin editor's contract: session + allowlist auth on every action,
 * rule JSON round-trips, saves/deletes hit the repo, and preview returns a
 * well-formed shape. The merge math itself is covered by RuleMergePreviewSpec.
 */
class AdminTitleRulesControllerSpec extends AnyFlatSpec with Matchers {

  // Disabled repo → empty corpus; the rule-merge preview's findAll() is empty.
  private val emptyRepo = new MovieRepo {
    def enabled = false
    def findAll() = Seq.empty
    def delete(title: String, year: Option[Int]) = ()
    def upsert(title: String, year: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def controller(repo: InMemoryTitleRulesRepo = new InMemoryTitleRulesRepo(),
                         reports: InMemoryNormalizationReportRepo = new InMemoryNormalizationReportRepo(),
                         gate: AdminAction = TestAdminAction()) =
    new AdminTitleRulesController(Helpers.stubControllerComponents(), gate, repo, emptyRepo, reports)

  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)

  private def jsonReq(session: Boolean, body: play.api.libs.json.JsValue) = {
    val base = if (session) adminSession else FakeRequest()
    base.withBody(body).withHeaders("Content-Type" -> "application/json")
  }

  "index" should "401 when not logged in" in {
    status(controller().index().apply(FakeRequest())) shouldBe UNAUTHORIZED
  }

  it should "403 when logged in but the user's email is not on the allowlist" in {
    val users = new InMemoryUserRepo
    users.upsert(User("rando1", "google", "sub-rando", Some("rando@example.com"),
      None, None, Instant.EPOCH, Instant.EPOCH))
    val req = FakeRequest().withSession("userId" -> "rando1")
    status(controller(gate = TestAdminAction(users = users)).index().apply(req)) shouldBe FORBIDDEN
  }

  it should "403 when the session user id can't be resolved" in {
    val req = FakeRequest().withSession("userId" -> "ghost")
    status(controller().index().apply(req)) shouldBe FORBIDDEN
  }

  it should "render the editor for an allowlisted user" in {
    val result = controller().index().apply(adminSession)
    status(result) shouldBe OK
    contentAsString(result) should include ("Title-stripping rules")
  }

  "save" should "persist a per-cinema record under its cinema id, minting rule ids when blank" in {
    val repo = new InMemoryTitleRulesRepo()
    val body = Json.obj("scope" -> "PerCinema", "cinemaId" -> "cinema-city",
      "rules" -> Json.arr(Json.obj("pattern" -> "^Ladies Night - ", "replacement" -> "")))
    val result = controller(repo).save().apply(jsonReq(session = true, body))
    status(result) shouldBe OK
    repo.loadRecords().map(_.id) shouldBe Seq("cinema-city")     // non-composite id = cinema key
    val saved = repo.findAll()
    saved should have size 1
    saved.head.cinemaId shouldBe Some("cinema-city")
    saved.head.id should not be empty
  }

  it should "reject a record carrying an invalid regex" in {
    val body = Json.obj("scope" -> "GlobalStructural",
      "rules" -> Json.arr(Json.obj("pattern" -> "(unclosed")))
    status(controller().save().apply(jsonReq(session = true, body))) shouldBe BAD_REQUEST
  }

  it should "reject a PerCinema record with no cinema" in {
    val body = Json.obj("scope" -> "PerCinema",
      "rules" -> Json.arr(Json.obj("pattern" -> "x")))
    status(controller().save().apply(jsonReq(session = true, body))) shouldBe BAD_REQUEST
  }

  it should "401 a save with no session" in {
    val body = Json.obj("scope" -> "Search", "rules" -> Json.arr(Json.obj("pattern" -> "x")))
    status(controller().save().apply(jsonReq(session = false, body))) shouldBe UNAUTHORIZED
  }

  "delete" should "remove the record by id" in {
    val repo = new InMemoryTitleRulesRepo(TitleRuleRecord.fromRules(Seq(
      TitleRule("r1", RuleScope.Search, None, "x", "", applyAll = false, order = 1))))
    repo.loadRecords().map(_.id) shouldBe Seq("Search")          // global record id = scope name
    val result = controller(repo).delete().apply(jsonReq(session = true, Json.obj("id" -> "Search")))
    status(result) shouldBe OK
    repo.findAll() shouldBe empty
  }

  "preview" should "return a zero-merge result over an empty corpus" in {
    val body = Json.obj("rules" -> Json.arr(
      Json.obj("scope" -> "PerCinema", "cinemaId" -> "cinema-city", "pattern" -> "^X ", "order" -> 1)))
    val result = controller().preview().apply(jsonReq(session = true, body))
    status(result) shouldBe OK
    (contentAsJson(result) \ "newMergeCount").as[Int] shouldBe 0
  }

  "report" should "surface the latest backfill outcome" in {
    val reports = new InMemoryNormalizationReportRepo()
    reports.writeLatest(NormalizationReport.render(
      NormalizationRebuilder.RebuildResult(1,
        Seq(NormalizationRebuilder.MergeEvent("Anora", Some(2024), Seq("Anora", "Ladies Night - Anora"))),
        Seq.empty),
      reEnriched = 2, atEpochMs = 1000L))
    val result = controller(reports = reports).report().apply(adminSession)
    status(result) shouldBe OK
    val js = contentAsJson(result)
    (js \ "reEnriched").as[Int] shouldBe 2
    (js \ "merges").as[Seq[String]].head should include ("Anora")
  }

  it should "401 the report for an anonymous request" in {
    status(controller().report().apply(FakeRequest())) shouldBe UNAUTHORIZED
  }

  "recordFromJson / recordToJson" should "round-trip a record with normal + last rules" in {
    val rec = TitleRuleRecord("cinema-city", RuleScope.PerCinema, Some("cinema-city"),
      rules = Seq(TitleRule("r1", RuleScope.PerCinema, Some("cinema-city"), "^A ", "",
        applyAll = false, order = 0, tag = Some("t"), note = Some("n"))),
      lastRules = Seq(TitleRule("r2", RuleScope.PerCinema, Some("cinema-city"), "B$", "",
        applyAll = true, order = 0, last = true)))
    AdminTitleRulesController.recordFromJson(AdminTitleRulesController.recordToJson(rec)) shouldBe Right(rec)
  }

  "flatRuleFromJson" should "parse the flattened preview shape incl. last + order" in {
    val js = Json.obj("id" -> "x", "scope" -> "Search", "pattern" -> "p$",
      "order" -> 3, "last" -> true)
    AdminTitleRulesController.flatRuleFromJson(js).map(r => (r.order, r.last)) shouldBe Right((3, true))
  }
}
