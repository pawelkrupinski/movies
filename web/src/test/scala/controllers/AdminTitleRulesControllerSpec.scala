package controllers

import models.{MovieRecord, User}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.movies.{InMemoryNormalizationReportRepository, MovieRepository, NormalizationReport, NormalizationRebuilder, StoredMovieRecord}
import services.titlerules.{InMemoryTitleRulesRepository, RuleScope, TitleRule, TitleRuleRecord}
import services.users.InMemoryUserRepository

import java.time.Instant

/**
 * Locks the admin editor's contract: session + allowlist auth on every action,
 * rule JSON round-trips, saves/deletes hit the repository, and preview returns a
 * well-formed shape. The merge math itself is covered by RuleMergePreviewSpec.
 */
class AdminTitleRulesControllerSpec extends AnyFlatSpec with Matchers {

  // Disabled repository → empty corpus; the rule-merge preview's findAll() is empty.
  private val emptyRepository = new MovieRepository {
    def enabled = false
    def findAll() = Seq.empty
    def delete(title: String, year: Option[Int]) = ()
    def upsert(title: String, year: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  /** A read-only corpus of the given display titles — only `findAll().title` is
   *  read by the affected/preview endpoints. */
  private def repositoryWith(titles: String*): MovieRepository = new MovieRepository {
    def enabled = true
    def findAll() = titles.map(t => StoredMovieRecord(t, None, MovieRecord()))
    def delete(title: String, year: Option[Int]) = ()
    def upsert(title: String, year: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  private def controller(repository: InMemoryTitleRulesRepository = new InMemoryTitleRulesRepository(),
                         reports: InMemoryNormalizationReportRepository = new InMemoryNormalizationReportRepository(),
                         gate: AdminAction = TestAdminAction(),
                         movies: MovieRepository = emptyRepository) =
    new AdminTitleRulesController(Helpers.stubControllerComponents(), gate, repository, movies, reports)

  private val adminSession = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)

  private def jsonRequest(session: Boolean, body: play.api.libs.json.JsValue) = {
    val base = if (session) adminSession else FakeRequest()
    base.withBody(body).withHeaders("Content-Type" -> "application/json")
  }

  "index" should "401 when not logged in" in {
    status(controller().index().apply(FakeRequest())) shouldBe UNAUTHORIZED
  }

  it should "403 when logged in but the user's email is not on the allowlist" in {
    val users = new InMemoryUserRepository
    users.upsert(User("rando1", "google", "sub-rando", Some("rando@example.com"),
      None, None, Instant.EPOCH, Instant.EPOCH))
    val request = FakeRequest().withSession("userId" -> "rando1")
    status(controller(gate = TestAdminAction(users = users)).index().apply(request)) shouldBe FORBIDDEN
  }

  it should "403 when the session user id can't be resolved" in {
    val request = FakeRequest().withSession("userId" -> "ghost")
    status(controller().index().apply(request)) shouldBe FORBIDDEN
  }

  it should "render the editor for an allowlisted user" in {
    val result = controller().index().apply(adminSession)
    status(result) shouldBe OK
    contentAsString(result) should include ("Title-stripping rules")
  }

  "save" should "persist a per-cinema record under its cinema id, minting rule ids when blank" in {
    val repository = new InMemoryTitleRulesRepository()
    val body = Json.obj("scope" -> "PerCinema", "cinemaId" -> "cinema-city",
      "rules" -> Json.arr(Json.obj("pattern" -> "^Ladies Night - ", "replacement" -> "")))
    val result = controller(repository).save().apply(jsonRequest(session = true, body))
    status(result) shouldBe OK
    repository.loadRecords().map(_.id) shouldBe Seq("cinema-city")     // non-composite id = cinema key
    val saved = repository.findAll()
    saved should have size 1
    saved.head.cinemaId shouldBe Some("cinema-city")
    saved.head.id should not be empty
  }

  it should "reject a record carrying an invalid regex" in {
    val body = Json.obj("scope" -> "GlobalStructural",
      "rules" -> Json.arr(Json.obj("pattern" -> "(unclosed")))
    status(controller().save().apply(jsonRequest(session = true, body))) shouldBe BAD_REQUEST
  }

  it should "reject a PerCinema record with no cinema" in {
    val body = Json.obj("scope" -> "PerCinema",
      "rules" -> Json.arr(Json.obj("pattern" -> "x")))
    status(controller().save().apply(jsonRequest(session = true, body))) shouldBe BAD_REQUEST
  }

  it should "401 a save with no session" in {
    val body = Json.obj("scope" -> "GlobalStructural", "rules" -> Json.arr(Json.obj("pattern" -> "x")))
    status(controller().save().apply(jsonRequest(session = false, body))) shouldBe UNAUTHORIZED
  }

  it should "accept the legacy \"Search\" scope name and store it under GlobalStructural (alias)" in {
    val repository = new InMemoryTitleRulesRepository()
    val body = Json.obj("scope" -> "Search",
      "rules" -> Json.arr(Json.obj("pattern" -> "^Klub: ", "replacement" -> "")))
    status(controller(repository).save().apply(jsonRequest(session = true, body))).shouldBe(OK)
    // byName("Search") → GlobalStructural, so the record's id is the live scope name.
    repository.loadRecords().map(_.id).shouldBe(Seq("GlobalStructural"))
    repository.loadRecords().head.scope.shouldBe(RuleScope.GlobalStructural)
  }

  "delete" should "remove the record by id" in {
    val repository = new InMemoryTitleRulesRepository(TitleRuleRecord.fromRules(Seq(
      TitleRule("r1", RuleScope.GlobalStructural, None, "x", "", applyAll = false, order = 1))))
    repository.loadRecords().map(_.id) shouldBe Seq("GlobalStructural")          // global record id = scope name
    val result = controller(repository).delete().apply(jsonRequest(session = true, Json.obj("id" -> "GlobalStructural")))
    status(result) shouldBe OK
    repository.findAll() shouldBe empty
  }

  "preview" should "return a zero-merge result over an empty corpus" in {
    val body = Json.obj("rules" -> Json.arr(
      Json.obj("scope" -> "PerCinema", "cinemaId" -> "cinema-city", "pattern" -> "^X ", "order" -> 1)))
    val result = controller().preview().apply(jsonRequest(session = true, body))
    status(result) shouldBe OK
    (contentAsJson(result) \ "newMergeCount").as[Int] shouldBe 0
  }

  "affected" should "401 an anonymous request" in {
    val body = Json.obj("rules" -> Json.arr())
    status(controller().affected().apply(jsonRequest(session = false, body))) shouldBe UNAUTHORIZED
  }

  it should "report, per transient rule, which corpus titles it rewrites and to what" in {
    val corpus = repositoryWith("Top Gun - Restored", "Klub: Vertigo", "Anora")
    val body = Json.obj("rules" -> Json.arr(
      Json.obj("id" -> "g1", "scope" -> "GlobalStructural",
        "pattern" -> "(?i)\\s*-\\s*restored$", "replacement" -> "", "order" -> 1),
      // "Search" is the legacy alias of GlobalStructural — still accepted as a transient rule.
      Json.obj("id" -> "s1", "scope" -> "Search",
        "pattern" -> "(?i)^Klub:\\s*", "replacement" -> "", "order" -> 2),
      // A record-changing rule: present in the draft but NOT in the affected output.
      Json.obj("id" -> "p1", "scope" -> "PerCinema", "cinemaId" -> "cinema-city",
        "pattern" -> "^X ", "replacement" -> "", "order" -> 1)))
    val result = controller(movies = corpus).affected().apply(jsonRequest(session = true, body))
    status(result) shouldBe OK
    val array = (contentAsJson(result) \ "affected").as[Seq[play.api.libs.json.JsValue]]
    array.map(a => (a \ "ruleId").as[String]) should contain theSameElementsAs Seq("g1", "s1")
    val g1 = array.find(a => (a \ "ruleId").as[String] == "g1").get
    (g1 \ "count").as[Int] shouldBe 1
    (g1 \ "changes" \ 0 \ "title").as[String] shouldBe "Top Gun - Restored"
    (g1 \ "changes" \ 0 \ "result").as[String] shouldBe "Top Gun"
    val s1 = array.find(a => (a \ "ruleId").as[String] == "s1").get
    (s1 \ "changes" \ 0 \ "result").as[String] shouldBe "Vertigo"
  }

  it should "400 a malformed rule body" in {
    val body = Json.obj("rules" -> Json.arr(Json.obj("scope" -> "GlobalStructural"))) // no pattern
    status(controller().affected().apply(jsonRequest(session = true, body))) shouldBe BAD_REQUEST
  }

  "report" should "surface the latest backfill outcome" in {
    val reports = new InMemoryNormalizationReportRepository()
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
    val record = TitleRuleRecord("cinema-city", RuleScope.PerCinema, Some("cinema-city"),
      rules = Seq(TitleRule("r1", RuleScope.PerCinema, Some("cinema-city"), "^A ", "",
        applyAll = false, order = 0, tag = Some("t"), note = Some("n"))),
      lastRules = Seq(TitleRule("r2", RuleScope.PerCinema, Some("cinema-city"), "B$", "",
        applyAll = true, order = 0, last = true)))
    AdminTitleRulesController.recordFromJson(AdminTitleRulesController.recordToJson(record)) shouldBe Right(record)
  }

  "flatRuleFromJson" should "parse the flattened preview shape incl. last + order" in {
    val js = Json.obj("id" -> "x", "scope" -> "GlobalStructural", "pattern" -> "p$",
      "order" -> 3, "last" -> true)
    AdminTitleRulesController.flatRuleFromJson(js).map(r => (r.order, r.last)) shouldBe Right((3, true))
  }

  it should "resolve the legacy \"Search\" scope name to GlobalStructural (alias)" in {
    val js = Json.obj("id" -> "x", "scope" -> "Search", "pattern" -> "p$", "order" -> 3)
    AdminTitleRulesController.flatRuleFromJson(js).map(_.scope) shouldBe Right(RuleScope.GlobalStructural)
  }
}
