package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsValue
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import services.identity.staging.{IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedListing, StagedQuestion, StagedTake, StagingBeat}
import tools.MutableClock

import java.time.Instant

/**
 * `/admin/identity/staging` reads what every country's worker published, whichever country's host serves it: the page
 * itself, and its JSON — every country in order, each from its own cursor — behind the admin gate.
 */
class IdentityStagingControllerSpec extends AnyFlatSpec with Matchers {
  private val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  private val staging = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private val controller = new IdentityStagingController(Helpers.stubControllerComponents(), TestAdminAction(), staging, Country.UnitedKingdom, clock)
  private def admin(path: String) = FakeRequest("GET", path).withSession("userId" -> TestAdminAction.AdminUserId)

  private def cluster(key: String, title: String) = StagedCluster(key, Seq(StagedListing("Kino Muza", title)), 1,
    StagedTake(None, None, "NoEvidence", 0.0), 0, onTmdb = false,
    Seq(StagedQuestion("FamilySearch", "imdb", s"imdb|title|$title", StagedQuestion.Failed, 3, Some(clock.millis() + 60000), Some("v3.sg.media-imdb.com"))),
    1, 1, since = clock.millis())
  private def beat(n: Int) = StagingBeat(clock.millis(), n, n, 0, Map("FamilySearch" -> n), 0)

  staging.publish("pl", Seq(cluster("a1", "Diuna")), Nil, beat(1))
  staging.publish("de", Seq(cluster("b2", "Der Pate"), cluster("b3", "Rose")), Nil, beat(2))

  private def data(since: Option[String]): JsValue = contentAsJson(controller.data(since)(admin("/admin/identity/staging/data")))

  "The staging data" should "carry every country in its order, those that published with their clusters and beat" in {
    val countries = (data(None) \ "countries").as[Seq[JsValue]]
    countries.map(c => (c \ "code").as[String]) shouldBe Country.all.map(_.code)
    val pl = countries.head
    (pl \ "name").as[String] shouldBe "Polska"
    (pl \ "changed" \ 0 \ "questions" \ 0 \ "outcome").as[String] shouldBe "failed"
    (pl \ "changed" \ 0 \ "questions" \ 0 \ "host").as[String] shouldBe "v3.sg.media-imdb.com"
    (pl \ "beat" \ "staging").as[Int] shouldBe 1
    val de = countries.find(c => (c \ "code").as[String] == "de").get
    (de \ "keys").as[Seq[String]].sorted shouldBe Seq("b2", "b3")
    val uk = countries.find(c => (c \ "code").as[String] == "uk").get
    (uk \ "changed").as[Seq[JsValue]] shouldBe empty
    (uk \ "beat").toOption.flatMap(_.asOpt[StagingBeat]) shouldBe None
  }

  it should "carry from each country's cursor only what was written since" in {
    val cursors = (data(None) \ "countries").as[Seq[JsValue]].flatMap(c => (c \ "cursor").asOpt[Long].map(m => s"${(c \ "code").as[String]}:$m"))
    clock.advanceSeconds(20)
    staging.publish("de", Seq(cluster("b4", "Kos")), removed = Seq("b2"), beat(2))
    val de = (data(Some(cursors.mkString(","))) \ "countries").as[Seq[JsValue]].find(c => (c \ "code").as[String] == "de").get
    // b4 written since; b3 once more, written AT the cursor; b2 removed
    (de \ "changed").as[Seq[JsValue]].map(c => (c \ "key").as[String]).sorted shouldBe Seq("b3", "b4")
    (de \ "keys").as[Seq[String]].sorted shouldBe Seq("b3", "b4")
  }

  it should "read a malformed cursor as none" in {
    IdentityStagingController.cursors(Some("pl:12,de,uk:x")) shouldBe Map("pl" -> Instant.ofEpochMilli(12))
  }

  "The staging page" should "render its shell, polling the data, with the same page in every other country a click away" in {
    val html = contentAsString(controller.index(admin("/admin/identity/staging")))
    html should include ("Identity staging")
    html should include ("/admin/identity/staging/data")
    html should include (s"""<option value="${Country.UnitedKingdom.webUrl.get}/admin/identity/staging" selected>""")
  }

  it should "refuse anyone not signed in" in {
    status(controller.index(FakeRequest("GET", "/admin/identity/staging"))) shouldBe UNAUTHORIZED
    status(controller.data(None)(FakeRequest("GET", "/admin/identity/staging/data"))) shouldBe UNAUTHORIZED
  }
}
