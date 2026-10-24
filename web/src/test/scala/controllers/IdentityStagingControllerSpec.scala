package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.JsValue
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import services.identity.staging.{IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedListing, StagedQuestion, StagedTake, StagingBeat}
import tools.MutableClock

import java.time.Instant

/**
 * `/debug/identity/staging` reads what every country's worker published to its own database, each country's store read
 * beside the others: the page itself, and its JSON — every country in order, each from its own cursor — dev-only.
 */
class IdentityStagingControllerSpec extends AnyFlatSpec with Matchers {
  private val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  // one store a country, as each worker writes its own database
  private val pl = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private val de = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private val uk = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private def controllerIn(mode: Mode) =
    new IdentityStagingController(Helpers.stubControllerComponents(), mode, Seq(pl, de, uk), Country.UnitedKingdom, clock)
  private val controller = controllerIn(Mode.Dev)
  private def get(path: String) = FakeRequest("GET", path)

  private def cluster(key: String, title: String) = StagedCluster(key, Seq(StagedListing("Kino Muza", title)), 1,
    StagedTake(None, None, "NoEvidence", 0.0), 0, onTmdb = false,
    Seq(StagedQuestion("FamilySearch", "imdb", s"imdb|title|$title", StagedQuestion.Failed, 3, Some(clock.millis() + 60000), Some("v3.sg.media-imdb.com"))),
    1, 1, since = clock.millis())
  private def beat(n: Int) = StagingBeat(clock.millis(), n, n, 0, Map("FamilySearch" -> n), 0)

  pl.publish("pl", Seq(cluster("a1", "Diuna")), Nil, beat(1))
  de.publish("de", Seq(cluster("b2", "Der Pate"), cluster("b3", "Rose")), Nil, beat(2))

  private def data(since: Option[String]): JsValue = contentAsJson(controller.data(since)(get("/debug/identity/staging/data")))

  "The staging data" should "carry every country in its order, those that published with their clusters and beat" in {
    val countries = (data(None) \ "countries").as[Seq[JsValue]]
    countries.map(c => (c \ "code").as[String]) shouldBe Country.all.map(_.code)
    val plNow = countries.head
    (plNow \ "name").as[String] shouldBe "Polska"
    (plNow \ "changed" \ 0 \ "questions" \ 0 \ "outcome").as[String] shouldBe "failed"
    (plNow \ "changed" \ 0 \ "questions" \ 0 \ "host").as[String] shouldBe "v3.sg.media-imdb.com"
    (plNow \ "beat" \ "staging").as[Int] shouldBe 1
    val deNow = countries.find(c => (c \ "code").as[String] == "de").get
    (deNow \ "keys").as[Seq[String]].sorted shouldBe Seq("b2", "b3")
    val ukNow = countries.find(c => (c \ "code").as[String] == "uk").get
    (ukNow \ "changed").as[Seq[JsValue]] shouldBe empty
    (ukNow \ "beat").toOption.flatMap(_.asOpt[StagingBeat]) shouldBe None
  }

  it should "carry from each country's cursor only what was written since" in {
    val cursors = (data(None) \ "countries").as[Seq[JsValue]].flatMap(c => (c \ "cursor").asOpt[Long].map(m => s"${(c \ "code").as[String]}:$m"))
    clock.advanceSeconds(20)
    de.publish("de", Seq(cluster("b4", "Kos")), removed = Seq("b2"), beat(2))
    val deNow = (data(Some(cursors.mkString(","))) \ "countries").as[Seq[JsValue]].find(c => (c \ "code").as[String] == "de").get
    // b4 written since; b3 once more, written AT the cursor; b2 removed
    (deNow \ "changed").as[Seq[JsValue]].map(c => (c \ "key").as[String]).sorted shouldBe Seq("b3", "b4")
    (deNow \ "keys").as[Seq[String]].sorted shouldBe Seq("b3", "b4")
  }

  it should "carry each take's explanation for the page to show why it was decided" in {
    uk.publish("uk", Seq(cluster("c1", "Groundswell").copy(take = StagedTake(Some(7), Some("Groundswell"), "OwnMatch", 0.96,
      explanation = Seq("best tmdb 7 at 96.0%")))), Nil, beat(1))
    val ukNow = (data(None) \ "countries").as[Seq[JsValue]].find(c => (c \ "code").as[String] == "uk").get
    (ukNow \ "changed" \ 0 \ "take" \ "explanation").as[Seq[String]] shouldBe Seq("best tmdb 7 at 96.0%")
  }

  it should "read a malformed cursor as none" in {
    IdentityStagingController.cursors(Some("pl:12,de,uk:x")) shouldBe Map("pl" -> Instant.ofEpochMilli(12))
  }

  "The staging page" should "render its shell, polling the data" in {
    val html = contentAsString(controller.index(get("/debug/identity/staging")))
    html should include ("Identity staging")
    html should include ("/debug/identity/staging/data")
  }

  it should "not exist in production, like the rest of /debug" in {
    val prod = controllerIn(Mode.Prod)
    status(prod.index(get("/debug/identity/staging"))) shouldBe NOT_FOUND
    status(prod.data(None)(get("/debug/identity/staging/data"))) shouldBe NOT_FOUND
  }
}
