package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.identity.ConfidenceCalibration.Sample
import services.identity.{Decision, InMemoryPinStore, PinClaim, Pins, ShadowDecisions}
import services.movies.ListingKey

import java.time.{Clock, Instant, ZoneOffset}

/**
 * `/admin/identity`: a read-only diagnostic of the resolver's contradicted and low-confidence
 * decisions, and pin create/remove for emergencies. Driven through the real [[Pins]] rules over
 * an in-memory store, and a shadow source standing in for the resolver's output.
 */
class IdentityAdminControllerSpec extends AnyFlatSpec with Matchers {
  import IdentityAdminControllerSpec._

  private def fixture(shadow: ShadowDecisions = Shadow) = {
    val pins = new Pins(new InMemoryPinStore, Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC))
    (new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(), TestAdminAction.adminRepository, pins, shadow), pins)
  }

  private val admin = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)
  private def json(body: JsObject, session: Boolean = true) =
    (if (session) admin else FakeRequest()).withBody(body).withHeaders("Content-Type" -> "application/json")

  "the identity page" should "list contradicted and low-confidence decisions with their explanations, and the calibration" in {
    val (c, _) = fixture()
    val page = contentAsString(c.index.apply(admin))
    page should include ("Contradicted")
    page should include ("different bracketed years hold it apart")      // contradicted's pressure
    page should include ("bare title, no year")                          // low-confidence's explanation
    page should not include ("Uncontested Film")                         // confident and uncontested: not listed
    page should include ("threshold 0.5")
  }

  it should "say the gate withholds nothing when there is no labelled shadow data" in {
    val (c, _) = fixture(ShadowDecisions.none)
    contentAsString(c.index.apply(admin)) should include ("no labelled shadow data")
  }

  it should "refuse an anonymous caller and a non-admin" in {
    val (c, _) = fixture()
    status(c.index.apply(FakeRequest())) shouldBe UNAUTHORIZED
    val member = new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(allow = Set.empty),
      TestAdminAction.adminRepository, fixture()._2, Shadow)
    status(member.index.apply(admin)) shouldBe FORBIDDEN
  }

  "creating a pin" should "store it with the signed-in admin as its author, and show it on the page" in {
    val (c, pins) = fixture()
    val r = c.createPin.apply(json(Json.obj("kind" -> "is-film", "tmdbId" -> 21484, "reason" -> "the 4K strand",
      "listings" -> Json.arr(Json.obj("venue" -> "Kino 1410", "rawTitle" -> "Opętanie | klasyka w 4k")))))
    status(r) shouldBe OK
    pins.all().map(p => (p.claim, p.author, p.listings)) shouldBe
      Seq((PinClaim.IsFilm(21484), TestAdminAction.AdminEmail, Seq(ListingKey.Published("Kino 1410", "Opętanie | klasyka w 4k", None, Nil))))
    contentAsString(c.index.apply(admin)) should include ("the 4K strand")
  }

  it should "answer 400 with the pin rules' refusal, and store nothing" in {
    val (c, pins) = fixture()
    val r = c.createPin.apply(json(Json.obj("kind" -> "same-film", "reason" -> "x",
      "listings" -> Json.arr(Json.obj("venue" -> "A", "page" -> "https://a/1", "rawTitle" -> "One")))))
    status(r) shouldBe BAD_REQUEST
    (contentAsJson(r) \ "error").as[String] should include ("two or more")
    status(c.createPin.apply(json(Json.obj("kind" -> "nonsense", "reason" -> "x", "listings" -> Json.arr())))) shouldBe BAD_REQUEST
    pins.all() shouldBe empty
  }

  "removing a pin" should "drop it, and 404 an id that is not there" in {
    val (c, pins) = fixture()
    val pin = pins.add(Seq(ListingKey.Published("A", "One", Some(2020), Seq("X"))), PinClaim.NeverFilm(7), "a", "r").toOption.get
    status(c.removePin.apply(json(Json.obj("id" -> pin.id)))) shouldBe OK
    pins.all() shouldBe empty
    status(c.removePin.apply(json(Json.obj("id" -> pin.id)))) shouldBe NOT_FOUND
  }

  "the listing JSON" should "round-trip both key shapes" in {
    Seq[ListingKey](ListingKey.Native("V", "https://v/p", "Raw"), ListingKey.Published("V", "Raw", Some(1999), Seq("A", "B")),
      ListingKey.Published("V", "Raw", None, Nil)).foreach { k =>
      IdentityAdminController.listingJson(k).as[ListingKey](using IdentityAdminController.listingReads) shouldBe k
    }
  }
}

object IdentityAdminControllerSpec {
  final case class D(listings: Set[ListingKey], tmdbId: Option[Int], confidence: Double, explanation: Seq[String],
                     contradictions: Seq[String]) extends Decision

  val Shadow: ShadowDecisions = new ShadowDecisions {
    def latest(): Seq[Decision] = Seq(
      D(Set(ListingKey.Published("Vue Leeds", "Mockingjay – Part 2 (2026)", Some(2026), Nil)), None, 0.9,
        Seq("no must-link joins it to the 2015 film"), Seq("different bracketed years hold it apart")),
      D(Set(ListingKey.Published("Kino Amok", "Samson i Dalila", None, Nil)), Some(22683), 0.2,
        Seq("bare title, no year"), Nil),
      D(Set(ListingKey.Published("Kino X", "Uncontested Film", Some(2024), Seq("Y"))), Some(1), 0.95, Seq("same TMDB id"), Nil))
    def verdicts(): Seq[Sample] = Seq(Sample(0.1, correct = false), Sample(0.5, correct = true), Sample(0.9, correct = true))
  }
}
