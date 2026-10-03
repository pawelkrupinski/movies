package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.identity.{InMemoryPinStore, PinClaim, Pins}
import services.movies.ListingKey

import java.time.{Clock, Instant, ZoneOffset}

/**
 * `/admin/identity`: pin create/remove for emergencies, driven through the real [[Pins]] rules over
 * an in-memory store, and the trace page.
 */
class IdentityAdminControllerSpec extends AnyFlatSpec with Matchers {
  import IdentityAdminControllerSpec._

  private def fixture() = {
    val pins = new Pins(new InMemoryPinStore, Now)
    (new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(), TestAdminAction.adminRepository, pins), pins)
  }

  private val admin = FakeRequest().withSession("userId" -> TestAdminAction.AdminUserId)
  private def json(body: JsObject, session: Boolean = true) =
    (if (session) admin else FakeRequest()).withBody(body).withHeaders("Content-Type" -> "application/json")

  "the trace page" should "show a rule's listings with their evidence, a film's, a title's — and every rule's count when asked nothing" in {
    import services.identity.{InMemoryIdentityTraceStore, ListingTrace}
    val store = new InMemoryIdentityTraceStore
    val brides = ListingKey.Published("Metrograph", "Brides of Dracula", None, Seq("Terence Fisher"))
    val camino = ListingKey.Published("Helios Bełchatów", "Camino dla opornych - KNT", None, Nil)
    val quill  = ListingKey.Published("Goli Theater Goch", "Ein Hund namens Quill", Some(2004), Seq("Yōichi Sai"))
    val quillRefused = services.identity.DecisionTrace.Refusal("favoured-calibrated", "below the rating cut", Some(49258), "26.6% < 40.0%")
    store.replace(Set.empty, () => Seq(
      ListingTrace(brides, "f1", Some(23220), "OwnMatch", Seq("accept:favoured-calibrated", "join:same-film"), None,
        Seq("director=same_person +4.22", "title=exact +1.50"), Some(23220)),
      ListingTrace(camino, "f2", None, "Vetoed", Seq("veto:learned-listing-film-probability-below-the-cannot-link-cut"), Some("'Camino dla opornych - KNT'"),
        Seq("originalTitle=fragment -4.53"), Some(1404604)),
      ListingTrace(quill, "f3", None, "BelowThreshold", Seq(quillRefused.ruleId), None, Nil, Some(49258), Seq(quillRefused),
        Seq("title \"Ein Hund namens Quill\": 0 film(s)"), Seq("49258 26.6% rank - 'Quill - Ein Freund für´s Leben' (2004)"),
        Some("rule:below-the-rating-cut"))))
    val c = new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(), TestAdminAction.adminRepository,
      new Pins(new InMemoryPinStore, Now), store)
    val byRule = contentAsString(c.traces(Some("accept:favoured-calibrated"), None, None).apply(admin))
    byRule should include ("Brides of Dracula")
    byRule should include ("director=same_person +4.22")
    byRule should not include ("Camino")
    contentAsString(c.traces(None, Some(23220), None).apply(admin)) should include ("Brides of Dracula")
    val byTitle = contentAsString(c.traces(None, None, Some("camino")).apply(admin))
    byTitle should include ("vetoed by")
    byTitle should include ("originalTitle=fragment -4.53")
    // a listing no rule took: each rule's refusal with the candidate it weighed and what that said
    val refused = contentAsString(c.traces(None, None, Some("quill")).apply(admin))
    refused should include ("refused:favoured-calibrated:below-the-rating-cut")
    refused should include ("tmdb 49258")
    refused should include ("26.6% &lt; 40.0%")
    // what it searched and weighed, and the blocker that stopped it, linked to every listing it stopped
    refused should include ("title &quot;Ein Hund namens Quill&quot;: 0 film(s)")
    refused should include ("Quill - Ein Freund für´s Leben")
    contentAsString(c.traces(None, None, None, Some("rule:below-the-rating-cut")).apply(admin)) should include ("Ein Hund namens Quill")
    val counts = contentAsString(c.traces(None, None, None).apply(admin))
    // asked nothing: the next wins, the blocker stopping most listings first
    counts should include ("Next wins")
    counts should include ("?blocker=rule%3Abelow-the-rating-cut")
    counts should include ("accept:favoured-calibrated")
    counts should include ("join:same-film")
    // admins only
    status(c.traces(None, None, None).apply(FakeRequest())) should not be OK
  }

  "the identity page" should "list the pins and link the traces" in {
    val (c, pins) = fixture()
    pins.add(Seq(ListingKey.Published("A", "One", Some(2020), Seq("X"))), PinClaim.NeverFilm(7), "a", "the strand").toOption.get
    val page = contentAsString(c.index.apply(admin))
    page should include ("never film 7")
    page should include ("the strand")
    page should include ("/admin/identity/traces")
  }

  it should "refuse an anonymous caller and a non-admin" in {
    val (c, _) = fixture()
    status(c.index.apply(FakeRequest())) shouldBe UNAUTHORIZED
    val member = new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(allow = Set.empty),
      TestAdminAction.adminRepository, fixture()._2)
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
  val Now: Clock = Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC)

}
