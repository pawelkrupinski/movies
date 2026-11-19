package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.{FakeRequest, Helpers}
import play.api.test.Helpers.*
import services.readmodel.TestReadModel
import settings.ShareCardDirectoryTemplate

import java.nio.file.Files

/**
 * The dev-only share-card galleries (`/debug/share-cards/cities`, `/debug/share-cards/movies`) and the route serving
 * their card files: every city of the roster plus the home card, a sample of the films showing, each at its production
 * address and — when its file is in this machine's share-card directory — at the dev file route, else "not drawn yet".
 * All of it 404s in production, like the tune pages.
 */
class ShareCardGalleryControllerSpec extends AnyFlatSpec with Matchers with tools.ScratchDirectories {

  private val drawnCard = "f0123456789abcd.jpg"

  private val records = Seq("Diuna", "Belle", "Anora").map(title => TestCityControllers.showing(title))

  /** The gallery over a share-card directory holding Poznań's city card and Diuna's film card (Diuna's and Belle's
   *  read-model rows name a card; Anora's names none). */
  private def controller(mode: Mode): ShareCardGalleryController = {
    val root = tempDirectory("share-card-gallery")
    Files.createDirectories(root.resolve("pl"))
    Files.write(root.resolve("pl").resolve("city-poznan.jpg"), Array[Byte](1, 2, 3))
    Files.write(root.resolve("pl").resolve(drawnCard), Array[Byte](4, 5, 6))
    val store = TestReadModel.store(records)
    store.findAllMovies().foreach { film =>
      film.title match {
        case "Diuna" => store.upsertMovie(film.copy(shareCard = Some(s"$drawnCard?v=7")))
        case "Belle" => store.upsertMovie(film.copy(shareCard = Some("f9999999999abcd.jpg?v=1")))
        case _       => ()
      }
    }
    new ShareCardGalleryController(Helpers.stubControllerComponents(), mode,
      DebugReadModels.of(Country.Poland, store, mirror = None), new ShareCardFiles(ShareCardDirectoryTemplate(s"$root/{cc}")))
  }

  private def get(path: String) = FakeRequest(GET, path)

  "GET /debug/share-cards/cities" should "show the home card and every city card of the country's roster" in {
    val html = contentAsString(controller(Mode.Dev).cities().apply(get("/debug/share-cards/cities")))

    html should include("/assets/img/og-home.jpg")
    html should include("https://kinowo.net/assets/img/og-home.jpg")
    Country.Poland.cities.foreach(city => html should include(ShareCardUrl.city(city)))
    // Drawn: served by the dev file route. Not drawn: a placeholder.
    html should include("""<img src="/debug/share-cards/pl/city-poznan.jpg"""")
    html should not include "/debug/share-cards/pl/city-krakow.jpg"
    html should include("not drawn yet")
    // The way to its sibling page, and to the cards themselves.
    html should include("/debug/share-cards/movies?country=pl")
    html should include("rsync -a k3s-worker-1:/var/lib/kinowo/share-cards/pl/")
  }

  it should "show another country's roster for ?country=" in {
    val html = contentAsString(controller(Mode.Dev).cities().apply(get("/debug/share-cards/cities?country=uk")))
    Country.byCode("uk").get.cities.foreach(city => html should include(ShareCardUrl.city(city)))
  }

  "GET /debug/share-cards/movies" should "show the films' cards, by title, up to ?limit=" in {
    val ctrl = controller(Mode.Dev)
    val html = contentAsString(ctrl.movies(None).apply(get("/debug/share-cards/movies")))

    html should include(s"""<img src="/debug/share-cards/pl/$drawnCard"""")
    html should include(s"https://kinowo.net/share-cards/pl/$drawnCard?v=7")
    html should include("https://kinowo.net/share-cards/pl/f9999999999abcd.jpg?v=1")
    html should include("not drawn yet")                      // Belle's card is not on this machine
    html should include("no share card in the read model")    // Anora has none
    html should include("/debug/share-cards/cities?country=pl")

    val limited = contentAsString(ctrl.movies(Some(1)).apply(get("/debug/share-cards/movies?limit=1")))
    limited should include("Anora")
    limited should not include "Diuna"
  }

  "GET /debug/share-cards/:country/:file" should "serve a card on disk and nothing outside the country's directory" in {
    val ctrl = controller(Mode.Dev)
    val card = ctrl.file("pl", drawnCard).apply(get(s"/debug/share-cards/pl/$drawnCard"))
    status(card) shouldBe OK
    contentType(card) shouldBe Some("image/jpeg")
    contentAsBytes(card).toArray shouldBe Array[Byte](4, 5, 6)

    status(ctrl.file("pl", "city-krakow.jpg").apply(get("/"))) shouldBe NOT_FOUND
    status(ctrl.file("pl", "..").apply(get("/"))) shouldBe NOT_FOUND
    status(ctrl.file("pl", "../pl/city-poznan.jpg").apply(get("/"))) shouldBe NOT_FOUND
  }

  "Every share-card gallery route" should "404 as a dev-only endpoint in production" in {
    val ctrl = controller(Mode.Prod)
    Seq(ctrl.cities().apply(get("/")), ctrl.movies(None).apply(get("/")), ctrl.file("pl", drawnCard).apply(get("/")))
      .foreach { result =>
        status(result) shouldBe NOT_FOUND
        contentAsString(result) shouldBe "dev-only endpoint"
      }
  }
}
