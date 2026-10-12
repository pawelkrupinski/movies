package controllers

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import services.readmodel.{TestReadModel, WebReadModel}

/**
 * `/admin/share-cards` shows every city card and every film card the serving country's pages name, at the
 * addresses those pages' `og:image` points at, behind the admin gate.
 */
class ShareCardGalleryControllerSpec extends AnyFlatSpec with Matchers {

  private val card = "f0123456789abcd.jpg?v=0123456789abcdef"

  private val records = Seq("Diuna", "Belle").map(title =>
    TestMovieController.showing(title, posterUrl = Some(s"https://cinema.example/${title.toLowerCase}.jpg")))

  /** Diuna has a share card, Belle has none. */
  private def controller(): ShareCardGalleryController = {
    val store = TestReadModel.store(records)
    store.findAllMovies().filter(_.title == "Diuna").foreach(m => store.upsertMovie(m.copy(shareCard = Some(card))))
    val readModel = new WebReadModel(store, clock = _root_.tools.SpecClock.Pinned)
    readModel.reload()
    new ShareCardGalleryController(Helpers.stubControllerComponents(), TestAdminAction(), readModel, Country.Poland)
  }

  private def page(): String =
    contentAsString(controller().index(FakeRequest("GET", "/admin/share-cards").withSession("userId" -> TestAdminAction.AdminUserId)))

  "The share-card gallery" should "show every city of the country with its card" in {
    val html = page()
    Country.Poland.cities.foreach(city => html should include(s"""src="${ShareCardUrl.city(city)}""""))
  }

  it should "show a film's card, and say when a film has none" in {
    val html = page()
    html should include(s"""src="https://kinowo.net/share-cards/pl/$card"""")
    html should include("no card")
    html should include("1 without a card")
  }

  it should "list the films without a card first" in {
    val html = page()
    html.indexOf(">Belle<") should be < html.indexOf(">Diuna<")
  }

  it should "switch to the same page in every other country, this one selected" in {
    val html = page()
    Country.switchable.size should be > 1
    Country.switchable.foreach { c =>
      html should include(s"""<option value="${c.webUrl.get}/admin/share-cards"${if (c == Country.Poland) " selected" else ""}>""")
    }
  }

  it should "refuse anyone not signed in" in {
    status(controller().index(FakeRequest("GET", "/admin/share-cards"))) shouldBe UNAUTHORIZED
  }
}
