package views

import controllers.ShareCardGalleryController.{CityCard, FilmCard, Gallery}
import controllers.WebHref
import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import tools.{CdpPage, Chrome, TestHttpServer}

/**
 * `/admin/share-cards` in real Chrome: two tabs, cities and films, each with its own search box that hides
 * the cards it does not match — accent- and case-blind, every word must match — and counts what is left.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class ShareCardGalleryPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  /** A card address nothing answers: the page only lays the images out. */
  private def card(path: String): Option[WebHref] = WebHref.of(s"http://cards.invalid$path")

  private val gallery = Gallery(Country.Poland,
    cities = Seq(CityCard("Łódź", "lodz", card("/c/lodz.jpg")), CityCard("Poznań", "poznan", card("/c/poznan.jpg")),
      CityCard("Wrocław", "wroclaw", card("/c/wroclaw.jpg"))),
    films = Seq(FilmCard("Belle", Some(2021), None, pending = true), FilmCard("Diuna", Some(2021), card("/f/diuna.jpg"), pending = false),
      FilmCard("Diuna: Część druga", Some(2024), card("/f/diuna2.jpg"), pending = false)))

  private val html = views.html.admin.shareCards(gallery).body

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/admin/share-cards" | "/other-country/admin/share-cards" => html })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onPage(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/admin/share-cards") { page =>
      page.useScript("common/dom.js")
      body(page)
    }
    case None    => cancel("Chrome not installed — skipping /admin/share-cards page test")
  }

  private def shown(page: CdpPage, panel: String): Seq[String] =
    page.eval(s"__dom.texts('[data-panel=$panel] .card:not([hidden]) .label')").as[Seq[String]]

  private def count(page: CdpPage, panel: String): String =
    page.evalString(s"document.querySelector('[data-panel=$panel] .count').textContent")

  private def panelHidden(page: CdpPage, panel: String): Boolean =
    page.evalBool(s"document.querySelector('[data-panel=$panel]').hidden")

  private def search(page: CdpPage, panel: String, text: String): Unit =
    page.eval(s"__dom.type('[data-panel=$panel] input', ${Json.toJson(text)})")

  "the share-card gallery" should "open on the cities tab, every card shown and counted" in {
    onPage { page =>
      panelHidden(page, "cities") shouldBe false
      panelHidden(page, "films") shouldBe true
      shown(page, "cities") shouldBe Seq("Łódź", "Poznań", "Wrocław")
      count(page, "cities") shouldBe "3 of 3"
    }
  }

  it should "filter the cities by name or slug, blind to accents and case" in {
    onPage { page =>
      search(page, "cities", "LODZ")
      shown(page, "cities") shouldBe Seq("Łódź")
      count(page, "cities") shouldBe "1 of 3"
      search(page, "cities", "wroc")
      shown(page, "cities") shouldBe Seq("Wrocław")
      search(page, "cities", "")
      shown(page, "cities") should have size 3
    }
  }

  it should "switch to the films tab, which keeps a search of its own, every word having to match" in {
    onPage { page =>
      search(page, "cities", "poznan")
      page.eval("__dom.click('[data-tab=films]')")
      panelHidden(page, "cities") shouldBe true
      panelHidden(page, "films") shouldBe false
      page.evalString("location.hash") shouldBe "#films"
      shown(page, "films") shouldBe Seq("Belle", "Diuna", "Diuna: Część druga")
      search(page, "films", "diuna 2024")
      shown(page, "films") shouldBe Seq("Diuna: Część druga")
      count(page, "films") shouldBe "1 of 3"
      page.eval("__dom.click('[data-tab=cities]')")
      shown(page, "cities") shouldBe Seq("Poznań")
    }
  }

  // The switch's options name the other countries' real hosts; one is pointed back at this server.
  it should "switch country onto the same page, keeping the open tab" in {
    onPage { page =>
      page.eval("__dom.click('[data-tab=films]')")
      page.eval("document.querySelector('.country-switch').options[1].value = location.origin + '/other-country/admin/share-cards'")
      page.eval("var s = document.querySelector('.country-switch'); s.selectedIndex = 1; s.dispatchEvent(new Event('change'))")
      page.waitFor("location.pathname === '/other-country/admin/share-cards' && document.readyState === 'complete'")
      page.evalString("location.hash") shouldBe "#films"
      panelHidden(page, "films") shouldBe false
    }
  }

  it should "say which films have no card, and which are pending" in {
    onPage { page =>
      page.evalInt("document.querySelectorAll('[data-panel=films] .none').length") shouldBe 1
      page.evalInt("document.querySelectorAll('[data-panel=films] .pending').length") shouldBe 1
    }
  }
}
