package views

import controllers.LegalController
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout}
import play.api.test.{FakeRequest, Helpers}
import tools.{CdpPage, Chrome, TestHttpServer}

/**
 * The language picker on `/privacy-policy`, driven in real Chrome: choosing a
 * language must reload the SAME document in it, on the SAME mount prefix — the
 * UK deployment serves it at `showtimes.cc/uk/privacy-policy`, and a picker that
 * built its own URL could drop the `/uk` and land on another country's site.
 *
 * Every request is answered by the real `LegalController`, keyed on the query
 * the browser actually sent, so the assertion is on what a visitor would get.
 */
class PrivacyLanguageSwitchSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val Path       = "/uk/privacy-policy"
  private val controller = new LegalController(Helpers.stubControllerComponents(), models.Country.UnitedKingdom)

  private def render(lang: Option[String]): String =
    contentAsString(controller.privacy(lang)(FakeRequest("GET", Path)))

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(harness.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer({
      case Path                                    => render(None)
      case p if p.startsWith(Path + "?lang=")      => render(Some(p.stripPrefix(Path + "?lang=")))
    })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onPolicy(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + Path)(body)
    case None    => cancel("Chrome not installed — skipping privacy language switch test")
  }

  private def choose(page: CdpPage, code: String): Unit = {
    page.navigateBy(
      s"""(() => { const s = document.querySelector('.lang-switch select');
         |  s.value = '$code'; s.dispatchEvent(new Event('change')); })()""".stripMargin)
    page.evalString("document.documentElement.lang") shouldBe code
  }

  "the privacy policy's language picker" should "list every published language, the page's own selected" in {
    onPolicy { page =>
      page.evalString("document.documentElement.lang") shouldBe "en"
      page.evalString("JSON.stringify([...document.querySelectorAll('.lang-switch option')].map(o => o.value))") shouldBe
        """["pl","en","de","es"]"""
      page.evalString("document.querySelector('.lang-switch select').value") shouldBe "en"
    }
  }

  it should "reload the same document in the chosen language, keeping the mount prefix" in {
    onPolicy { page =>
      choose(page, "de")
      page.evalString("location.pathname") shouldBe Path
      page.evalString("location.search") shouldBe "?lang=de"
      page.evalString("document.querySelector('h1').textContent") shouldBe "Datenschutzerklärung"
      page.evalString("document.querySelector('.lang-switch select').value") shouldBe "de"

      choose(page, "pl")
      page.evalString("location.pathname") shouldBe Path
      page.evalString("document.querySelector('h1').textContent") shouldBe "Polityka prywatności"

      choose(page, "es")
      page.evalString("location.pathname") shouldBe Path
      page.evalString("document.querySelector('h1').textContent") shouldBe "Política de privacidad"
    }
  }
}
