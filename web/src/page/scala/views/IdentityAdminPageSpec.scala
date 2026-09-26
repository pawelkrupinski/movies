package views

import controllers.{IdentityAdminController, IdentityAdminControllerSpec, TestAdminAction}
import com.sun.net.httpserver.HttpExchange
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout, status}
import play.api.test.{FakeRequest, Helpers}
import services.identity.ConfidenceCalibration.Sample
import services.identity.{Decision, InMemoryPinStore, PinClaim, Pins, ShadowDecisions}
import services.movies.ListingKey
import tools.{CdpPage, Chrome, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.time.{Clock, Instant, ZoneOffset}

/**
 * `/admin/identity` in real Chrome: the decisions render with their reasons, and the page's own
 * script pins the TICKED listing (not a hand-built body) and removes a pin — each POST served
 * by the real controller over the real pin rules, the page reloading onto the new state.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class IdentityAdminPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val pins = new Pins(new InMemoryPinStore, Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC))
  private val controller = new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(),
    TestAdminAction.adminRepository, pins, IdentityAdminControllerSpec.Shadow)
  /** Three low-confidence decisions whose listing order differs from their confidence order. */
  private val sortable = new IdentityAdminController(Helpers.stubControllerComponents(), TestAdminAction(),
    TestAdminAction.adminRepository, pins, new ShadowDecisions {
      private def d(title: String, confidence: Double) =
        IdentityAdminControllerSpec.D(Set(ListingKey.Published("Kino Amok", title, None, Nil)), None, confidence, Nil, Nil)
      def latest(): Seq[Decision] = Seq(d("Aa", 0.3), d("Bb", 0.1), d("Cc", 0.2))
      def verdicts(): Seq[Sample] = IdentityAdminControllerSpec.Shadow.verdicts()
    })
  private def admin[A](r: FakeRequest[A]) = r.withSession("userId" -> TestAdminAction.AdminUserId)

  /** The page's POSTs, answered by the controller as an admin's session. */
  private def post(exchange: HttpExchange): Boolean = {
    val path = exchange.getRequestURI.getPath
    val action = path match {
      case "/admin/identity/pins"        => Some(controller.createPin)
      case "/admin/identity/pins/remove" => Some(controller.removePin)
      case _                             => None
    }
    action.filter(_ => exchange.getRequestMethod == "POST").exists { a =>
      val body   = Json.parse(new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8))
      val result = a(admin(FakeRequest("POST", path)).withBody(body))
      val bytes  = contentAsString(result).getBytes(StandardCharsets.UTF_8)
      exchange.getResponseHeaders.add("Content-Type", "application/json")
      exchange.sendResponseHeaders(status(result), bytes.length.toLong)
      val os = exchange.getResponseBody
      try os.write(bytes) finally os.close()
      true
    }
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer(
      {
        case "/admin/identity"          => contentAsString(controller.index(admin(FakeRequest("GET", "/admin/identity"))))
        case "/admin/identity/sortable" => contentAsString(sortable.index(admin(FakeRequest("GET", "/admin/identity"))))
      },
      dynamicRoute = post)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onPage(body: CdpPage => Any): Unit = onPath("/admin/identity")(body)

  private def onPath(path: String)(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + path)(body(_))
    case None    => cancel("Chrome not installed — skipping /admin/identity page test")
  }

  /** Run `act`, which makes the page reload itself, and wait for the NEW document. */
  private def reloadingAfter(page: CdpPage)(act: String): Unit = {
    page.eval("window.__stamp = 1")
    page.eval(act)
    page.waitFor("typeof window.__stamp === 'undefined' && document.readyState === 'complete'", timeoutMs = 10000)
  }

  private val samson = ListingKey.Published("Kino Amok", "Samson i Dalila", None, Nil)

  "the identity page" should "show the contradicted and low-confidence decisions with their reasons" in {
    onPage { page =>
      page.evalInt("document.querySelectorAll('#contradicted tr.decision').length") shouldBe 1
      page.evalString("document.querySelector('#contradicted .contra').textContent") shouldBe "different bracketed years hold it apart"
      page.evalInt("document.querySelectorAll('#low-confidence tr.decision').length") shouldBe 1
      page.evalString("document.querySelector('#low-confidence tr.decision').textContent") should include ("bare title, no year")
      page.evalString("document.getElementById('calibration').textContent") should include ("threshold 0.5")
    }
  }

  it should "pin the ticked listing, then remove the pin, reloading onto each new state" in {
    onPage { page =>
      page.evalInt("document.querySelectorAll('tr.pin').length") shouldBe 0
      page.eval("""(function(){
        document.querySelector('#low-confidence .pick').checked = true;
        document.getElementById('kind').value = 'never-film';
        document.getElementById('tmdb').value = '22683';
        document.getElementById('reason').value = 'an opera broadcast';
      })()""")
      reloadingAfter(page)("document.getElementById('pin').click()")
      pins.all().map(p => (p.listings, p.claim, p.author, p.reason)) shouldBe
        Seq((Seq(samson), PinClaim.NeverFilm(22683), TestAdminAction.AdminEmail, "an opera broadcast"))
      page.evalInt("document.querySelectorAll('tr.pin').length") shouldBe 1
      page.evalString("document.querySelector('tr.pin').textContent") should include ("never film 22683")

      reloadingAfter(page)("document.querySelector('.unpin').click()")
      pins.all() shouldBe empty
      page.evalInt("document.querySelectorAll('tr.pin').length") shouldBe 0
    }
  }

  it should "show the pin rules' refusal and stay on the page" in {
    onPage { page =>
      page.eval("""(function(){
        document.querySelector('#contradicted .pick').checked = true;
        document.getElementById('kind').value = 'same-film';
        document.getElementById('reason').value = 'one film';
        document.getElementById('pin').click();
      })()""")
      page.waitFor("document.getElementById('status').className === 'err'")
      page.evalString("document.getElementById('status').textContent") should include ("two or more")
      pins.all() shouldBe empty
    }
  }

  it should "order a table by listing or confidence on a header click, flipping direction on the next click" in {
    onPath("/admin/identity/sortable") { page =>
      def titles() = page.evalString(
        "[...document.querySelectorAll('#low-confidence tr.decision')].map(tr => tr.dataset.listing.split(' — ')[1]).join(',')")
      def click(column: String) = page.eval(s"document.querySelector('#low-confidence th[data-sort=$column]').click()")
      def arrow(column: String) = page.evalString(s"document.querySelector('#low-confidence th[data-sort=$column]').dataset.dir || ''")

      titles() shouldBe "Bb,Cc,Aa"                  // server order: ascending confidence
      arrow("confidence") shouldBe "asc"
      click("listing")
      titles() shouldBe "Aa,Bb,Cc"
      (arrow("listing"), arrow("confidence")) shouldBe (("asc", ""))
      click("listing")
      titles() shouldBe "Cc,Bb,Aa"
      arrow("listing") shouldBe "desc"
      click("confidence")
      titles() shouldBe "Bb,Cc,Aa"
      click("confidence")
      titles() shouldBe "Aa,Cc,Bb"
      (arrow("listing"), arrow("confidence")) shouldBe (("", "desc"))
    }
  }
}
