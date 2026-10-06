package views

import com.sun.net.httpserver.HttpExchange
import controllers.ReviewController
import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout, status}
import play.api.test.{FakeRequest, Helpers}
import services.review._
import tools.{CdpPage, Chrome, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.{Clock, Instant, ZoneOffset}

/**
 * `/debug/review` in real Chrome: the queue rendered by the real controller from a fixture, an answer
 * button clicked, the page's own script posting the card's payload to the real controller — the answer
 * stored, the card hidden, and a reload no longer listing the cluster. Then "Other film" with a pasted
 * link, and "Undo" bringing a card back.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class ReviewPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val now     = Instant.parse("2026-10-06T10:00:00Z")
  private val answers = new ReviewAnswers(new InMemoryReviewAnswerStore)
  private val labels  = Files.createTempFile("labels", ".tsv")
  private val controller = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev,
    Map(Country.Poland -> ReviewFixtures.source(now)), answers, labels, Clock.fixed(now, ZoneOffset.UTC))

  private def post(exchange: HttpExchange): Boolean = {
    val path = exchange.getRequestURI.getPath
    lazy val body = Json.parse(new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8))
    val result = Option.when(exchange.getRequestMethod == "POST")(path).collect {
      case "/debug/review/answer" => controller.answer()(FakeRequest("POST", path).withBody(body))
      case "/debug/review/export" => controller.exportLabels()(FakeRequest("POST", path))
    }
    result.foreach { r =>
      val bytes = contentAsString(r).getBytes(StandardCharsets.UTF_8)
      exchange.getResponseHeaders.add("Content-Type", "application/json")
      exchange.sendResponseHeaders(status(r), bytes.length.toLong)
      val os = exchange.getResponseBody
      try os.write(bytes) finally os.close()
    }
    result.isDefined
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer(
      { case "/debug/review?country=pl" => contentAsString(controller.queue(Some("pl"), 60, false)(FakeRequest())) },
      dynamicRoute = post)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
    Files.deleteIfExists(labels): Unit
  }

  private def onQueue(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/review?country=pl")(body(_))
    case None    => cancel("Chrome not installed — skipping /debug/review page test")
  }

  private def card(title: String) =
    s"Array.prototype.find.call(document.querySelectorAll('.card'), function(c){ return c.querySelector('.raw-title').textContent === ${Json.toJson(title)} })"

  "the review queue" should "store a clicked answer, hide its card, and leave it out of the next load" in {
    onQueue { page =>
      page.evalInt("document.querySelectorAll('.card').length") shouldBe 3
      page.eval(s"${card("FRANZ KAFKA")}.querySelector('button[data-verdict=event]').click()")
      page.waitFor(s"${card("FRANZ KAFKA")}.getAttribute('data-answered') === 'event'")
      page.waitFor(s"${card("FRANZ KAFKA")}.hidden === true")
      answers.current().map(a => (a.title, a.verdict, a.shown.map(_.ref.render))) shouldBe
        Seq(("FRANZ KAFKA", ReviewVerdict.Event, Some("tmdb:1157322")))

      page.reload()
      page.evalInt("document.querySelectorAll('.card').length") shouldBe 2
      page.evalString("document.querySelector('.summary').textContent") should include ("1 answered hidden")
    }
  }

  it should "take an Other film link, refuse one that names no film, and undo an answer" in {
    onQueue { page =>
      val macbeth = card("Macbeth")
      page.eval(s"$macbeth.querySelector('input.other').value = 'https://example.com/nothing'")
      page.eval(s"$macbeth.querySelector('button[data-other]').click()")
      page.waitFor(s"$macbeth.querySelector('.status').classList.contains('is-error')")
      page.evalString(s"$macbeth.querySelector('.status').textContent") should include ("not a film link")

      page.eval(s"$macbeth.querySelector('input.other').value = 'https://www.imdb.com/title/tt0067372/'")
      page.eval(s"$macbeth.querySelector('button[data-other]').click()")
      page.waitFor(s"$macbeth.getAttribute('data-answered') === 'film'")
      answers.current().find(_.title == "Macbeth").flatMap(_.ref).map(_.render) shouldBe Some("imdb:tt0067372")

      page.eval(s"$macbeth.querySelector('button[data-verdict=undo]').click()")
      page.waitFor(s"$macbeth.getAttribute('data-answered') === ''")
      answers.current().find(_.title == "Macbeth") shouldBe None
    }
  }

  it should "export the answers into labels.tsv from the page's button" in {
    onQueue { page =>
      page.eval("document.getElementById('export-labels').click()")
      page.waitFor("document.getElementById('export-summary').textContent.indexOf('added') >= 0")
      LabelsTsv.read(labels).map(r => (r.rawTitle, r.film, r.verdict)) shouldBe Seq(("FRANZ KAFKA", "tmdb:1157322", "wrong"))
    }
  }
}
