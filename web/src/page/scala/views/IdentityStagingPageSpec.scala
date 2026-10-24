package views

import com.sun.net.httpserver.HttpExchange
import controllers.IdentityStagingController
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout, status}
import play.api.test.{FakeRequest, Helpers}
import services.identity.staging.{IdentityStaging, InMemoryStagingDocuments, StagedCluster, StagedListing, StagedQuestion, StagedTake, StagingBeat}
import tools.{CdpPage, Chrome, MutableClock, TestHttpServer}

import java.nio.charset.StandardCharsets
import java.time.Instant

/**
 * `/debug/identity/staging` in real Chrome: every country's section on the one page, each cluster a row that opens onto
 * its questions and their outcomes, and the page following what the workers publish next by its own polls — a row
 * left open stays open across them.
 *
 * Skips gracefully when Chrome isn't installed, same as the other PageTest specs.
 */
class IdentityStagingPageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  private val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  private val staging = new IdentityStaging(new InMemoryStagingDocuments, clock)
  private val controller = new IdentityStagingController(Helpers.stubControllerComponents(), play.api.Mode.Dev, Seq(staging),
    models.Country.Poland, clock, pollMillis = 100)
  private def admin(path: String) = FakeRequest("GET", path)

  private def question(text: String, outcome: String, attempts: Int = 0) =
    StagedQuestion("FamilySearch", "imdb", text, outcome, attempts, Option.when(attempts > 0)(clock.millis() + 600000), Option.when(attempts > 0)("imdb.com"))
  private def cluster(key: String, venue: String, title: String, questions: Seq[StagedQuestion], resolvedAt: Option[Long] = None) =
    StagedCluster(key, Seq(StagedListing(venue, title)), 1, StagedTake(None, None, "NoEvidence", 0.0), 0, onTmdb = false,
      questions, questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since = clock.millis() - 120000,
      resolvedAt = resolvedAt, resolution = resolvedAt.map(_ => StagedCluster.Settled))
  private def beat(n: Int) = StagingBeat(clock.millis(), n, n, 0, Map("FamilySearch" -> n), 0)

  private val diuna = cluster("a1", "Kino Muza", "Diuna",
    Seq(question("imdb|title|Diuna", StagedQuestion.Failed, attempts = 3), question("wiki|title|Diuna", StagedQuestion.NotAttempted)))

  /** The page's polls, answered by the controller, its query string and all. */
  private def data(exchange: HttpExchange): Boolean = exchange.getRequestURI.getPath == "/debug/identity/staging/data" && {
    val since  = Option(exchange.getRequestURI.getRawQuery).map(q => java.net.URLDecoder.decode(q.stripPrefix("since="), StandardCharsets.UTF_8))
    val result = controller.data(since)(admin("/debug/identity/staging/data"))
    val bytes  = contentAsString(result).getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.add("Content-Type", "application/json")
    exchange.sendResponseHeaders(status(result), bytes.length.toLong)
    val os = exchange.getResponseBody
    try os.write(bytes) finally os.close()
    true
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(configuration.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer(
      { case "/debug/identity/staging" => contentAsString(controller.index(admin("/debug/identity/staging"))) },
      dynamicRoute = data)
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onPage(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/identity/staging")(body)
    case None    => cancel("Chrome not installed — skipping /debug/identity/staging page test")
  }

  "the staging page" should "show every country at once, open a cluster onto its questions, and follow the next publish live" in {
    staging.publish("pl", Seq(diuna), Nil, beat(1))
    staging.publish("de", Seq(cluster("b1", "Babylon", "Der Pate", Seq(question("imdb|title|Der Pate", StagedQuestion.Answered)))), Nil, beat(1))
    onPage { page =>
      page.waitFor("document.querySelectorAll('section.country').length === " + models.Country.all.size)
      page.evalString("document.querySelector('section[data-country=pl] .pill.staging').textContent") should include ("1")
      page.evalString("document.querySelector('section[data-country=de] details.cluster summary').textContent") should include ("Der Pate")
      page.evalString("document.querySelector('section[data-country=uk]').textContent") should include ("no snapshot yet")
      page.evalString("document.querySelector('section[data-country=pl] details.cluster .body.meta').textContent") should include ("1/2 attempted")

      page.eval("document.querySelector('section[data-country=pl] details.cluster summary').click()")
      page.waitFor("document.querySelector('section[data-country=pl] details.cluster').open")
      page.evalInt("document.querySelectorAll('section[data-country=pl] details.cluster[open] tr.question').length") shouldBe 2
      page.evalString("document.querySelector('section[data-country=pl] tr.question .o-failed').textContent") should include ("failed 3×")

      // the next publish: Diuna stops waiting, and arrives under "Recently resolved" by the page's own poll, still open
      clock.advanceSeconds(20)
      staging.publish("pl", Seq(diuna.copy(resolvedAt = Some(clock.millis()), resolution = Some(StagedCluster.Settled))), Nil, beat(0))
      page.waitFor("document.querySelector('section[data-country=pl]').textContent.indexOf('settled') >= 0")
      page.evalString("document.querySelector('section[data-country=pl]').textContent") should include ("nothing waits")
      page.eval("document.querySelector('section[data-country=pl] details.cluster').open").as[Boolean] shouldBe true
    }
  }
}
