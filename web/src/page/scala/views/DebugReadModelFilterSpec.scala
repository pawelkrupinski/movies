package views

import tools.SpecClock.given

import controllers.TestDebugController
import models.MovieRecord
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.JsString
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, contentAsString, defaultAwaitTimeout}
import tools.{CdpPage, Chrome, TestHttpServer}

/**
 * JS-behaviour regression for `/debug/readmodel`'s `filterRows`/row-click
 * expand handler (`debugTable.js` plus the inline `<script>` in
 * debugReadModel.scala.html): the details rows stay in the DOM as plain hidden
 * siblings, found by a `[data-row-id]` attribute selector.
 *
 * Skips gracefully when Chrome isn't installed.
 */
class DebugReadModelFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.SuiteConfiguration {

  // Two resolved movies with distinct, non-overlapping titles so a query can
  // isolate one and hide the other.
  private val records = Seq(
    ("Pending Film",    Some(2024), MovieRecord(tmdbId = Some(1))),
    ("Unresolved Film", Some(2023), MovieRecord()),
  )

  private val readModelHtml: String = {
    val ctrl = TestDebugController.build(records, Mode.Dev)._1
    contentAsString(ctrl.debugReadModel().apply(FakeRequest(GET, "/debug/readmodel")))
  }

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart(harness.cdpBrowserBinary)
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/debug/readmodel" => readModelHtml })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onReadModel(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/readmodel") { page =>
      Seq("common/dom.js", "debug/read-model-table.js").foreach(page.useScript)
      body(page)
    }
    case None    => cancel("Chrome not installed — skipping /debug/readmodel filter JS behaviour test")
  }

  private def typeFilter(q: String): String = s"__dom.type('#q', ${JsString(q)})"

  private val visibleCount = "__debugTable.shownRows()"

  private def dataRow(needle: String): String = s"__debugTable.row('$needle')"

  private def detailsFor(needle: String): String = s"__debugTable.detailsOf('$needle')"

  "the /debug/readmodel filter" should "hide rows the search box doesn't match" in {
    onReadModel { page =>
      page.waitFor("document.querySelectorAll('#t tbody tr.data').length === 2")
      page.evalInt(visibleCount) shouldBe 2

      page.eval(typeFilter("pending"))
      page.evalInt(visibleCount) shouldBe 1
      page.evalBool(s"!${dataRow("pending")}.classList.contains('hidden')") shouldBe true
      page.evalBool(s"${dataRow("unresolved")}.classList.contains('hidden')") shouldBe true

      page.eval(typeFilter(""))
      page.evalInt(visibleCount) shouldBe 2
    }
  }

  it should "toggle a row's details (a DOM sibling, not a fetch) on click, and collapse it when filtered out" in {
    onReadModel { page =>
      page.waitFor("document.querySelectorAll('#t tbody tr.data').length === 2")
      page.eval(s"${dataRow("pending")}.click()")
      page.evalBool(s"${dataRow("pending")}.classList.contains('expanded')") shouldBe true
      page.evalBool(s"!${detailsFor("pending")}.classList.contains('hidden')") shouldBe true

      // Filtering it away collapses the row AND re-hides its (still in-DOM) details.
      page.eval(typeFilter("unresolved"))
      page.evalBool(s"${dataRow("pending")}.classList.contains('expanded')") shouldBe false
      page.evalBool(s"${detailsFor("pending")}.classList.contains('hidden')") shouldBe true
    }
  }
}
