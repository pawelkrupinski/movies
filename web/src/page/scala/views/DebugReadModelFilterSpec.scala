package views

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
 * expand handler (the inline `<script>` in debugReadModel.scala.html) — pinned
 * ahead of extracting its shared shape with debug.scala.html's own
 * filter+expand into one helper both pages call. Unlike debug.scala.html
 * (whose details rows are parked off-DOM and fetched lazily — see
 * PageJsBehaviourSpec's "/debug corpus table" specs), this page's details rows
 * stay in the DOM as plain hidden siblings, found by a `[data-row-id]`
 * attribute selector rather than a JS `Map`.
 *
 * Skips gracefully when Chrome isn't installed, same as CadenceFilterSpec.
 */
class DebugReadModelFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

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
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/debug/readmodel" => readModelHtml })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onReadModel(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/readmodel")(body(_))
    case None    => cancel("Chrome not installed — skipping /debug/readmodel filter JS behaviour test")
  }

  private def typeFilter(q: String): String =
    "(function(){var i=document.getElementById('q');" +
      s"i.value=${JsString(q)};i.dispatchEvent(new Event('input'));})()"

  private val visibleCount =
    "[...document.querySelectorAll('#t tbody tr.data')].filter(tr => !tr.classList.contains('hidden')).length"

  private def dataRow(needle: String): String =
    s"[...document.querySelectorAll('#t tbody tr.data')].find(tr => tr.dataset.haystack.includes('$needle'))"

  private def detailsFor(needle: String): String =
    s"document.querySelector('#t tbody tr.details[data-row-id=\"' + ${dataRow(needle)}.dataset.rowId + '\"]')"

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
