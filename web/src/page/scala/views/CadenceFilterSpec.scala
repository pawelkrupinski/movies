package views

import models.Poznan
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsString
import services.cadence.CadenceReport
import tools.{CdpPage, Chrome, TestHttpServer}

import java.time.Instant
import scala.concurrent.duration._

/**
 * JS-behaviour regression for the /debug/cadence title filter (the inline
 * `<script>` in cadence.scala.html). Renders the real view with a couple of
 * groups, drives the filter box in headless Chrome over CDP, and asserts the
 * DOM: non-matching rows hide, groups with no match drop out, matching groups
 * auto-open (they render collapsed), and clearing the box restores everything.
 *
 * Skips gracefully when Chrome isn't installed — CI images without a browser
 * get cancelled tests; a dev with Chrome gets the full coverage.
 */
class CadenceFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val city: models.City = Poznan
  private val now = Instant.parse("2026-07-21T00:00:00Z")

  private def entry(site: String, title: String, interval: FiniteDuration) =
    CadenceReport.Entry(site, title, interval, now.plusSeconds(3600),
      streak = 3, windowChecks = 10, windowChanges = 1, lastChange = None, prevChange = None)

  // Two interval groups: a slow one with two films, a fast one with a single film
  // whose title is unique — so a filter matching only it must both hide the other
  // group entirely and open its own collapsed group.
  private val groups = Seq(
    CadenceReport.Group(4.days,  Seq(entry("imdb", "Odyseja", 4.days), entry("fw", "Dune", 4.days))),
    CadenceReport.Group(2.hours, Seq(entry("mc", "Oppenheimer", 2.hours))),
  )

  private val cadenceHtml: String = views.html.cadence(groups, now).body

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) server = new TestHttpServer({ case "/debug/cadence" => cadenceHtml })
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onCadence(body: CdpPage => Any): Unit = chrome match {
    case Some(c) => c.openPage(server.baseUrl + "/debug/cadence")(body(_))
    case None    => cancel("Chrome not installed — skipping cadence filter JS behaviour test")
  }

  // JSON array of the titles of every row not currently hidden, sorted — a clean,
  // collapse-independent read of "what the filter is letting through".
  private val visibleTitles =
    "JSON.stringify(Array.prototype.slice.call(document.querySelectorAll('tbody tr'))" +
      ".filter(function(tr){return !tr.hidden})" +
      ".map(function(tr){return tr.querySelector('.film').textContent.trim()}).sort())"

  private def typeFilter(q: String): String =
    s"(function(){var i=document.getElementById('cadence-filter');" +
      s"i.value=${JsString(q)};i.dispatchEvent(new Event('input'));})()"

  private val shownGroups = "document.querySelectorAll('details.grp:not([hidden])').length"

  "the /debug/cadence filter" should "hide rows whose title doesn't contain the query" in {
    onCadence { page =>
      page.evalString(visibleTitles) shouldBe """["Dune","Odyseja","Oppenheimer"]"""

      page.eval(typeFilter("dune"))
      page.evalString(visibleTitles) shouldBe """["Dune"]"""
    }
  }

  it should "drop a group with no matching row and open the group that matches" in {
    onCadence { page =>
      // "opp" matches only Oppenheimer, which lives alone in the fast (2h) group.
      page.eval(typeFilter("opp"))
      page.evalString(visibleTitles) shouldBe """["Oppenheimer"]"""
      page.evalInt(shownGroups) shouldBe 1                       // the 4d group is hidden
      // The matching group renders collapsed by default; the filter opens it so the
      // hit is actually visible, not buried behind a closed <details>.
      page.evalBool("document.querySelectorAll('details.grp[open]:not([hidden])').length === 1") shouldBe true
    }
  }

  it should "match case-insensitively and on a substring" in {
    onCadence { page =>
      page.eval(typeFilter("YSE"))                               // inside 'Od-YSE-ja'
      page.evalString(visibleTitles) shouldBe """["Odyseja"]"""
    }
  }

  it should "restore every row and the default collapse state when the box is cleared" in {
    onCadence { page =>
      page.eval(typeFilter("dune"))
      page.evalString(visibleTitles) shouldBe """["Dune"]"""

      page.eval(typeFilter(""))
      page.evalString(visibleTitles) shouldBe """["Dune","Odyseja","Oppenheimer"]"""
      page.evalInt(shownGroups) shouldBe 2                       // both groups back
      // Restored to the server-rendered default: groups collapsed.
      page.evalInt("document.querySelectorAll('details.grp[open]').length") shouldBe 0
    }
  }
}
