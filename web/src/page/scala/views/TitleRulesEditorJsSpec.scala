package views

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.{RuleScope, TitleRule, TitleRuleRecord}
import tools.{CdpPage, Chrome, TestHttpServer}

/**
 * JS-behaviour regression for the dark admin title-rules editor. Renders the
 * real Twirl template with a couple of records + the cinema dropdown options,
 * serves it over a tiny HTTP server, and drives it in headless Chrome over CDP.
 *
 * The editor's reorder / last-toggle / flatten logic is factored into the pure
 * `window.TitleRules` functions precisely so it's testable here WITHOUT
 * simulating raw pointer drags (which are flaky across Chrome versions): we
 * assert the decision logic that a drag drives, plus the dark theme and the
 * rendered scope structure. Skips cleanly when Chrome isn't installed.
 */
class TitleRulesEditorJsSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private var chrome: Option[Chrome] = None
  private var server: TestHttpServer = _

  private def rule(id: String, pattern: String): TitleRule =
    TitleRule(id, RuleScope.GlobalStructural, None, pattern, "", applyAll = true, order = 0)

  override def beforeAll(): Unit = {
    chrome = Chrome.tryStart()
    if (chrome.nonEmpty) {
      val records = Seq(
        TitleRuleRecord("GlobalStructural", RuleScope.GlobalStructural, None,
          rules = Seq(rule("g1", "ab"), rule("g2", "a")), lastRules = Nil),
        TitleRuleRecord("cinema-city", RuleScope.PerCinema, Some("cinema-city"),
          rules = Seq(rule("c1", "^X ")), lastRules = Seq(rule("c2", "Y$"))))
      val cinemas = Seq("cinema-city" -> "Cinema City", "kino-muza" -> "Kino Muza")
      val html = views.html.admin.titleRulesEditor(records, cinemas).body
      server = new TestHttpServer({ case _ => html })  // every path (incl. the /report fetch) serves the page
    }
  }

  override def afterAll(): Unit = {
    if (server != null) server.close()
    chrome.foreach(_.close())
  }

  private def onEditor(body: CdpPage => Any): Unit =
    chrome match {
      case Some(c) => c.openPage(server.baseUrl + "/admin/title-rules")(body(_))
      case None    => cancel("Chrome not installed — skipping title-rules editor JS test")
    }

  "the editor" should "render a dark theme" in {
    onEditor { page =>
      // --bg = #0d1117 = rgb(13, 17, 23)
      page.evalString("getComputedStyle(document.body).backgroundColor") shouldBe "rgb(13, 17, 23)"
    }
  }

  it should "render a section + explanation for each global scope and a per-cinema card with a dropdown" in {
    onEditor { page =>
      val text = page.evalString("document.body.textContent")
      text should include ("Global structural")
      text should include ("Search (API query only)")
      text should include ("Canonical")
      // a per-cinema card carries a real cinema dropdown listing the human label
      page.evalInt("document.querySelectorAll('.card select option').length") should be > 1
      page.evalBool(
        "[...document.querySelectorAll('.card select option')].some(o => o.textContent === 'Cinema City')"
      ) shouldBe true
    }
  }

  "window.TitleRules.moveItem" should "reorder an array (the drag-reorder decision)" in {
    onEditor { page =>
      page.evalString("JSON.stringify(window.TitleRules.moveItem(['a','b','c'], 0, 2))") shouldBe
        """["b","c","a"]"""
      page.evalString("JSON.stringify(window.TitleRules.moveItem(['a','b','c'], 2, 0))") shouldBe
        """["c","a","b"]"""
    }
  }

  "window.TitleRules.toggleLast" should "move a rule between the rules and lastRules lists" in {
    onEditor { page =>
      val res = page.evalString(
        """(() => { const rec = { rules: [{id:'x'}], lastRules: [] };
          |  window.TitleRules.toggleLast(rec, 'rules', 0);
          |  return JSON.stringify([rec.rules.length, rec.lastRules.length, rec.lastRules[0].id]); })()""".stripMargin)
      res shouldBe """[0,1,"x"]"""
    }
  }

  "window.TitleRules.flattenForPreview" should "stamp scope, position-order and last onto each rule" in {
    onEditor { page =>
      val res = page.evalString(
        """(() => {
          |  const recs = [{ scope:'Search', cinemaId:null,
          |    rules:[{id:'a',pattern:'p'},{id:'b',pattern:'q'}],
          |    lastRules:[{id:'z',pattern:'r'}] }];
          |  const flat = window.TitleRules.flattenForPreview(recs);
          |  return JSON.stringify(flat.map(x => [x.scope, x.order, x.last, x.pattern])); })()""".stripMargin)
      res shouldBe """[["Search",0,false,"p"],["Search",1,false,"q"],["Search",0,true,"r"]]"""
    }
  }
}
