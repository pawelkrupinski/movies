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
          // g3 is a real Java/Scala pattern (inline `(?i)` flag) that JS's RegExp
          // rejects; gBad is genuinely malformed. The editor must red gBad, not g3.
          rules = Seq(rule("g1", "ab"), rule("g2", "a"),
            rule("g3", "(?i)^Klub: "), rule("gBad", "(oops")), lastRules = Nil),
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

  it should "not red a valid Java `(?i)` pattern, but still red a genuinely malformed one" in {
    onEditor { page =>
      def isRed(value: String): Boolean = page.evalBool(
        s"[...document.querySelectorAll('input.mono')].find(i => i.value === ${jsLit(value)})" +
          ".classList.contains('bad-pat')")
      withClue("a `(?i)…` Java regex must NOT be flagged invalid: ") { isRed("(?i)^Klub: ") shouldBe false }
      withClue("a malformed regex must still be flagged invalid: ")   { isRed("(oops")       shouldBe true  }
    }
  }

  it should "give every transient (external-lookup) rule an unfoldable affected-titles list, but not per-cinema rules" in {
    onEditor { page =>
      // GlobalStructural seeds 4 rules → 4 affected <details>; the Search/Canonical
      // cards seed no rules, so the only affected lists come from those 4.
      page.evalInt("document.querySelectorAll('.rule-wrap details.affected').length") shouldBe 4
      // The per-cinema card (the one carrying a cinema <select>) has rule rows but
      // NO affected list — its rules rewrite the stored record.
      page.evalInt(
        "[...document.querySelectorAll('.card')]" +
          ".filter(c => c.querySelector('select'))" +
          ".reduce((n, c) => n + c.querySelectorAll('details.affected').length, 0)"
      ) shouldBe 0
    }
  }

  it should "re-run the affected preview as the user edits a transient rule, but only once the regex compiles" in {
    onEditor { page =>
      // `scheduleAffected` is the debounced edit→refresh hook. It must fire for a
      // valid pattern and stay quiet for a malformed one (no point querying the
      // corpus with a regex the server will reject).
      page.evalBool("typeof window.TitleRules.scheduleAffected === 'function'") shouldBe true
      page.evalBool(
        """(() => { let fired = false;
          |  window.TitleRules.scheduleAffected({ pattern: '^Klub: ' }, () => { fired = true; });
          |  return fired; })()""".stripMargin) shouldBe true
      page.evalBool(
        """(() => { let fired = false;
          |  window.TitleRules.scheduleAffected({ pattern: '(oops' }, () => { fired = true; });
          |  return fired; })()""".stripMargin) shouldBe false
    }
  }

  private def jsLit(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  "window.TitleRules.moveBetween" should "reorder within one list (the same-list drag decision)" in {
    onEditor { page =>
      val res = page.evalString(
        """(() => { const rec = { rules: [{id:'a'},{id:'b'},{id:'c'}], lastRules: [] };
          |  window.TitleRules.moveBetween(rec, 'rules', 0, 'rules', 2);
          |  return JSON.stringify(rec.rules.map(x => x.id)); })()""".stripMargin)
      res shouldBe """["b","c","a"]"""
    }
  }

  it should "move a rule across to the Last list (the cross-list drag that replaces the checkbox)" in {
    onEditor { page =>
      val res = page.evalString(
        """(() => { const rec = { rules: [{id:'x'},{id:'y'}], lastRules: [] };
          |  window.TitleRules.moveBetween(rec, 'rules', 0, 'lastRules', rec.lastRules.length);
          |  return JSON.stringify([rec.rules.map(r=>r.id), rec.lastRules.map(r=>r.id)]); })()""".stripMargin)
      res shouldBe """[["y"],["x"]]"""
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
