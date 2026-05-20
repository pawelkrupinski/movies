package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MinifySpec extends AnyFlatSpec with Matchers {

  // ── JS: comment + string safety ──────────────────────────────────────────

  "minifyJs" should "strip line comments outside strings" in {
    Minify.minifyJs("var x = 1; // trailing\nvar y = 2;") shouldBe "var x = 1;\nvar y = 2;"
  }

  it should "strip block comments outside strings" in {
    Minify.minifyJs("var x /* inline */ = 1;") shouldBe "var x = 1;"
  }

  it should "keep // sequences that live inside string literals" in {
    val src = """var u = "https://example.com/path"; // real comment"""
    Minify.minifyJs(src) shouldBe """var u = "https://example.com/path";"""
  }

  it should "keep block-comment-looking sequences inside string literals" in {
    val src = """var s = "before /* still string */ after"; // c"""
    Minify.minifyJs(src) shouldBe """var s = "before /* still string */ after";"""
  }

  it should "keep // inside single-quoted strings" in {
    Minify.minifyJs("""var p = 'path=/; SameSite=Lax'; // x""") shouldBe
      """var p = 'path=/; SameSite=Lax';"""
  }

  it should "preserve newlines so ASI keeps working" in {
    // `return\n{…}` MUST stay split — ASI turns it into `return; {…}`.
    // Collapsing to one line would change the semantics.
    Minify.minifyJs("function f() {\n  return\n  { a: 1 }\n}") should include ("return\n")
  }

  it should "collapse multi-blank lines + indent runs" in {
    val src =
      """
        |function f() {
        |
        |
        |    var a = 1;
        |
        |    var b = 2;
        |}
        |""".stripMargin
    val out = Minify.minifyJs(src)
    out should not include "\n\n"
    // Leading indentation is dropped as part of the whitespace collapse.
    out should include ("function f() {\nvar a = 1;\nvar b = 2;\n}")
  }

  it should "not strip regex literals" in {
    Minify.minifyJs("var r = /^\\d{4}$/.test(s);") shouldBe "var r = /^\\d{4}$/.test(s);"
  }

  // ── CSS ──────────────────────────────────────────────────────────────────

  "minifyCss" should "strip block comments and collapse whitespace" in {
    Minify.minifyCss("/* hi */  .a  {  color:  red;  }") shouldBe ".a{color:red}"
  }

  it should "drop the last semicolon before a closing brace" in {
    Minify.minifyCss(".a { color: red; }") shouldBe ".a{color:red}"
  }

  it should "preserve descendant combinator spaces inside selectors" in {
    Minify.minifyCss(".parent .child { color: red; }") shouldBe ".parent .child{color:red}"
  }

  it should "preserve spaces between values inside a declaration" in {
    Minify.minifyCss(".a { border: 1px solid #fff; }") shouldBe ".a{border:1px solid #fff}"
  }

  it should "tighten media queries" in {
    Minify.minifyCss("@media (max-width: 575px) { .a { color: red; } }") shouldBe
      "@media (max-width:575px){.a{color:red}}"
  }

  // ── process: minify only <script> + <style> blocks, keep markup ─────────

  "process" should "minify <script> contents and leave surrounding markup alone" in {
    val html = "<div>before</div>\n<script>\n  // c\n  var x = 1;\n</script>\n<p>after</p>"
    val out = Minify.process(html)
    out should startWith ("<div>before</div>\n<script>")
    out should endWith ("</script>\n<p>after</p>")
    out should not include "// c"
  }

  it should "minify <style> contents and keep the attributes on the opening tag" in {
    val html = """<style type="text/css">
                 |  /* boom */
                 |  .a { color: red; }
                 |</style>""".stripMargin
    val out = Minify.process(html)
    out should startWith ("""<style type="text/css">""")
    out should include (".a{color:red}")
    out should not include "/* boom */"
  }

  it should "be idempotent" in {
    val html = "<script>var x = 1; // c</script><style>.a { color: red; }</style>"
    val once  = Minify.process(html)
    val twice = Minify.process(once)
    twice shouldBe once
  }
}
