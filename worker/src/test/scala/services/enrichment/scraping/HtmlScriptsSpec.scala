package services.enrichment.scraping

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** [[HtmlScripts]] is the script-element regex the rating readers used, found by `indexOf` instead:
 *  it must match exactly what that regex matched, and do it in one pass over the page. */
class HtmlScriptsSpec extends AnyFlatSpec with Matchers {

  /** What the readers matched before, kept as the reference. */
  private val Reference = """(?is)<script\b([^>]*)>(.*?)</script\s*>""".r
  private def byReference(html: String): Seq[HtmlScripts.Script] =
    Reference.findAllMatchIn(html).map(m => HtmlScripts.Script(m.group(1), m.group(2))).toSeq
  private def scanned(html: String): Seq[HtmlScripts.Script] = HtmlScripts.all(html).toSeq

  "the script scan" should "read every script element's attributes and body, in page order" in {
    val html = """<p>x</p><script>a</script><SCRIPT type="t">b</ScRiPt ><script
                 |id='i'>c</script
                 |>tail""".stripMargin
    scanned(html) shouldBe Seq(HtmlScripts.Script("", "a"), HtmlScripts.Script(""" type="t"""", "b"), HtmlScripts.Script("\nid='i'", "c"))
  }

  it should "match the regex on its edge cases" in {
    Seq(
      "", "<", "<script", "<script>", "<script>no close", "<scripts>x</script>", "<script_>x</script>", "<script1>x</script>",
      "<script-x>y</script>", "<script>a</scriptx>b</script>", "<script>a</script/>b</script>", "<script>a</ script>b</script>",
      "<script>a<script>b</script>c</script>", "<script a='>'>b</script>", "<<script>>x<</script>>", "<script>x</script\t\r\n>",
      "<script>x</script >y</script>", "<scrİpt>x</script>", "<ſcript>x</script>", "<script>x</ſcript>y</SCRIPT>",
      "text<script>1</script>text<script>2</script>", "<script/>x</script>", "<script\n>x</script>"
    ).foreach(html => withClue(s"[$html] ")(scanned(html) shouldBe byReference(html)))
  }

  it should "match the regex on random pages built from its own pieces" in {
    val pieces = Seq("<", ">", "/", "script", "SCRIPT", "Script", " ", "\n", "\t", "a", "_", "1", "-", "'", "\"", "</script>", "<script>",
      "</script ", "type=x", " ")
    val random = new Random(20261001L)
    (1 to 5000).foreach { _ =>
      val html = Seq.fill(random.nextInt(24))(pieces(random.nextInt(pieces.size))).mkString
      withClue(s"[$html] ")(scanned(html) shouldBe byReference(html))
    }
  }

  // The regex's lazy body tried its closing at every character after every opening: a page whose
  // openings never close cost each opening a scan to the end of the page, so 30,000 of them took tens
  // of seconds. One pass over 300 kB is milliseconds; the bound leaves two orders of magnitude.
  it should "read a page whose scripts never close in one pass, not one pass per opening" in {
    val html    = "<script>x " * 30000
    val started = System.nanoTime()
    JsonLdAggregateRating.scripts(html) shouldBe empty
    RottenTomatoesScorecard.criticsScore(html) shouldBe None
    (System.nanoTime() - started) / 1000000 should be < 1000L
  }
}
