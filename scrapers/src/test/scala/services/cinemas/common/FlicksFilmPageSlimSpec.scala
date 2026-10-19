package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

/** The parts of a Flicks film page [[FlicksFilmPage.slimmed]] keeps must be all the parse reads: every recorded
 *  film page reads the same slimmed as whole. */
class FlicksFilmPageSlimSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 10, 6)

  // Every film page a US or UK detail phase reads went through a whole-document parse, and the parse was 42% of the
  // detail drain's CPU (JFR, run 37619819275): a page is ~350 KB, of which the read needs the head, the hero and the
  // schema.org block — the rest is the showtimes tabs. Slimmed to those, it must read exactly what the whole page does.
  "FlicksFilmPage.slimmed" should "keep everything the parse reads, off every recorded film page" in {
    val pages = Seq("www.flicks.us", "www.flicks.co.uk").flatMap { host =>
      val dir = java.nio.file.Path.of(s"test/resources/fixtures/flicks/$host/movie")
      scala.jdk.CollectionConverters.IteratorHasAsScala(java.nio.file.Files.list(dir).iterator).asScala.toSeq.sortBy(_.toString)
    }
    pages.size should be >= 7
    pages.foreach { path =>
      val html = clients.tools.FixtureFile.read(path.toString)
      val slug = path.getFileName.toString.stripSuffix(".html")
      withClue(slug)(FlicksFilmPage.parse(html, slug, today) shouldBe FlicksFilmPage.parseDocument(org.jsoup.Jsoup.parse(html), slug, today))
    }
  }

  // The positive control: a slimming that kept the whole page would pass the test above too.
  it should "drop the showtimes tabs, and leave a page without the tabs mark whole" in {
    val html = clients.tools.FixtureFile.read("test/resources/fixtures/flicks/www.flicks.us/movie/a-night-at-the-opera.html")
    val slim = FlicksFilmPage.slimmed(html)
    slim.length.toDouble / html.length should be < 0.2
    slim should include ("application/ld+json")
    val unknown = "<html><body><div class=\"elsewhere\">x</div></body></html>"
    FlicksFilmPage.slimmed(unknown) shouldBe unknown
  }

  // Markup the recorded pages do not show, read the same slimmed as whole all the same: a page whose tabs come before
  // its hero is kept whole (the cut would drop the hero), and a schema.org type named outside a script tag — inline JS
  // selecting the block — keeps each script once, in order, never text twice.
  it should "read unusual markup the same slimmed as whole" in {
    val block = """<script type="application/ld+json">{"@type":"Movie","name":"Odd","dateCreated":"1999-01-01"}</script>"""
    val hero  = """<div class="movie-hero-v6__title"><h1>Odd</h1></div><div class="movie-hero-v6__meta"><span>1999</span><span>101mins</span></div>"""
    val tabsFirst = s"""<html><head></head><body><div id="movie-tabs">sessions</div>$hero$block</body></html>"""
    val inlineJs  = s"""<html><head></head><body>$hero<div id="movie-tabs">sessions</div>""" +
      """<script>document.querySelector('script[type="application/ld+json"]')</script>""" + block +
      """<script>var t = "application/ld+json"; var u = "application/ld+json";</script></body></html>"""
    Seq("tabs first" -> tabsFirst, "inline js" -> inlineJs).foreach { case (name, html) =>
      withClue(name)(FlicksFilmPage.parse(html, "odd", today) shouldBe FlicksFilmPage.parseDocument(org.jsoup.Jsoup.parse(html), "odd", today))
    }
    FlicksFilmPage.slimmed(tabsFirst) shouldBe tabsFirst
    "application/ld\\+json\">".r.findAllMatchIn(FlicksFilmPage.slimmed(inlineJs)).size shouldBe 1
  }
}
