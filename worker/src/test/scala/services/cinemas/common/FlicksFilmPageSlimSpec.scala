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
  it should "drop the showtimes tabs, and leave a page it cannot find the hero's end in whole" in {
    val html = clients.tools.FixtureFile.read("test/resources/fixtures/flicks/www.flicks.us/movie/a-night-at-the-opera.html")
    val slim = FlicksFilmPage.slimmed(html)
    slim.length.toDouble / html.length should be < 0.2
    slim should include ("application/ld+json")
    val unknown = "<html><body><div class=\"elsewhere\">x</div></body></html>"
    FlicksFilmPage.slimmed(unknown) shouldBe unknown
  }
}
