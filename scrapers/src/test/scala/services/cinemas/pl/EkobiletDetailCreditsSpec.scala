package services.cinemas.pl

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where an ekobilet detail panel's credits live: a metadata line's labels and a credits paragraph of its own are
 *  read together, so a panel splitting them (director on the metadata line, cast below) keeps both. The panel is
 *  Kino Rejs's recorded "Erupcja" (`EkobiletClientSpec`) with its cast moved to a paragraph of its own. */
class EkobiletDetailCreditsSpec extends AnyFlatSpec with Matchers {

  private def panel(paragraphs: String*) = Jsoup.parse(
    s"""<div id="offcanvasRightInfo"><div class="offcanvas-body">${paragraphs.map(p => s"<p>$p</p>").mkString}</div></div>""")

  "an ekobilet detail" should "keep the metadata line's director when the cast is a credits paragraph of its own" in {
    val detail = EkobiletClient.parseDetail(panel(
      "Jest upalne warszawskie lato.",
      "USA, Polska 2025, 71 min reżyseria: Pete Ohs",
      "obsada: Lena Góra, Charli XCX"))
    detail.director shouldBe Seq("Pete Ohs")
    detail.cast     shouldBe Seq("Lena Góra", "Charli XCX")
  }

  it should "prefer the credits paragraph's director over the metadata line's" in {
    val detail = EkobiletClient.parseDetail(panel(
      "USA 2025, 71 min reżyseria: Pete",
      "reżyseria: Pete Ohs obsada: Lena Góra"))
    detail.director shouldBe Seq("Pete Ohs")
  }
}
