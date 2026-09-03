package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/**
 * Every shareable page names a committed Open Graph card, and this pins that
 * the named file actually EXISTS — for every country, not just the one whose
 * cards someone remembered to generate.
 *
 * The city index emits `og-{slug}.jpg` and the `/` landing emits
 * [[Country.homeOgImage]], both as absolute prod URLs (see `_ogTagsApp`). A
 * country whose cards were never generated therefore doesn't degrade to a
 * generic card — it points Facebook, Messenger, Slack and X at a 404 and the
 * link preview comes back with no image at all. That is exactly what Germany
 * (158 regions) and the United States (55 states + their metros) shipped with:
 * `og-*.png` covered Poland and the UK alone, because the weekly refresh
 * workflow only ever ran those two legs, and nothing failed when the other two
 * countries went live.
 *
 * A city added by a commit cannot have its card in that same commit: the
 * generator SCREENSHOTS the live page, and the new city's page does not exist
 * upstream until the commit creating it has deployed. So a roster change that
 * adds cities lands red here, and the card sweep follows it — dispatch
 * `regenerate-og-cards.yml` once the deploy is out, or generate the few by hand
 * with `KINOWO_COUNTRY=<code> sbt "web/PageTest/runMain tools.OgCardGenerator
 * <slug>…"`, which takes about five seconds a card against prod.
 *
 * Runs off the filenames rather than the rendered HTML on purpose: the page
 * specs (`RepertoirePreviewMetaSpec`, `LandingPreviewMetaSpec`) already pin the
 * URL a page emits, so what is left to prove is that the other end of that URL
 * is on disk — cheap to check for all 739 cards, where rendering 739 pages
 * would not be.
 */
class OgCardAssetsSpec extends AnyFlatSpec with Matchers {

  private val cards: File = testsupport.RepoRoot.file("web/src/main/assets/img")

  private def missing(names: Seq[String]): Seq[String] = names.filterNot(new File(cards, _).exists())

  "every country" should "have the landing card its `/` page names" in {
    missing(Country.all.map(_.homeOgImage)) shouldBe empty
  }

  "every city, in every country" should "have the card its index page names" in {
    val absent = missing(Country.all.flatMap(_.cities).map(_.shareImage))
    // Only the first few names, or a country that was never swept prints its
    // whole roster — 546 filenames on one assertion line, in the run that
    // introduced this spec.
    withClue(s"${absent.size} cities have no committed share card; first: ") {
      absent.take(8) shouldBe empty
    }
  }

  "the cards" should "be the JPEGs the pages name, with no PNG left behind" in {
    // The cards were PNG until the sweep grew from 122 to 739 of them: at
    // ~810 KB each, and rewritten by every weekly refresh, PNG cost ~600 MB a
    // run against JPEG's 77 MB for the same pixels. A leftover `og-*.png` is
    // dead weight nothing serves.
    val strays = Option(cards.listFiles((_, n) => n.startsWith("og-") && n.endsWith(".png"))).toSeq.flatten.map(_.getName)
    withClue(s"${strays.size} stale PNG cards; first: ") { strays.take(8) shouldBe empty }
  }
}
