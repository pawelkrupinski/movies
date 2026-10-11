package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

/**
 * Every country's `/` landing names a committed Open Graph card, [[Country.homeOgImage]], as an
 * absolute prod URL (see `_ogTagsApp`), and this pins that the named file actually EXISTS — for
 * every country, not just the one whose card someone remembered to generate. A missing one points
 * Facebook, Messenger, Slack and X at a 404, and the preview comes back with no image at all. The
 * home card is also what a city with no card of its own yet falls back to.
 *
 * City cards are not committed: the worker renders each one into its share-card directory
 * (`services.sharecards.CityCardService`, pinned by `CityCardServiceSpec`).
 */
class OgCardAssetsSpec extends AnyFlatSpec with Matchers {

  private val cards: File = testsupport.RepoRoot.file("web/src/main/assets/img")

  private def missing(names: Seq[String]): Seq[String] = names.filterNot(new File(cards, _).exists())

  "every country" should "have the landing card its `/` page names" in {
    missing(Country.all.map(_.homeOgImage)) shouldBe empty
  }

  "the cards" should "be the JPEGs the pages name, with no PNG left behind" in {
    // The cards were PNG until the sweep grew from 122 to 739 of them: at ~810 KB each, PNG cost
    // ~600 MB a run against JPEG's 77 MB for the same pixels. A leftover `og-*.png` is dead weight
    // nothing serves.
    val strays = Option(cards.listFiles((_, n) => n.startsWith("og-") && n.endsWith(".png"))).toSeq.flatten.map(_.getName)
    withClue(s"${strays.size} stale PNG cards; first: ") { strays.take(8) shouldBe empty }
  }

  "a city card" should "not be committed: the worker renders them, and nothing serves an `og-<city>.jpg` asset" in {
    val homes  = Country.all.map(_.homeOgImage).toSet
    val strays = Option(cards.listFiles((_, n) => n.startsWith("og-") && n.endsWith(".jpg") && !homes(n))).toSeq.flatten.map(_.getName)
    withClue(s"${strays.size} committed city cards; first: ") { strays.sorted.take(8) shouldBe empty }
  }
}
