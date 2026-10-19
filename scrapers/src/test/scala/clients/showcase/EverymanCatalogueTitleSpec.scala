package clients.showcase

import clients.tools.FixtureFile
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.GatsbyBoxOfficeParser

/**
 * Everyman's live `allMovie` catalogue (`GET https://www.everymancinema.com/page-data/sq/d/3836549025.json`,
 * recorded 2026-09-27, trimmed to three nodes) bills film 1000051611 with a `title` its CMS lost the
 * closing parenthesis from — "Throwback: Casino Royale (20th Anniversary" — while the same node's
 * `originalTitle` still reads "…(20th Anniversary)". The scraper must not carry the dangling "(" into
 * the card or the enrichment query.
 */
class EverymanCatalogueTitleSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val catalogue = GatsbyBoxOfficeParser.parseCatalogue(
    FixtureFile.read("test/resources/fixtures/everyman/catalogue-unclosed-anniversary-paren.json"))

  "parseCatalogue" should "close a parenthetical the upstream title left open" in {
    val casinoRoyale = catalogue.get("1000051611").value
    casinoRoyale.title shouldBe "Throwback: Casino Royale (20th Anniversary)"
    // Now the same string as `originalTitle`, so no duplicate hint rides along.
    casinoRoyale.originalTitle shouldBe None
  }

  it should "leave balanced and parenthesis-free titles as billed" in {
    catalogue.values.map(_.title).toSet should contain allOf (
      "Throwback: Donnie Darko (25th Anniversary)",
      "Royal Ballet and Opera: Romeo and Juliet"
    )
  }
}
