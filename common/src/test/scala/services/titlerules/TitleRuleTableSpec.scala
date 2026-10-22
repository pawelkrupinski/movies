package services.titlerules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The title-rule table's reading: each kind's mechanism, the table's order as the fold order, and a malformed row
 *  failing loudly instead of dropping a rule. What the rules DO is [[TitleRulesGoldenSpec]]'s. */
class TitleRuleTableSpec extends AnyFlatSpec with Matchers {

  private def parse(rows: String*) = TitleRuleTable.parse(rows.iterator)
  private val Ex = """[["Lalka - napisy","Lalka"]]"""

  "a row's kind" should "pick the tier, the banner tag and the proper-name mark" in {
    val Seq(banner, query, key, name, venue) = parse(
      "b\tbanner\t\t\t\tbanner\t\t^Kino Konesera:\\s+\t\t\t" + Ex,
      "q\tquery\t\t\t\tevent\t\t\\s+premiera$\t\t\t" + Ex,
      "k\tkey\t\tpl\tall\trename\t\t & \t i \tPolish 'and'\t" + Ex,
      "n\tname\t\t\t\trelay\t\t^NT Live:\tNational Theatre Live:\t\t" + Ex,
      "v\tvenue\thelios\t\t\tformat\t\t - napisy$\t\t\t" + Ex).map(_.rule): @unchecked
    banner.scope shouldBe RuleScope.GlobalStructural
    banner.tag shouldBe Some("programmePrefix")
    query.scope shouldBe RuleScope.GlobalStructural
    query.tag shouldBe None
    key.scope shouldBe RuleScope.Canonical
    key.countries shouldBe Some(Set(Country.Poland))
    key.applyAll shouldBe true
    key.replacement shouldBe " i "
    key.note shouldBe Some("Polish 'and'")
    name.writesProperName shouldBe true
    venue.scope shouldBe RuleScope.PerCinema
    venue.cinemaId shouldBe Some("helios")
  }

  "the table's order" should "be the order each tier folds its rows in" in {
    val rules = parse(
      "# a comment and a blank line between rows are not rows",
      "first\tquery\t\t\t\tjunk\t\t\\s+/.*$\t\t\t" + Ex,
      "",
      "second\tquery\t\t\t\tjunk\t\t\\s+-.*$\t\t\t" + Ex).map(_.rule)
    rules.map(_.order) shouldBe Seq(0, 1)
    TitleRuleSet(rules).firedSearch("Lalka - B / C") shouldBe Seq("first", "second")
  }

  it should "keep a seasonal row in force out of its season" in {
    val Seq(row) = parse("lato\tbanner\t\t\t\tbanner\tjun-sep\t^Lato w kinie:\\s+\t\t\t" + Ex): @unchecked
    row.season shouldBe Some("jun-sep")
    TitleRuleSet(Seq(row.rule)).structural("Lato w kinie: Lalka") shouldBe "Lalka"
  }

  "a malformed row" should "fail with its line" in {
    an[IllegalArgumentException] should be thrownBy parse("x\tstrip\t\t\t\tjunk\t\t.")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tnoise\t\t.")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\tfirst\tjunk\t\t.")
    an[IllegalArgumentException] should be thrownBy parse("x\tvenue\t\t\t\tjunk\t\t.")
    an[IllegalArgumentException] should be thrownBy parse("x\tkey\t\tpolska\t\tjunk\t\t.")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tjunk\tsummer\t.")
  }

  "a row without an example" should "fail with its line" in {
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tjunk\t\t\\s+/.*$")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tjunk\t\t\\s+/.*$\t\t\t[]")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tjunk\t\t\\s+/.*$\t\t\tLalka -> Lalka")
    an[IllegalArgumentException] should be thrownBy parse("x\tquery\t\t\t\tjunk\t\t\\s+/.*$\t\t\t[[\"Lalka\"]]")
    parse("x\tquery\t\t\t\tjunk\t\t\\s+/.*$\t\t\t" + Ex).head.examples shouldBe Seq("Lalka - napisy" -> "Lalka")
  }

  "the shipped table" should "load, every id once" in {
    val ids = TitleRuleTable.all.map(_.id)
    ids should not be empty
    ids.distinct shouldBe ids
  }
}
