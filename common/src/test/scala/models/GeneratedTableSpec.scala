package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GeneratedTableSpec extends AnyFlatSpec with Matchers {

  "a generated table" should "read every row of its resource, tab-separated, with empty cells kept" in {
    val venueTowns = GeneratedTable.rows("generated/venue-towns.tsv")
    venueTowns should not be empty
    all(venueTowns.map(_.size)) shouldBe 2
    GeneratedTable.lookup("generated/venue-towns.tsv") should have size venueTowns.size.toLong
  }

  it should "fail loudly when its resource is not on the classpath" in {
    the[IllegalStateException] thrownBy GeneratedTable.rows("generated/no-such-table.tsv") should have message
      "generated table generated/no-such-table.tsv is not on the classpath"
  }

  "a list cell" should "split on the list separator, and an empty one be no items" in {
    GeneratedTable.list("Bonn|Siegburg") shouldBe Seq("Bonn", "Siegburg")
    GeneratedTable.list("") shouldBe empty
  }

  "an optional cell" should "be None when empty" in {
    GeneratedTable.option("") shouldBe None
    GeneratedTable.option("x") shouldBe Some("x")
  }

  "the slug index" should "answer every country's aliases from its table" in {
    CitySlugIndexData.aliases("uk").get("ilkley") shouldBe defined
    CitySlugIndexData.sharedPageQualifiedSlugs("us").get("york") shouldBe Some("york-pennsylvania")
  }
}
