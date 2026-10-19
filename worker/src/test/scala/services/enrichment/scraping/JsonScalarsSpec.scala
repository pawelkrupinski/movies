package services.enrichment.scraping

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

/** The JSON scalar reads every scraper shares: a string field trimmed and absent when blank, a score
 *  published as a string or a number. */
class JsonScalarsSpec extends AnyFlatSpec with Matchers {

  private val js = Json.obj("title" -> "  Dune ", "blank" -> "   ", "count" -> 3, "score" -> "94")

  "text" should "trim a string field, and give nothing for a missing, blank or non-string one" in {
    JsonScalars.text(js \ "title") shouldBe Some("Dune")
    JsonScalars.text(js \ "blank") shouldBe None
    JsonScalars.text(js \ "missing") shouldBe None
    JsonScalars.text(js \ "count") shouldBe None
    JsonScalars.text(Json.toJson(" x ")) shouldBe Some("x")
  }

  "intValue" should "read a score published as a string or as a number" in {
    JsonScalars.intValue((js \ "score").get) shouldBe Some(94)
    JsonScalars.intValue((js \ "count").get) shouldBe Some(3)
    JsonScalars.intValue((js \ "title").get) shouldBe None
  }
}
