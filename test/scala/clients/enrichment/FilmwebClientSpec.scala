package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.FilmwebClient

class FilmwebClientSpec extends AnyFlatSpec with Matchers {

  private val client = new FilmwebClient()

  "parseSearch" should "extract film-type ids in the order Filmweb returned them" in {
    val json =
      """{
        |  "total": 3,
        |  "searchHits": [
        |    {"id": 10058855, "type": "film", "matchedTitle": "Sentimental Value"},
        |    {"id": 2559,     "type": "person", "matchedTitle": "Stellan Skarsgård"},
        |    {"id": 838929,   "type": "film", "matchedTitle": "Wartość sentymentalna"}
        |  ]
        |}""".stripMargin
    client.parseSearch(json) shouldBe Seq(10058855, 838929)
  }

  it should "return empty when search has no hits" in {
    client.parseSearch("""{"total":0,"searchHits":[]}""") shouldBe empty
  }

  it should "skip entries missing an id" in {
    val json = """{"searchHits":[{"type":"film","matchedTitle":"orphan"}]}"""
    client.parseSearch(json) shouldBe empty
  }

  "parseInfo" should "pull title + year from the film info response" in {
    val json =
      """{
        |  "title": "Wartość sentymentalna",
        |  "originalTitle": "Affeksjonsverdi",
        |  "year": 2025,
        |  "type": "film",
        |  "subType": "film_cinema",
        |  "posterPath": "/88/55/10058855/8228146.$.jpg"
        |}""".stripMargin
    val info = client.parseInfo(json).get
    info.title shouldBe "Wartość sentymentalna"
    info.year  shouldBe Some(2025)
  }

  it should "return None when title is missing" in {
    client.parseInfo("""{"year":2025}""") shouldBe None
  }

  "parseRating" should "extract the 1-10 rate value" in {
    val json = """{"count": 26186, "rate": 7.5034, "countWantToSee": 29885}"""
    client.parseRating(json) shouldBe Some(7.5034)
  }

  it should "return None when rate is missing" in {
    client.parseRating("""{"count": 0}""") shouldBe None
  }
}
