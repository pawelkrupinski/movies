package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.ImdbClient

class ImdbClientSpec extends AnyFlatSpec with Matchers {

  private val client = new ImdbClient()

  "parseRating" should "extract the aggregateRating from the GraphQL response" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.0,"voteCount":16966}}}}"""
    client.parseRating(body) shouldBe Some(7.0)
  }

  it should "treat a missing ratingsSummary as None" in {
    val body = """{"data":{"title":null}}"""
    client.parseRating(body) shouldBe None
  }

  it should "treat a zero rating as None (IMDb returns 0 when unrated)" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":0,"voteCount":0}}}}"""
    client.parseRating(body) shouldBe None
  }

  // Mirror TMDB's "suppress single-enthusiast" guard: <5 votes is noise.
  it should "suppress the rating when there are fewer than MinVotes" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":10.0,"voteCount":3}}}}"""
    client.parseRating(body) shouldBe None
  }

  it should "accept integer aggregateRating values (GraphQL drops the .0)" in {
    val body = """{"data":{"title":{"ratingsSummary":{"aggregateRating":7,"voteCount":1000}}}}"""
    client.parseRating(body) shouldBe Some(7.0)
  }

  it should "return None for an empty / malformed response" in {
    client.parseRating("""{}""") shouldBe None
  }
}
