package clients.tmdb

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json
import clients.TmdbClient

/**
 * `TmdbClient.certificationFor` — picks the DEPLOYMENT country's age-rating out of
 * a `/movie/{id}/release_dates` body. Verbatim per country (a GB "12A" ≠ a DE "16"),
 * first non-blank certification for that country, None when the country is unlisted
 * or blank. This is the per-country fallback beneath cinema-scraped ratings.
 */
class TmdbCertificationSpec extends AnyFlatSpec with Matchers {

  private val body = Json.parse(
    """{"results":[
      | {"iso_3166_1":"GB","release_dates":[{"certification":"12A","type":3},{"certification":"12","type":5}]},
      | {"iso_3166_1":"DE","release_dates":[{"certification":"16","type":3}]},
      | {"iso_3166_1":"PL","release_dates":[{"certification":"","type":3},{"certification":"16","type":6}]},
      | {"iso_3166_1":"US","release_dates":[{"certification":"PG-13","type":3}]},
      | {"iso_3166_1":"FR","release_dates":[{"certification":"","type":3}]}
      |]}""".stripMargin)

  "certificationFor" should "return each country's own certification verbatim" in {
    TmdbClient.certificationFor(body, "GB") shouldBe Some("12A")  // first non-blank (theatrical) wins
    TmdbClient.certificationFor(body, "DE") shouldBe Some("16")
    TmdbClient.certificationFor(body, "US") shouldBe Some("PG-13")
  }

  it should "skip a blank certification and take the next non-blank for that country" in {
    TmdbClient.certificationFor(body, "PL") shouldBe Some("16")   // first PL entry is ""
  }

  it should "be None for a country with only blank certs, or not listed at all" in {
    TmdbClient.certificationFor(body, "FR") shouldBe None
    TmdbClient.certificationFor(body, "JP") shouldBe None
    TmdbClient.certificationFor(Json.parse("""{"results":[]}"""), "GB") shouldBe None
  }
}
