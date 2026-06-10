package services.cinemas

import clients.tools.FakeHttpFetch
import models.{CinemaCityChain, CinemaCityKinepolis, CinemaCityPoznanPlaza}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Cinema City's per-film detail is fetched once per network, not per venue: all
 *  venues share one `detailGroup` (so the queue + freshness drop sibling
 *  duplicates), write detail into the single `CinemaCityChain` source, and
 *  report enrichment health under one global name. */
class CinemaCityScraperSpec extends AnyFlatSpec with Matchers {

  private val client = new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza"))
  private def scraper(id: String, c: models.Cinema) = new CinemaCityScraper(client, id, c)

  "Every Cinema City venue" should "share one network-level detail group, source, and uptime name" in {
    val plaza     = scraper("1078", CinemaCityPoznanPlaza)
    val kinepolis = scraper("1081", CinemaCityKinepolis)

    plaza.detailGroup shouldBe "cinema-city"
    plaza.detailGroup shouldBe kinepolis.detailGroup            // one fetch per network, not per venue
    plaza.detailTarget shouldBe CinemaCityChain                 // detail written to the shared source
    kinepolis.detailTarget shouldBe CinemaCityChain
    plaza.enrichmentServiceOverride shouldBe Some("Cinema City Enrichment")
  }
}
