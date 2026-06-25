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

  "planChunks" should "group screening dates into week-sized chunks, not one per date" in {
    val plaza = scraper("1078", CinemaCityPoznanPlaza)
    val dates = client.dates("1078")
    dates.size should be > CinemaCityScraper.DaysPerChunk // fixture spans more than one chunk

    val keys = plaza.planChunks()
    keys.size shouldBe math.ceil(dates.size.toDouble / CinemaCityScraper.DaysPerChunk).toInt
    keys.size should be < dates.size                      // fewer tasks than per-date
    // Each key packs up to a week of dates; together they cover every date once.
    keys.foreach(_.split(",").length should be <= CinemaCityScraper.DaysPerChunk)
    keys.flatMap(_.split(",")).toList shouldBe dates.map(_.toString).toList
  }

  "fetchChunk" should "fetch every day in a week chunk" in {
    val plaza = scraper("1078", CinemaCityPoznanPlaza)
    // The whole repertoire reached via fetchChunk over all planned chunks equals
    // the whole-venue fetch() — i.e. chunking by week loses nothing.
    val viaChunks = plaza.planChunks().flatMap(plaza.fetchChunk)
    viaChunks.flatMap(_.externalIds.get("cc")).toSet shouldBe plaza.fetch().flatMap(_.externalIds.get("cc")).toSet
  }
}
