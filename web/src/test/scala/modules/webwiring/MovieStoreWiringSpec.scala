package modules.webwiring

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titles.TitleNormalizer
import tools.MutableClock

/** The web tier reads every country's corpus through one wiring: the boot country's mirror and each /debug extra get alike-built stores. */
class MovieStoreWiringSpec extends AnyFlatSpec with Matchers {

  private val wiring = new MovieStoreWiring(new MutableClock(java.time.Instant.parse("2026-01-01T00:00:00Z")), _root_.tools.SpecStopwatch.Frozen)

  "MovieStoreWiring.readOnly" should "key the repository by the normalizer it is handed, one per country" in {
    val poland  = TitleNormalizer.forCountry(Country.Poland)
    val germany = TitleNormalizer.forCountry(Country.Germany)
    wiring.readOnly(None, poland).repository.normalizer should be theSameInstanceAs poland
    wiring.readOnly(None, germany).repository.normalizer should be theSameInstanceAs germany
  }

  it should "read an unconfigured database as an empty corpus through every store" in {
    val stores = wiring.readOnly(None, TitleNormalizer.forCountry(Country.Poland))
    stores.repository.findAll() shouldBe empty
    stores.listingReader.findAllForListing() shouldBe empty
  }
}
