package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CitySpec extends AnyFlatSpec with Matchers {

  "City.bySlug" should "resolve known slugs and reject an unknown one" in {
    City.bySlug("poznan") shouldBe Some(Poznan)
    City.bySlug("wroclaw") shouldBe Some(Wroclaw)
    City.bySlug("warszawa") shouldBe Some(Warszawa)
    City.bySlug("krakow") shouldBe Some(Krakow)
    City.bySlug("trojmiasto") shouldBe Some(Trojmiasto)
    City.bySlug("gliwice") shouldBe None
    City.bySlug("") shouldBe None
  }

  "Each city" should "scope to its own cinemas (disjoint partition of the universe)" in {
    Poznan.cinemas shouldBe Cinema.poznan
    Wroclaw.cinemas shouldBe Cinema.wroclaw
    Warszawa.cinemas shouldBe Cinema.warszawa
    // The cities partition the global cinema universe with no overlap.
    City.all.flatMap(_.cinemas) should contain theSameElementsAs Cinema.all
    City.all.flatMap(_.cinemas).distinct.size shouldBe Cinema.all.size
  }

  it should "carry the Polish label inflections the templates render" in {
    Poznan.labels.nominative shouldBe "Poznań"
    Poznan.labels.genitivePlural shouldBe "poznańskich"
    Poznan.labels.locative shouldBe "Poznaniu"
  }

  "City.allJson" should "emit a slug/name/lat/lon object per city for the clients" in {
    val json = City.allJson
    json should include(""""slug":"poznan"""")
    json should include(""""name":"Poznań"""")
    json should include(""""lat":52.4064""")
    json should include(""""lon":16.9252""")
    json should startWith("[")
    json should endWith("]")
  }
}
