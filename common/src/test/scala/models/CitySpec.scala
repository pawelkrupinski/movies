package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CitySpec extends AnyFlatSpec with Matchers {

  "City.bySlug" should "resolve known slugs and reject an unknown one" in {
    City.bySlug("poznan") shouldBe Some(Poznan)
    City.bySlug("wroclaw") shouldBe Some(Wroclaw)
    City.bySlug("warszawa") shouldBe Some(Warszawa)
    City.bySlug("krakow") shouldBe Some(Krakow)
    City.bySlug("lodz") shouldBe Some(Lodz)
    City.bySlug("katowice") shouldBe Some(Katowice)
    City.bySlug("szczecin") shouldBe Some(Szczecin)
    City.bySlug("trojmiasto") shouldBe Some(Trojmiasto)
    City.bySlug("gliwice") shouldBe Some(Gliwice)
    City.bySlug("rzeszow") shouldBe Some(Rzeszow)
    City.bySlug("sopot") shouldBe None
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

  "City.locativePhrase" should "pick the right Polish preposition for the share-card line" in {
    // Plain "w" before a vowel-initial or non-W/F locative.
    Poznan.locativePhrase shouldBe "w Poznaniu"
    Warszawa.locativePhrase shouldBe "w Warszawie"   // W + vowel → still "w"
    Krakow.locativePhrase shouldBe "w Krakowie"
    Walbrzych.locativePhrase shouldBe "w Wałbrzychu" // W + vowel → "w"
    // "we" before a W/F + consonant cluster.
    Wroclaw.locativePhrase shouldBe "we Wrocławiu"   // W + r
    Wloclawek.locativePhrase shouldBe "we Włocławku" // W + ł
  }

  "City.allSorted" should "list every city alphabetically under Polish collation" in {
    // Same cities as `all`, just reordered — nothing dropped or duplicated.
    City.allSorted should contain theSameElementsAs City.all

    // The Polish cities keep their exact Polish-collation order regardless of the
    // foreign (UK/DE) cities now interleaved among them by their own names.
    City.allSorted.filter(City.polishCities.contains).map(_.slug) shouldBe Seq(
      "bialystok", "bielsko-biala", "bydgoszcz", "bytom", "czestochowa",
      "dabrowa-gornicza", "elblag", "gliwice", "gorzow-wielkopolski", "jelenia-gora",
      "kalisz", "katowice", "kielce", "konin", "koszalin", "krakow",
      "legnica", "lublin", "lodz", "nowy-sacz", "olsztyn", "opole",
      "plock", "poznan", "przemysl", "radom", "rybnik", "rzeszow",
      "slupsk", "sosnowiec", "szczecin", "tarnow", "torun", "trojmiasto",
      "tychy", "walbrzych", "warszawa", "wloclawek", "wroclaw", "zabrze",
      "zielona-gora",
    )

    // The foreign cities are present in the global sort too.
    City.allSorted.map(_.slug) should contain allOf ("london", "manchester", "norwich", "berlin", "munich", "wurzburg")
  }

  it should "collate Ł after L (Łódź follows Lublin), not dump it at the end" in {
    // The Polish-collation discriminator: a naive code-point sort puts "Łódź"
    // (Ł = U+0141) after every ASCII-initial name, i.e. near the very end.
    val slugs = City.allSorted.map(_.slug)
    slugs.indexOf("lodz") shouldBe slugs.indexOf("lublin") + 1
    slugs.indexOf("lodz") should be < slugs.indexOf("zabrze")
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
