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
    // The full modelled roster — including the disabled UK cities — partitions
    // the global cinema universe with no overlap: every cinema is owned by
    // exactly one city, live or not.
    City.allModelled.flatMap(_.cinemas) should contain theSameElementsAs Cinema.all
    City.allModelled.flatMap(_.cinemas).distinct.size shouldBe Cinema.all.size
    // The live view (`City.all`, which the worker scrapes + web serves) is a
    // no-overlap subset of that universe — the disabled cities' cinemas stay
    // modelled but drop out of the live set.
    val liveCinemas = City.all.flatMap(_.cinemas)
    liveCinemas.distinct.size shouldBe liveCinemas.size
    liveCinemas.toSet.subsetOf(Cinema.all.toSet) shouldBe true
  }

  /** `/{slug}/` is ONE global namespace — `City.bySlug` searches every country's
   *  list — and the US now puts 457 places into it beside 41 Polish, 79 UK and
   *  158 German ones. Two cities sharing a slug means one of them is
   *  unreachable, silently, at whichever position `find` reaches second. */
  "Every city slug" should "be unique across every country, and URL-shaped" in {
    val slugs = City.allModelled.map(_.slug)
    val dupes = slugs.groupBy(identity).collect { case (s, xs) if xs.sizeIs > 1 => s }
    dupes shouldBe empty
    slugs.foreach(_ should fullyMatch regex "[a-z0-9]+(-[a-z0-9]+)*")
    City.allModelled.foreach(c => withClue(s"${c.slug}: ")(City.bySlug(c.slug) should contain(c)))
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
    City.allSorted.map(_.slug) should contain allOf ("london", "manchester", "birmingham", "berlin", "muenchen", "koeln")
  }

  it should "collate Ł after L (Łódź follows Lublin), not dump it at the end" in {
    // The Polish-collation discriminator: a naive code-point sort puts "Łódź"
    // (Ł = U+0141) after every ASCII-initial name, i.e. near the very end.
    val slugs = City.allSorted.map(_.slug)
    // Ł collates within the L-group, AFTER Lublin (naive code-point sort would push
    // it past every ASCII-initial name). Not `+1` anymore: German L-cities (Lübeck,
    // Lüneburg, Luckenwalde…) now interleave between Lublin and Łódź.
    slugs.indexOf("lodz") should be > slugs.indexOf("lublin")
    slugs.indexOf("lodz") should be < slugs.indexOf("zabrze")
  }

  // ── coveredPlaces ───────────────────────────────────────────────────────────

  "coveredPlaces" should "be the city itself, and nothing else, for a one-town city" in {
    Poznan.coveredPlaces      shouldBe Seq("Poznań")
    Poznan.otherCoveredPlaces shouldBe empty
  }

  it should "name every town a conurbation's own name hides" in {
    // The page is `/trojmiasto/`, and "Sopot" and "Gdynia" occur in no slug, no
    // label and no cinema display name — so without this the towns are on the
    // page nowhere at all, and a search for either can match nothing.
    Trojmiasto.coveredPlaces      shouldBe Seq("Trójmiasto", "Gdańsk", "Gdynia", "Sopot", "Rumia")
    Trojmiasto.otherCoveredPlaces shouldBe Seq("Gdańsk", "Gdynia", "Sopot", "Rumia")
  }

  it should "read a split city's districts, which are towns in their own right" in {
    val bay = City.all.find(_.labels.nominative == "San Francisco Bay Area")
      .getOrElse(fail("no San Francisco Bay Area metro in the roster"))
    bay.coveredPlaces.head shouldBe "San Francisco Bay Area"
    // The metro is named after the BAY, so it names neither the city it is
    // named after nor any of the towns around it. Both halves land: the
    // districts a local browses by, and the towns the cinemas are actually in.
    bay.otherCoveredPlaces should contain allOf ("East Bay", "South Bay")   // districts
    bay.otherCoveredPlaces should contain allOf ("San Francisco", "Berkeley", "San Jose")
    bay.coveredPlaces      shouldBe bay.coveredPlaces.distinct
  }

  it should "read a German region's towns, which the hub's own name hides" in {
    val koeln = City.all.find(_.slug == "koeln").getOrElse(fail("no Köln region in the roster"))
    koeln.coveredPlaces.head shouldBe "Köln"
    // A region is the towns within ~35 km of its hub, so `/koeln/` has always
    // listed Düsseldorf's and Bonn's cinemas while naming neither.
    koeln.otherCoveredPlaces should contain allOf ("Düsseldorf", "Bonn")
    koeln.coveredPlaces shouldBe koeln.coveredPlaces.distinct
  }

  // The gap the districts left: only the five biggest metros are sub-divided,
  // so the other 432 had nothing but their own name — `/san-diego/` covers
  // Chula Vista, Carlsbad and El Cajon and named none of them.
  it should "name a plain metro's towns, which have no districts to name them" in {
    val sd = City.all.find(_.labels.nominative == "San Diego").getOrElse(fail("no San Diego metro"))
    sd.areas              shouldBe empty
    sd.coveredPlaces.head shouldBe "San Diego"
    sd.otherCoveredPlaces should contain allOf ("Chula Vista", "Carlsbad", "El Cajon")
  }

  it should "read a Spanish province's towns, which the province's own name hides" in {
    val madrid = City.all.find(_.slug == "madrid").getOrElse(fail("no Madrid province in the roster"))
    madrid.coveredPlaces.head shouldBe "Madrid"
    madrid.otherCoveredPlaces should contain allOf ("Getafe", "Alcala de Henares")
    // The province is named after its capital, so the capital is both the
    // province name and one of its towns — named once, not twice.
    madrid.coveredPlaces shouldBe madrid.coveredPlaces.distinct
  }

  it should "ignore compass bearings, which name no place outside their city" in {
    London.areas.map(_.area.label) should contain("Central")  // London IS split…
    London.coveredPlaces           shouldBe Seq("London")     // …but into directions
    London.otherCoveredPlaces      shouldBe empty
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
