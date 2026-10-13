package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CitySpec extends AnyFlatSpec with Matchers {

  "Country.bySlug" should "resolve its own known slugs and reject an unknown one" in {
    val pl = Country.Poland.bySlug
    Seq("poznan" -> Poznan, "wroclaw" -> Wroclaw, "warszawa" -> Warszawa, "krakow" -> Krakow, "lodz" -> Lodz,
        "katowice" -> Katowice, "szczecin" -> Szczecin, "trojmiasto" -> Trojmiasto, "gliwice" -> Gliwice,
        "rzeszow" -> Rzeszow).foreach { case (slug, city) => pl.get(slug) shouldBe Some(city) }
    pl.get("sopot") shouldBe None
    pl.get("") shouldBe None
    // Another country's city is not this one's.
    Country.Poland.bySlug.get("london") shouldBe None
  }

  "Each city" should "scope to its own cinemas (disjoint partition of the universe)" in {
    Poznan.cinemas shouldBe Cinema.poznan
    Wroclaw.cinemas shouldBe Cinema.wroclaw
    Warszawa.cinemas shouldBe Cinema.warszawa
    // Every city's venues partition the global cinema universe with no overlap:
    // every cinema is owned by exactly one city.
    City.all.flatMap(_.cinemas) should contain theSameElementsAs Cinema.all
    City.all.flatMap(_.cinemas).distinct.size shouldBe Cinema.all.size
  }

  /** Every country owns its own slugs: `/uk/york/` is York and `/us/york/` York,
   *  Pennsylvania. Within ONE country two cities sharing a slug means one of them
   *  is unreachable, so a slug is unique there — and only there. */
  "Every city slug" should "be unique within its country, and URL-shaped" in {
    Country.all.foreach { country =>
      val slugs = country.cities.map(_.slug)
      withClue(s"${country.code}: ")(slugs.groupBy(identity).collect { case (s, xs) if xs.sizeIs > 1 => s } shouldBe empty)
      slugs.foreach(_ should fullyMatch regex "[a-z0-9]+(-[a-z0-9]+)*")
      country.cities.foreach(c => withClue(s"${country.code}/${c.slug}: ")(country.bySlug.get(c.slug) shouldBe Some(c)))
    }
  }

  it should "be free for two countries to share, each meaning its own town" in {
    val ukYork = Country.UnitedKingdom.bySlug("york")
    val usYork = Country.UnitedStates.bySlug("york")
    ukYork should not be usYork
    ukYork.country shouldBe Country.UnitedKingdom
    usYork.country shouldBe Country.UnitedStates
    // …with neither qualified for the other: the US page Alabama's Birmingham had to
    // be `/birmingham-alabama/` for is `/us/birmingham/`, the old slug its 301.
    Country.UnitedStates.bySlug("birmingham").country shouldBe Country.UnitedStates
    Country.UnitedKingdom.bySlug("birmingham").country shouldBe Country.UnitedKingdom
    Country.UnitedStates.redirectFor("birmingham-alabama") shouldBe Some("birmingham")
    Country.Spain.bySlug("toledo").country shouldBe Country.Spain
    Country.Spain.redirectFor("toledo-castilla-la-mancha") shouldBe Some("toledo")
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
      "augustow", "belchatow", "biala-podlaska", "bialystok", "bielsko-biala", "bielsk-podlaski",
      "bilgoraj", "bochnia", "boleslawiec", "braniewo", "brzeg", "brzeg-dolny",
      "busko-zdroj", "bydgoszcz", "bytom", "chelm", "chojnice", "choszczno",
      "ciechanow", "czechowice-dziedzice", "czestochowa", "dabrowa-gornicza", "dabrowa-tarnowska", "elblag",
      "elk", "gizycko", "gliwice", "glogow", "gniezno", "goldap",
      "gorzow-wielkopolski", "grajewo", "grudziadz", "hrubieszow", "ilawa", "inowroclaw",
      "jaroslaw", "jaslo", "jastrzebie-zdroj", "jaworzno", "jelenia-gora", "kalisz",
      "katowice", "kedzierzyn-kozle", "kepno", "kielce", "kluczbork", "klodzko",
      "kolobrzeg", "konin", "koszalin", "koscierzyna", "krakow", "krapkowice",
      "krasnystaw", "krasnik", "krosno", "krotoszyn", "krynica-zdroj", "kutno",
      "legionowo", "legnica", "leszno", "lidzbark-warminski", "lipno", "lubin",
      "lublin", "lubliniec", "lapy", "leba", "lomza", "lodz",
      "lukow", "miechow", "mielec", "miedzyrzecz", "miedzyzdroje",
      "mlawa", "morag", "myszkow", "naklo-nad-notecia", "nowa-sol", "nowy-sacz",
      "nowy-targ", "nowy-tomysl", "nysa", "olecko", "olesnica", "olsztyn",
      "olsztynek", "olawa", "opoczno", "opole", "ostrowiec-swietokrzyski", "ostrow-mazowiecka",
      "ostrow-wielkopolski", "oswiecim", "otwock", "pabianice", "parczew", "pila",
      "piotrkow-trybunalski", "plock", "police", "polkowice", "poznan", "pruszkow",
      "przemysl", "przeworsk", "pulawy", "pultusk", "radom", "radomsko",
      "rawicz", "ruda-slaska", "rybnik", "rypin", "rzeszow", "sanok",
      "siedlce", "sierpc", "skierniewice", "slawno", "slubice", "slupca",
      "slupsk", "sokolow-podlaski", "sokolka", "sosnowiec", "stalowa-wola", "stara-blotnica",
      "starachowice", "stargard", "strzelin", "suwalki", "szamotuly", "szczecin",
      "szczecinek", "szczytno", "srem", "swidnica", "swidnik", "swiebodzin",
      "tarnow", "tczew", "tomaszow-lubelski", "tomaszow-mazowiecki", "torun", "trojmiasto",
      "turek", "tychy", "wadowice", "walbrzych", "warszawa", "wagrowiec",
      "wejherowo", "wielen", "wieliczka", "wielun", "wloclawek", "wlodawa",
      "wloszczowa", "wolomin", "wroclaw", "wysokie-mazowieckie", "wyszkow", "zabrze",
      "zakopane", "zamosc", "zawiercie", "zdunska-wola", "zgierz", "zgorzelec",
      "zielona-gora", "zlocieniec", "zlotow", "zary", "zory", "zyrardow",
      "zywiec",
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
    Sosnowiec.coveredPlaces      shouldBe Seq("Sosnowiec")
    Sosnowiec.otherCoveredPlaces shouldBe empty
  }

  it should "name every town a conurbation's own name hides" in {
    // The page is `/trojmiasto/`, and "Sopot" and "Gdynia" occur in no slug, no
    // label and no cinema display name — so without this the towns are on the
    // page nowhere at all, and a search for either can match nothing.
    // Its satellite towns (Rumia, Wejherowo…) are pages of their own now.
    val towns = Seq("Gdańsk", "Gdynia", "Sopot")
    Trojmiasto.coveredPlaces      shouldBe "Trójmiasto" +: towns
    Trojmiasto.otherCoveredPlaces shouldBe towns
  }

  // A US page was a 75 km metro until 2026-10, and `/new-york/` reached Long
  // Island; it is Poland's shape now — a major city is itself alone (naming the
  // boroughs its venues give as their town), a cluster names its towns.
  it should "keep a major US city to itself, naming the boroughs its venues are in" in {
    val nyc = Country.UnitedStates.bySlug.get("new-york").getOrElse(fail("no New York page in the roster"))
    nyc.coveredPlaces.head shouldBe "New York"
    nyc.otherCoveredPlaces should contain allOf ("Brooklyn", "Staten Island")
    nyc.otherCoveredPlaces should not contain "Yonkers"
    nyc.coveredPlaces shouldBe nyc.coveredPlaces.distinct
  }

  // A German page was a 35 km region until 2026-10, and `/koeln/` named Düsseldorf
  // and Bonn; it is Poland's shape now — a major city is itself alone, a cluster
  // names its towns.
  it should "keep a major German city to itself, and name a cluster's towns" in {
    val koeln = Country.Germany.bySlug.get("koeln").getOrElse(fail("no Köln page in the roster"))
    koeln.coveredPlaces shouldBe Seq("Köln")
    val bayreuth = Country.Germany.bySlug.get("bayreuth").getOrElse(fail("no Bayreuth page in the roster"))
    bayreuth.coveredPlaces shouldBe Seq("Bayreuth und Umgebung", "Bayreuth", "Kulmbach", "Pegnitz", "Hollfeld")
  }

  it should "name a US cluster's towns" in {
    val coeur = Country.UnitedStates.bySlug.get("coeur-d-alene").getOrElse(fail("no Coeur d'Alene page in the roster"))
    coeur.coveredPlaces shouldBe Seq("Coeur d'Alene area", "Coeur d'Alene", "Hayden")
  }

  // Poland's pages named no town but Trójmiasto's, on the assumption that a
  // Polish city is one town. 36 of the 41 are not: `/tarnow/` lists cinemas in
  // Biecz, Gorlice, Bochnia and Brzesko, and named none of them.
  it should "keep a major city to itself, and read a cluster's towns off its venues" in {
    // Tarnów used to list Biecz, Gorlice, Bochnia… — they are pages of their own now.
    Tarnow.otherCoveredPlaces shouldBe empty
    val turek = Country.Poland.bySlug.get("turek").get
    turek.coveredPlaces shouldBe Seq("Turek i okolice", "Turek", "Koło")
  }

  it should "keep a Polish city that really is one town naming only itself" in {
    Sosnowiec.otherCoveredPlaces shouldBe empty
  }

  // A Spanish page was a whole province until 2026-10, and named its towns; it is
  // Poland's shape now — a major city is itself alone, a cluster names its towns.
  it should "keep a major Spanish city to itself, and name a cluster's towns, accented" in {
    val madrid = Country.Spain.bySlug.get("madrid").getOrElse(fail("no Madrid page in the roster"))
    madrid.coveredPlaces shouldBe Seq("Madrid")
    val sitges = Country.Spain.bySlug.get("sitges").getOrElse(fail("no Sitges page in the roster"))
    // Spelled the way Spanish spells them, which SensaCine's own headers are not
    // (see data/spain/scripts/build_venue_towns.py).
    sitges.coveredPlaces shouldBe Seq("Sitges y alrededores", "Sitges", "Vilanova i la Geltrú", "Sant Pere de Ribes")
  }

  // A UK page was a Flicks region until 2026-10 (/aberdeenshire/ covered Aberdeen,
  // Peterhead and Banchory); it is Poland's shape now — a cluster names its towns.
  it should "name a UK cluster's towns, the one it is named after first" in {
    val peterhead = Country.UnitedKingdom.bySlug.get("peterhead").getOrElse(fail("no Peterhead page in the roster"))
    peterhead.coveredPlaces shouldBe Seq("Peterhead area", "Peterhead", "Ellon")
    peterhead.locativePhrase shouldBe "in the Peterhead area"
    val aberdeen = Country.UnitedKingdom.bySlug.get("aberdeen").getOrElse(fail("no Aberdeen page in the roster"))
    aberdeen.coveredPlaces.head shouldBe "Aberdeen"
    aberdeen.locativePhrase shouldBe "in Aberdeen"
  }

  it should "ignore compass bearings, which name no place outside their city" in {
    London.areas.map(_.area.label) should contain("Central")   // London IS split…
    // …but into directions. "Central" is not a place, and a `containsPlace`
    // claiming one would be a claim about a town by that name.
    London.coveredPlaces should contain noneOf ("Central", "North", "East", "South", "West")
    // What it does cover are the boroughs' towns its venues sit in, which the
    // bare "London" never named.
    London.otherCoveredPlaces should contain allOf ("Romford", "Sidcup", "Wembley")
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
