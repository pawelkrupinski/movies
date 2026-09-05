package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The `Country` spine: the scope above `City`. Locks in that Poland is the
 * default country, keeps its original database name (`kinowo`, so the existing
 * prod deployment is byte-identical), owns exactly today's city list, and that
 * the DB name is DERIVED from the country with an explicit `MONGODB_DB` still
 * winning — the single source of truth that replaces the scattered
 * `getOrElse("kinowo")` fallbacks.
 */
class CountrySpec extends AnyFlatSpec with Matchers {

  "Country.byCode" should "resolve pl/uk/de/us case-insensitively and reject unknown codes" in {
    Country.byCode("pl") shouldBe Some(Country.Poland)
    Country.byCode("PL") shouldBe Some(Country.Poland)
    Country.byCode("  pl ") shouldBe Some(Country.Poland)
    Country.byCode("uk") shouldBe Some(Country.UnitedKingdom)
    Country.byCode("UK") shouldBe Some(Country.UnitedKingdom)
    Country.byCode("de") shouldBe Some(Country.Germany)
    Country.byCode("DE") shouldBe Some(Country.Germany)
    Country.byCode("us") shouldBe Some(Country.UnitedStates)
    Country.byCode("US") shouldBe Some(Country.UnitedStates)
    Country.byCode("es") shouldBe Some(Country.Spain)
    Country.byCode("ES") shouldBe Some(Country.Spain)
    Country.byCode("xx") shouldBe None
    Country.byCode("") shouldBe None
  }

  "A KINOWO_COUNTRIES-style code list" should "resolve 'pl,uk,de,us,es' to all five countries, in order" in {
    // The exact contract each worker's KINOWO_COUNTRIES depends on
    // (WorkerMain.resolveCountries splits on comma and maps each via byCode).
    "pl,uk,de,us,es".split(",").toList.flatMap(c => Country.byCode(c.trim)) shouldBe
      List(Country.Poland, Country.UnitedKingdom, Country.Germany, Country.UnitedStates, Country.Spain)
  }

  "Country.UnitedKingdom" should "be an English, Filmweb-free deployment (Flicks-sourced) on its own database" in {
    Country.UnitedKingdom.code shouldBe "uk"
    Country.UnitedKingdom.mongoDb shouldBe "kinowo_uk"
    Country.UnitedKingdom.filmwebEnabled shouldBe false
    Country.UnitedKingdom.language.toLanguageTag shouldBe "en-GB"
    Country.UnitedKingdom.brandName shouldBe "Showtimes"   // "Kinowo" is Polish-only
    Country.UnitedKingdom.cities shouldBe City.ukCities
    Country.UnitedKingdom.cities.map(_.slug) should contain allOf ("london", "manchester", "birmingham")
  }

  "Country.UnitedKingdom.cities" should "be the full modelled UK roster (every Flicks region live)" in {
    // Every one of the 79 modelled regions is live — web serves them and the
    // worker scrapes them. `activeUkCities` currently equals the full roster, so
    // `ukCities` is `allUkCities` unchanged (in its declared order).
    City.ukCities shouldBe City.allUkCities
    City.ukCities should have size 79
    City.activeUkCities shouldBe City.allUkCities.toSet
    // Formerly-disabled regions (e.g. Norwich) are now live too.
    City.ukCities.map(_.slug) should contain("norwich")
  }

  "Country.Germany" should "be a German, Filmweb-free deployment (Filmstarts-sourced) on its own database" in {
    Country.Germany.code shouldBe "de"
    Country.Germany.mongoDb shouldBe "kinowo_de"
    Country.Germany.filmwebEnabled shouldBe false
    Country.Germany.language.toLanguageTag shouldBe "de-DE"
    Country.Germany.brandName shouldBe "Showtimes"   // any non-Polish deployment
    Country.Germany.cities shouldBe City.germanCities
    // The full data-driven Filmstarts roster: 158 regions (German slugs — muenchen,
    // koeln, …), each an aggregation of nearby cities' cinemas (see data/germany/).
    Country.Germany.cities should have size 158
    Country.Germany.cities.map(_.slug) should contain allOf ("berlin", "muenchen", "koeln", "hamburg", "frankfurt-am-main")
    // Every region carries cinemas; the roster totals 1,529 venues.
    Country.Germany.cities.flatMap(_.cinemas).size shouldBe 1529
  }

  "Country.UnitedStates" should "be an English, Filmweb-free deployment (Flicks-sourced) on its own database" in {
    Country.UnitedStates.code shouldBe "us"
    Country.UnitedStates.mongoDb shouldBe "kinowo_us"
    Country.UnitedStates.filmwebEnabled shouldBe false
    Country.UnitedStates.language.toLanguageTag shouldBe "en-US"
    Country.UnitedStates.brandName shouldBe "Showtimes"   // "Kinowo" is Polish-only
    Country.UnitedStates.cities shouldBe City.usCities
  }

  "Country.UnitedStates.cities" should "be one city per metro, the state being only how you find one" in {
    // The addressable place is the metro: 461 of them, plus the seven states and
    // territories with too few venues — and compact enough — to be worth
    // splitting. A state is a `CityGroup`, so `/california/` resolves to nothing.
    Country.UnitedStates.cities should have size 468
    Country.UnitedStates.cities.map(_.slug) should contain allOf (
      "los-angeles", "new-york", "houston", "district-of-columbia", "san-juan")
    Country.UnitedStates.bySlug.get("california") shouldBe None
    // Every place carries venues, and the roster still totals 5,031 — this was a
    // re-key, not a re-harvest. That corpus is ~6x the UK's, the fact that drives
    // the US worker's 840-minute cadence rather than the UK's 420 (a ~10h sweep
    // has to fit inside its own cadence).
    all(Country.UnitedStates.cities.map(_.cinemas.size)) should be > 0
    Country.UnitedStates.cities.flatMap(_.cinemas).size shouldBe 5031
  }

  "Country.cityGroups" should "group the US by state and leave the flat countries flat" in {
    Country.Poland.cityGroups shouldBe empty
    Country.Germany.cityGroups shouldBe empty
    Country.UnitedStates.cityGroups should have size 55
    // The groups partition the country's cities — a metro reachable from no
    // state heading is a metro nobody can find.
    Country.UnitedStates.cityGroups.flatMap(_.cities) should contain theSameElementsAs
      Country.UnitedStates.cities
    Country.UnitedStates.cityGroups.map(_.label) should contain allOf ("California", "Texas", "Alaska")
    Country.UnitedStates.cityGroups.find(_.label == "California").get
      .cities.map(_.labels.nominative) should contain ("Los Angeles")
  }

  it should "group the UK by nation, over the counties the roster is already cut into" in {
    // A level UP rather than one down: a Flicks region already is the county, so
    // 79 of them wanted a heading above, not metros below.
    Country.UnitedKingdom.cityGroups.map(_.label) shouldBe
      Seq("England", "Scotland", "Wales", "Northern Ireland", "Crown Dependencies")
    // The same partition invariant the US groups hold: every live UK city is
    // reachable from exactly one nation, and no nation names a city twice.
    Country.UnitedKingdom.cityGroups.flatMap(_.cities) should contain theSameElementsAs
      Country.UnitedKingdom.cities
    Country.UnitedKingdom.cityGroups.flatMap(_.cities).distinct should have size
      Country.UnitedKingdom.cities.size

    def nation(label: String): Seq[String] =
      Country.UnitedKingdom.cityGroups.find(_.label == label).get.cities.map(_.slug)

    nation("England")           should contain allOf ("london", "cheshire", "liverpool", "kent")
    nation("Scotland")          should contain allOf ("glasgow", "fife", "highlands-and-islands")
    nation("Wales")             should contain allOf ("cardiff", "gwynedd", "powys")
    nation("Northern Ireland")  should contain allOf ("belfast", "antrim", "tyrone")
    nation("Crown Dependencies") should contain theSameElementsAs Seq("guernsey", "isle-of-man", "jersey")
    // A nation is a heading, not a page: `/scotland/` is nothing, the same way
    // `/california/` is.
    Country.UnitedKingdom.cityGroups.map(_.slug).flatMap(Country.UnitedKingdom.bySlug.get) shouldBe empty
  }

  "CityGroup.soleCity" should "collapse a group that stands exactly where its one city stands" in {
    def group(country: Country, slug: String): CityGroup =
      country.cityGroups.find(_.slug == slug).getOrElse(fail(s"no group $slug"))

    // Delaware is too small to cut into metros, so the state's own venue list IS
    // `/delaware/`: the picker links straight there rather than offering a
    // heading you open to find one row repeating it.
    group(Country.UnitedStates, "delaware").soleCity.map(_.slug) shouldBe Some("delaware")
    group(Country.UnitedStates, "vermont").soleCity.map(_.slug) shouldBe Some("vermont")

    // California is a way to FIND a place, never a place — even though one of
    // its metros could have carried its name, it holds many.
    group(Country.UnitedStates, "california").soleCity shouldBe None
    // New York holds a metro of its own name, and Buffalo and Rochester besides:
    // linking the heading straight through would bury them.
    group(Country.UnitedStates, "new-york").cities.map(_.slug) should contain ("new-york")
    group(Country.UnitedStates, "new-york").soleCity shouldBe None

    // No UK nation is a place either, however few counties are live in it.
    Country.UnitedKingdom.cityGroups.flatMap(_.soleCity) shouldBe empty
  }

  "Country.Spain" should "be a Spanish, Filmweb-free deployment (SensaCine-sourced) on its own database" in {
    Country.Spain.code shouldBe "es"
    Country.Spain.mongoDb shouldBe "kinowo_es"
    Country.Spain.filmwebEnabled shouldBe false
    Country.Spain.language.toLanguageTag shouldBe "es-ES"
    Country.Spain.brandName shouldBe "Showtimes"   // "Kinowo" is Polish-only
    Country.Spain.cities shouldBe City.spanishCities
  }

  "Country.Spain.cities" should "be one city per province, flat like Germany's regions" in {
    // The 52 provinces SensaCine itself enumerates. Flat, with no CityGroup above
    // them: 52 is a list a picker stays readable at, and a province is the unit a
    // Spanish visitor names — unlike a US state, which nobody asks what is on in.
    Country.Spain.cities should have size 52
    Country.Spain.cityGroups shouldBe empty
    Country.Spain.cities.map(_.slug) should contain allOf ("madrid", "barcelona", "valencia", "las-palmas")
    all(Country.Spain.cities.map(_.cinemas.size)) should be > 0
    Country.Spain.cities.flatMap(_.cinemas).size shouldBe 595
  }

  it should "qualify a province slug another country already serves, and only that one" in {
    // `/toledo/` is the live US metro in Ohio — a published URL with a sitemap
    // entry and a `city` cookie behind it — so the NEWCOMER moves, not the
    // incumbent. Every other province keeps its bare name.
    Country.Spain.bySlug.get("toledo") shouldBe None
    Country.Spain.cities.map(_.slug) should contain ("toledo-castilla-la-mancha")
    City.bySlug("toledo").get.country shouldBe Country.UnitedStates
    // The LABEL is untouched — inside Spain "Toledo" is not ambiguous, and the
    // picker is what a visitor reads.
    Country.Spain.bySlug("toledo-castilla-la-mancha").labels.nominative shouldBe "Toledo"
  }

  "Spanish provinces" should "carry their own time zone, so the Canaries are not on peninsular time" in {
    // Spain has TWO zones, which is why `SpanishProvince` takes one per place like
    // `UsCity` rather than hardcoding one like `GermanRegion`. An hour is enough to
    // move a day boundary, so a Canary venue's late showing would otherwise fall on
    // the wrong day.
    def zoneOf(slug: String) = Country.Spain.bySlug(slug).zoneId.getId
    zoneOf("madrid")                 shouldBe "Europe/Madrid"
    zoneOf("barcelona")              shouldBe "Europe/Madrid"
    zoneOf("las-palmas")             shouldBe "Atlantic/Canary"
    zoneOf("santa-cruz-de-tenerife") shouldBe "Atlantic/Canary"
  }

  "US places" should "carry their own time zone rather than one national default" in {
    // Unlike Germany (one Europe/Berlin for every region), the US spans six zones,
    // so `UsCity` takes the zone per place. A single national default would put
    // California's day boundary three hours early. A metro carries its OWN zone,
    // resolved from its venues' coordinates — not its state's predominant one,
    // which is a different answer for a metro on the far side of a boundary.
    def zoneOf(slug: String) =
      Country.UnitedStates.cities.find(_.slug == slug).get.zoneId.getId
    zoneOf("los-angeles") shouldBe "America/Los_Angeles"
    zoneOf("new-york")    shouldBe "America/New_York"
    zoneOf("houston")     shouldBe "America/Chicago"
    zoneOf("oahu")        shouldBe "Pacific/Honolulu"
    zoneOf("phoenix")     shouldBe "America/Phoenix"   // no DST, deliberately its own
    zoneOf("knoxville")   shouldBe "America/New_York"  // Tennessee's is Central
    zoneOf("el-paso")     shouldBe "America/Denver"    // Texas's is Central
  }

  "Every modelled cinema display name" should "be globally unique across all five countries" in {
    // displayName is the WIRE KEY every per-cinema slot is stored under
    // (`movie_slots`, `screenings`, the embedded `sourceData` map) and
    // `Source.byDisplayName` is a plain `toMap` — so two venues sharing a name
    // silently collapse to whichever is built LAST, and the loser's stored slots
    // read back as the WINNER's. Adding ~4,250 US venues is far and away the
    // largest chance this repo has had to introduce such a collision.
    val names = City.allModelled.flatMap(_.cinemas).map(_.displayName)
    val dupes = names.groupBy(identity).collect { case (n, xs) if xs.sizeIs > 1 => n }
    dupes shouldBe empty
  }

  // Four venues were DELISTED by Filmstarts: every scrape got HTTP 404 for
  // `/kinoprogramm/kino/<id>/`, burning retries on every cycle and showing users a
  // cinema that no longer exists. Checked 2026-07-31 against Filmstarts' own
  // exhaustive city/state listings — none has been re-issued under a new id, and a
  // live-but-idle venue returns 200 with `no.showtime.error`, never 404, so a 404
  // means deletion rather than a quiet season:
  //   A0743 Kino Kiste (Berlin-Hellersdorf) — closed 31.12.2025
  //   G01C9 Inselkino Baltrum — no operator; the Gemeinde is advertising for one
  //   A2843 Heppel - Ettlich (München) — venue open, but as a Kleinkunst stage
  //   A2165 Kino Babenhausen — hall alive for theatre, no cinema programme
  // NOTE: do NOT re-point Heppel to A1575 "Neues Rottmann" — that is a different
  // operating cinema, not a rename.
  it should "not carry the Filmstarts theater ids that were delisted upstream" in {
    val delisted = Set("A0743", "G01C9", "A2843", "A2165")
    GermanRoster.theaterIdByCinema.values.toSet intersect delisted shouldBe empty
    val names = Country.Germany.cities.flatMap(_.cinemas).map(_.displayName).toSet
    names should not contain "Kino Kiste"
    names should not contain "Inselkino Baltrum"
  }

  "Country.Poland" should "keep the original kinowo database and Filmweb enabled" in {
    Country.default shouldBe Country.Poland
    // Renaming this to kinowo_pl would orphan the live prod database.
    Country.Poland.mongoDb shouldBe "kinowo"
    Country.Poland.filmwebEnabled shouldBe true
    Country.Poland.language.toLanguageTag shouldBe "pl-PL"
    Country.Poland.brandName shouldBe "Kinowo"   // the brand keeps its Polish name at home
  }

  "Every country" should "map to a distinct database (no two share one db)" in {
    val dbs = Country.all.map(_.mongoDb)
    dbs.distinct.size shouldBe dbs.size
  }

  "Country.Poland.cities" should "be exactly today's Polish city list; City.all is the union across countries" in {
    Country.Poland.cities shouldBe City.polishCities
    // City.all is the concatenation of every country's list (PL + UK + DE + US + ES).
    City.all should contain theSameElementsAs
      (City.polishCities ++ City.ukCities ++ City.germanCities ++ City.usCities ++ City.spanishCities)
    Country.all.flatMap(_.cities) should contain theSameElementsAs City.all
  }

  "Country.of and City.country" should "reverse-map each city back to its own country" in {
    Country.of(Poznan) shouldBe Country.Poland
    Warszawa.country shouldBe Country.Poland
    London.country shouldBe Country.UnitedKingdom
    City.bySlug("berlin").get.country shouldBe Country.Germany
    City.bySlug("los-angeles").get.country shouldBe Country.UnitedStates
    City.bySlug("barcelona").get.country shouldBe Country.Spain
    // Every city belongs to exactly the country whose list contains it.
    City.all.foreach(c => Country.of(c).cities should contain(c))
  }

  "A country's scoped views" should "scope to that country's own cities, a strict subset of the global views" in {
    Country.Poland.bySlug.get("poznan") shouldBe Some(Poznan)
    Country.Poland.bySlug.get("sopot") shouldBe None
    Country.Poland.bySlug.get("london") shouldBe None            // London is a UK city
    Country.UnitedKingdom.bySlug.get("london") shouldBe Some(London)
    Country.Poland.allSorted.toSet shouldBe City.polishCities.toSet
    Country.UnitedKingdom.allSorted.toSet shouldBe City.ukCities.toSet         // the full 79-region UK roster
    Country.UnitedKingdom.allSorted.head shouldBe Aberdeenshire                // English collation A→Z
    Country.UnitedKingdom.allSorted.last shouldBe Yorkshire
    Country.Poland.allJson should include("poznan")
    Country.Poland.allJson should not include "london"
  }

  "Country.switchable" should "list every deployed country (webUrl defined), Poland first" in {
    // The navbar country <select> iterates this, in this order.
    Country.switchable shouldBe
      Seq(Country.Poland, Country.UnitedKingdom, Country.Germany, Country.UnitedStates, Country.Spain)
    Country.Poland.webUrl shouldBe Some("https://kinowo.net")
    Country.UnitedKingdom.webUrl shouldBe Some("https://showtimes.cc/uk")
    Country.Germany.webUrl shouldBe Some("https://showtimes.cc/de")
    Country.UnitedStates.webUrl shouldBe Some("https://showtimes.cc/us")
    Country.Spain.webUrl shouldBe Some("https://showtimes.cc/es")
    // Being switchable is the ONE flag that adds a country to the navbar
    // <select>, the debug ?country= switcher and the /api/catalog mobile
    // endpoint — all three iterate this, so nothing else enumerates them.
    Country.switchable should contain (Country.UnitedStates)
    // Every switchable country carries a host (no trailing slash) and a label.
    Country.switchable.foreach { c =>
      c.webUrl.get should (startWith("https://") and not endWith "/")
      c.displayName should not be empty
    }
  }

  "A country's share-preview (Open Graph) identity" should "carry its own host origin and home-montage filename" in {
    // The `/` landing card and the default og:image are built from these, so a
    // UK deployment previews off uk.showtimes.cc with an English montage
    // (og-home-uk.jpg) instead of Poland's kinowo.net / og-home.jpg.
    Country.Poland.ogOrigin shouldBe "https://kinowo.net"
    Country.Poland.homeOgImage shouldBe "og-home.jpg"                    // the default keeps the unsuffixed asset
    Country.UnitedKingdom.ogOrigin shouldBe "https://showtimes.cc/uk"
    Country.UnitedKingdom.homeOgImage shouldBe "og-home-uk.jpg"
    // Germany previews off its own base, with a per-code montage name.
    Country.Germany.ogOrigin shouldBe "https://showtimes.cc/de"
    Country.Germany.homeOgImage shouldBe "og-home-de.jpg"
  }

  "Country.shareHost" should "be the bare domain each country's share cards are stamped with" in {
    // Drawn into every Open Graph PNG. It used to be the literal "kinowo.fly.dev"
    // in the renderer, so UK and German cards advertised the Polish host.
    Country.Poland.shareHost shouldBe "kinowo.net"
    Country.UnitedKingdom.shareHost shouldBe "showtimes.cc/uk"
    Country.Germany.shareHost shouldBe "showtimes.cc/de"
    Country.all.foreach(c => c.shareHost should not startWith "http")
  }

  "Country.isApexHost" should "recognise the brand domain, in every spelling, and nothing else" in {
    Country.isApexHost("showtimes.cc") shouldBe true
    Country.isApexHost("www.showtimes.cc") shouldBe true
    Country.isApexHost("SHOWTIMES.CC") shouldBe true
    Country.isApexHost("showtimes.cc:9000") shouldBe true
    Country.isApexHost("kinowo.net") shouldBe false
    Country.isApexHost("localhost") shouldBe false
    // A lookalike domain that merely ENDS with the apex must not match.
    Country.isApexHost("notshowtimes.cc") shouldBe false
  }

  "Country.servesApex" should "be answered only by the deployment mounted at the root" in {
    // The brand domain is now ALSO the host every Showtimes country's own pages
    // arrive on (`showtimes.cc/uk/kent/`), so the host alone no longer says
    // "front door". Only the country mounted at `/` has a `/` that isn't
    // already a country's landing — matching on the UK deployment would replace
    // its homepage with a country picker.
    Country.Poland.servesApex("showtimes.cc") shouldBe true
    Country.Poland.servesApex("www.showtimes.cc") shouldBe true
    Country.Poland.servesApex("showtimes.cc:9000") shouldBe true
    Country.UnitedKingdom.servesApex("showtimes.cc") shouldBe false
    Country.Germany.servesApex("showtimes.cc") shouldBe false
    Country.UnitedStates.servesApex("showtimes.cc") shouldBe false
    Country.Spain.servesApex("showtimes.cc") shouldBe false
    // A country's own domain is never the front door either.
    Country.Poland.servesApex("kinowo.net") shouldBe false
  }

  "A country's mount point" should "be the root for the country that owns its domain and a code segment for the rest" in {
    // ONE setting drives Play's `play.http.context`, the session/flash cookie
    // paths and every reverse route; the deployments differ only here.
    Country.Poland.pathPrefix shouldBe ""
    Country.Poland.mountPath shouldBe "/"
    Country.UnitedKingdom.pathPrefix shouldBe "/uk"
    Country.UnitedKingdom.mountPath shouldBe "/uk/"
    Country.Germany.mountPath shouldBe "/de/"
    Country.UnitedStates.mountPath shouldBe "/us/"
    Country.Spain.mountPath shouldBe "/es/"
    // The origin is the bare host — the prefix is the only thing that differs
    // between the four Showtimes countries.
    Country.UnitedKingdom.webOrigin shouldBe Some("https://showtimes.cc")
    Country.Germany.webOrigin shouldBe Some("https://showtimes.cc")
    Country.UnitedStates.webOrigin shouldBe Some("https://showtimes.cc")
    Country.Spain.webOrigin shouldBe Some("https://showtimes.cc")
    Country.Poland.webOrigin shouldBe Some("https://kinowo.net")
    // Every mount point is absolute and slash-terminated: Play rejects a
    // context that does not start with "/", and the trailing slash is what
    // keeps the landing at `/uk/` rather than `/uk`.
    Country.all.foreach { c =>
      c.mountPath should (startWith("/") and endWith("/"))
      c.pathPrefix should not endWith "/"
    }
  }

  "Country.resolvedDbName" should "prefer an explicit MONGODB_DB over the country default" in {
    // Only meaningful when nothing already supplies MONGODB_DB from the ambient
    // environment (env var / .env.local); skip otherwise to stay deterministic.
    if (System.getenv("MONGODB_DB") == null && tools.Env.get("MONGODB_DB").isEmpty) {
      val prev = System.getProperty("MONGODB_DB")
      try {
        System.setProperty("MONGODB_DB", "kinowo_override_probe")
        Country.resolvedDbName shouldBe "kinowo_override_probe"
      } finally {
        if (prev == null) System.clearProperty("MONGODB_DB") else System.setProperty("MONGODB_DB", prev)
      }
    }
  }

  it should "fall back to the process country's database when MONGODB_DB is unset" in {
    if (tools.Env.get("MONGODB_DB").isEmpty)
      Country.resolvedDbName shouldBe Country.fromEnv.mongoDb
  }

  /** The `users` + `userStates` collections are the ONE thing four country
   *  deployments must agree on: a person is not per country. `MONGODB_USERS_DB`
   *  names the database holding them, and the fallback is what keeps the change
   *  a no-op everywhere it is not set. */
  "Country.usersDbNameFrom" should "send the user collections to MONGODB_USERS_DB when it names one" in {
    Country.usersDbNameFrom(Some("kinowo_users"), "kinowo_uk") shouldBe "kinowo_users"
    Country.usersDbNameFrom(Some("kinowo_users"), "kinowo_de") shouldBe "kinowo_users"
  }

  it should "give every country the SAME users database, whatever its own is" in {
    val dbs = Country.all.map(c => Country.usersDbNameFrom(Some("kinowo_users"), c.mongoDb))
    dbs.distinct shouldBe List("kinowo_users")
  }

  it should "leave them in the deployment's own database when it is unset" in {
    Country.usersDbNameFrom(None, "kinowo_uk") shouldBe "kinowo_uk"
    Country.usersDbNameFrom(None, "kinowo")    shouldBe "kinowo"
  }

  // Blank is how the setting gets switched OFF in a ConfigMap — the key stays,
  // the value empties. Treating it as a database literally called "" would defer
  // the failure to the first query instead of never happening at all.
  it should "treat a blank or whitespace-only value as unset" in {
    Country.usersDbNameFrom(Some(""),    "kinowo_de") shouldBe "kinowo_de"
    Country.usersDbNameFrom(Some("   "), "kinowo_de") shouldBe "kinowo_de"
  }

  it should "trim a value that arrived with stray whitespace" in {
    Country.usersDbNameFrom(Some(" kinowo_users "), "kinowo_de") shouldBe "kinowo_users"
  }

  "Country.usersDbName" should "read MONGODB_USERS_DB from the environment" in {
    if (tools.Env.get("MONGODB_USERS_DB").isEmpty) {
      val prev = System.getProperty("MONGODB_USERS_DB")
      try {
        System.setProperty("MONGODB_USERS_DB", "kinowo_users_probe")
        Country.usersDbName shouldBe "kinowo_users_probe"
      } finally {
        if (prev == null) System.clearProperty("MONGODB_USERS_DB") else System.setProperty("MONGODB_USERS_DB", prev)
      }
    }
  }

  it should "fall back to this deployment's own database when it is unset" in {
    if (tools.Env.get("MONGODB_USERS_DB").isEmpty)
      Country.usersDbName shouldBe Country.resolvedDbName
  }

  /** The two deployments name their country through DIFFERENT env vars — web
   *  `KINOWO_COUNTRY=de`, each worker `KINOWO_COUNTRIES=de` — so anything
   *  process-global that must be country-correct has to read both. Reading only
   *  the singular is why the country-scoped title rules shipped working on web
   *  and doing NOTHING on the worker that writes the corpus. */
  "soleFrom" should "read the worker's plural KINOWO_COUNTRIES" in {
    Country.soleFrom(None, Some("de")) shouldBe Some(Country.Germany)
    Country.soleFrom(None, Some("uk")) shouldBe Some(Country.UnitedKingdom)
    Country.soleFrom(None, Some("pl")) shouldBe Some(Country.Poland)
  }

  it should "prefer the web's singular KINOWO_COUNTRY when both are set" in {
    Country.soleFrom(Some("de"), Some("pl")) shouldBe Some(Country.Germany)
  }

  it should "tolerate whitespace, unknown codes and empty entries" in {
    Country.soleFrom(None, Some(" de ")) shouldBe Some(Country.Germany)
    Country.soleFrom(None, Some("de,,")) shouldBe Some(Country.Germany)
    Country.soleFrom(None, Some("atlantis")) shouldBe None
    Country.soleFrom(None, None) shouldBe None
  }

  it should "yield None for a multi-country worker — no single global value fits" in {
    Country.soleFrom(None, Some("pl,de")) shouldBe None
  }
}
