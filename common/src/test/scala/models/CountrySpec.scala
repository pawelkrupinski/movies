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

  "Country.byCode" should "resolve pl/uk/de case-insensitively and reject unknown codes" in {
    Country.byCode("pl") shouldBe Some(Country.Poland)
    Country.byCode("PL") shouldBe Some(Country.Poland)
    Country.byCode("  pl ") shouldBe Some(Country.Poland)
    Country.byCode("uk") shouldBe Some(Country.UnitedKingdom)
    Country.byCode("UK") shouldBe Some(Country.UnitedKingdom)
    Country.byCode("de") shouldBe Some(Country.Germany)
    Country.byCode("DE") shouldBe Some(Country.Germany)
    Country.byCode("xx") shouldBe None
    Country.byCode("") shouldBe None
  }

  "A KINOWO_COUNTRIES-style code list" should "resolve 'pl,uk,de' to all three countries, in order" in {
    // The exact contract fly.worker.toml's KINOWO_COUNTRIES='pl,uk,de' depends on
    // (WorkerMain.resolveCountries splits on comma and maps each via byCode).
    "pl,uk,de".split(",").toList.flatMap(c => Country.byCode(c.trim)) shouldBe
      List(Country.Poland, Country.UnitedKingdom, Country.Germany)
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

  "Country.UnitedKingdom.cities" should "be scoped to the five largest UK cities while the full roster stays modelled" in {
    // Only these five are live — web serves them and the worker scrapes them.
    // The other 74 stay fully declared in code (nothing deleted or commented
    // out) so bringing one back online is a one-line edit to `activeUkCities`.
    City.ukCities.map(_.slug) shouldBe Seq("london", "manchester", "birmingham", "glasgow", "merseyside")
    City.activeUkCities shouldBe City.ukCities.toSet
    City.allUkCities should have size 79
    City.ukCities.foreach(c => City.allUkCities should contain(c))
    // A disabled city stays modelled in the full roster but is absent from the
    // live list that web + worker read.
    City.allUkCities.map(_.slug) should contain("norwich")
    City.ukCities.map(_.slug) should not contain "norwich"
  }

  "Country.Germany" should "be a German, Filmweb-free deployment (Filmstarts-sourced) on its own database" in {
    Country.Germany.code shouldBe "de"
    Country.Germany.mongoDb shouldBe "kinowo_de"
    Country.Germany.filmwebEnabled shouldBe false
    Country.Germany.language.toLanguageTag shouldBe "de-DE"
    Country.Germany.brandName shouldBe "Showtimes"   // any non-Polish deployment
    Country.Germany.cities shouldBe City.germanCities
    Country.Germany.cities.map(_.slug) should contain allOf ("berlin", "munich", "wurzburg")
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
    // City.all is the concatenation of every country's list (PL + UK + DE).
    City.all should contain theSameElementsAs (City.polishCities ++ City.ukCities ++ City.germanCities)
    Country.all.flatMap(_.cities) should contain theSameElementsAs City.all
  }

  "Country.of and City.country" should "reverse-map each city back to its own country" in {
    Country.of(Poznan) shouldBe Country.Poland
    Warszawa.country shouldBe Country.Poland
    London.country shouldBe Country.UnitedKingdom
    Berlin.country shouldBe Country.Germany
    // Every city belongs to exactly the country whose list contains it.
    City.all.foreach(c => Country.of(c).cities should contain(c))
  }

  "A country's scoped views" should "scope to that country's own cities, a strict subset of the global views" in {
    Country.Poland.bySlug.get("poznan") shouldBe Some(Poznan)
    Country.Poland.bySlug.get("sopot") shouldBe None
    Country.Poland.bySlug.get("london") shouldBe None            // London is a UK city
    Country.UnitedKingdom.bySlug.get("london") shouldBe Some(London)
    Country.Poland.allSorted.toSet shouldBe City.polishCities.toSet
    Country.UnitedKingdom.allSorted.toSet shouldBe City.ukCities.toSet         // the 5 live UK cities
    Country.UnitedKingdom.allSorted.head shouldBe Birmingham                   // English collation A→Z
    Country.UnitedKingdom.allSorted.last shouldBe Merseyside
    Country.Poland.allJson should include("poznan")
    Country.Poland.allJson should not include "london"
  }

  "Country.switchable" should "list only deployed countries (webUrl defined), Poland first, excluding Germany" in {
    // The navbar country <select> iterates this, in this order.
    Country.switchable shouldBe Seq(Country.Poland, Country.UnitedKingdom)
    Country.Poland.webUrl shouldBe Some("https://kinowo.fly.dev")
    Country.UnitedKingdom.webUrl shouldBe Some("https://showtimes-uk.fly.dev")
    // Germany is modelled but has no deployment, so it must be excluded.
    Country.Germany.webUrl shouldBe None
    Country.switchable should not contain Country.Germany
    // Every switchable country carries a host (no trailing slash) and a label.
    Country.switchable.foreach { c =>
      c.webUrl.get should (startWith("https://") and not endWith "/")
      c.displayName should not be empty
    }
  }

  "A country's share-preview (Open Graph) identity" should "carry its own host origin and home-montage filename" in {
    // The `/` landing card and the default og:image are built from these, so a
    // UK deployment previews off showtimes-uk.fly.dev with an English montage
    // (og-home-uk.png) instead of Poland's kinowo.fly.dev / og-home.png.
    Country.Poland.ogOrigin shouldBe "https://kinowo.fly.dev"
    Country.Poland.homeOgImage shouldBe "og-home.png"                    // the default keeps the unsuffixed asset
    Country.UnitedKingdom.ogOrigin shouldBe "https://showtimes-uk.fly.dev"
    Country.UnitedKingdom.homeOgImage shouldBe "og-home-uk.png"
    // Germany is modelled but undeployed: its share links fall back to the
    // default country's host, yet it still gets a per-code montage name.
    Country.Germany.ogOrigin shouldBe Country.Poland.ogOrigin
    Country.Germany.homeOgImage shouldBe "og-home-de.png"
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
}
