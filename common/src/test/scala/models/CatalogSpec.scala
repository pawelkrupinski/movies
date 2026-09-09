package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * The mobile catalog contract: the JSON lists exactly the DEPLOYED countries
 * (those with a `webUrl` — all five) and their cities keyed by the server
 * country code, and the ETag is a stable content hash the apps can
 * `If-None-Match` against.
 */
class CatalogSpec extends AnyFlatSpec with Matchers {

  "Catalog.json" should "list every deployed country, keyed by the server country code" in {
    val j = Catalog.json
    j should include("""{"code":"pl","name":"Polska","baseUrl":"https://kinowo.net","language":"pl","brand":"Kinowo","timezone":"Europe/Warsaw","versionTokens":{"subtitled":"NAP","dubbed":"DUB"}}""")
    j should include("""{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes.cc/uk","language":"en","brand":"Showtimes","timezone":"Europe/London","versionTokens":{"subtitled":"SUB","dubbed":"DUB"}}""")
    j should include("""{"code":"de","name":"Deutschland","baseUrl":"https://showtimes.cc/de","language":"de","brand":"Showtimes","timezone":"Europe/Berlin","versionTokens":{"subtitled":"OmU","dubbed":"DF"}}""")
  }

  it should "carry each country's OWN subtitled/dubbed tokens, and none where it has none" in {
    // The apps' "version" filter matches a literal `Showtime.format` token, and
    // until this field existed both hardcoded Poland's pair — so a German user
    // picking "subtitles" filtered on `NAP` and saw nothing. Each country's entry
    // names the pair its own scrapers emit; a country that marks neither omits
    // the field, and the apps then hide the row rather than offer a dead filter.
    def countryEntry(code: String) =
      Catalog.json.split("\\{\"code\"").find(_.startsWith(s""":"$code"""")).getOrElse(fail(s"no $code entry"))
    countryEntry("es") should include(""""versionTokens":{"subtitled":"VOSE","dubbed":"DOB"}""")
    countryEntry("us") should include(""""versionTokens":{"subtitled":"SUB","dubbed":"DUB"}""")
    Country.switchable.foreach { c =>
      countryEntry(c.code).contains("versionTokens") shouldBe c.versionTokens.isDefined
    }
  }

  it should "carry each deployed country's local IANA timezone" in {
    val j = Catalog.json
    // The field the mobile apps read to prune past showtimes on local
    // wall-clock — a London show disappears on Europe/London, not Warsaw.
    j should include(""""code":"uk"""")
    j should include(""""timezone":"Europe/London"""")
    j should include(""""timezone":"Europe/Berlin"""")
  }

  it should "take a multi-zone country's from its BIGGEST city, not its first" in {
    // Four countries keep one zone throughout, so any city answers for them. The
    // US spans six, and this field read whichever city sorted first — making a
    // live value a function of ROSTER ORDER. A generator change that reshuffled
    // the states once moved it from Chicago to Pacific/Pago_Pago, American Samoa's,
    // and every suite stayed green. Pinned here so the next reshuffle says so.
    val us = Catalog.json.split("\\{").find(_.contains(""""code":"us"""")).getOrElse(fail("no us entry"))
    us should include(""""timezone":"America/Los_Angeles"""")   // Los Angeles, 133 venues
    Country.UnitedStates.cities.maxBy(c => (c.cinemas.size, c.slug)).slug shouldBe "los-angeles"
    // …and a single-zone country is unaffected by which city is biggest.
    Catalog.json should include(""""code":"pl"""")
    Catalog.json should include(""""timezone":"Europe/Warsaw"""")
  }

  it should "give a city its own zone only where it differs from its country's" in {
    val j = Catalog.json
    // A single-zone country writes none at all: the field is what a client falls
    // back FROM, so emitting it where it would repeat the country's says nothing
    // and costs bytes on every city of four countries out of five.
    j should include("""{"slug":"poznan","name":"Poznań","lat":52.4064,"lon":16.9252,"country":"pl"}""")
    j should not include """"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk","timezone""""
    // The US spans six zones, so every metro off the country's own carries its.
    // This is the whole point: an app pruning a Knoxville showtime on the
    // country's Pacific would drop it three hours early.
    def cityEntry(slug: String) =
      j.split("\\{").find(_.contains(s""""slug":"$slug"""")).getOrElse(fail(s"no $slug entry"))
    cityEntry("knoxville") should include(""""timezone":"America/New_York"""")
    cityEntry("el-paso")   should include(""""timezone":"America/Denver"""")
    cityEntry("juneau")    should include(""""timezone":"America/Juneau"""")
    cityEntry("oahu")      should include(""""timezone":"Pacific/Honolulu"""")
    // …and a US metro that IS on the country's zone stays silent.
    cityEntry("los-angeles") should not include "timezone"
  }

  it should "carry each city with its owning country's code" in {
    val j = Catalog.json
    j should include("""{"slug":"poznan","name":"Poznań","lat":52.4064,"lon":16.9252,"country":"pl"}""")
    j should include("""{"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk","region":"England"}""")
  }

  it should "name the group a GROUPED country finds a city through, and only there" in {
    val j = Catalog.json
    def cityEntry(slug: String) =
      j.split("\\{").find(_.contains(s""""slug":"$slug"""")).getOrElse(fail(s"no $slug entry"))
    // The apps offer the same two-step pick the web does, so they need the same
    // headings: the US by state, the UK by nation.
    cityEntry("los-angeles") should include(""""region":"California"""")
    cityEntry("glasgow")     should include(""""region":"Scotland"""")
    cityEntry("cardiff")     should include(""""region":"Wales"""")
    cityEntry("muenchen")    should include(""""region":"Bayern"""")
    // The TOP level, where the web nests deeper: `region` is one string and the
    // apps' pick is two steps, so a UK city names its NATION, not the county the
    // page puts between them.
    cityEntry("cheshire") should include(""""region":"England"""")
    cityEntry("cheshire") should not include "West Midlands"
    // A flat country sends nothing — a name is all its visitor needs, and the
    // field costs bytes on every city where it would say nothing.
    cityEntry("poznan") should not include "region"
    cityEntry("madrid") should not include "region"
  }

  it should "carry no region where the TOP group collapsed onto its one city" in {
    val j = Catalog.json
    def cityEntry(slug: String) =
      j.split("\\{").find(_.contains(s""""slug":"$slug"""")).getOrElse(fail(s"no $slug entry"))
    // Berlin and Hamburg are Germany's single-region city-states; Delaware and
    // Vermont are US states too small to split into metros. Naming their one
    // Bundesland/state as a `region` a visitor would only ever open to reach
    // the single thing under it costs a tap for nothing — this is the same
    // `CityGroup.soleCity` collapse `region` already applies one level down
    // (a UK county with one place), now applied at the top too.
    cityEntry("berlin")   should not include "region"
    cityEntry("hamburg")  should not include "region"
    cityEntry("delaware") should not include "region"
    cityEntry("vermont")  should not include "region"
    // Bremen genuinely groups two places (Bremen, Bremerhaven), so it keeps
    // its region step.
    cityEntry("bremen")      should include(""""region":"Bremen"""")
    cityEntry("bremerhaven") should include(""""region":"Bremen"""")
  }

  it should "name the SUB-group only where it holds more than one city" in {
    val j = Catalog.json
    def cityEntry(slug: String) =
      j.split("\\{").find(_.contains(s""""slug":"$slug"""")).getOrElse(fail(s"no $slug entry"))
    // West Midlands, Glamorgan and Antrim are the three UK counties that group
    // more than one place — the level `region` alone can't carry (it names only
    // the nation, "England"/"Wales"/"Northern Ireland").
    cityEntry("birmingham") should include(""""region":"England","subregion":"West Midlands"""")
    cityEntry("dudley")     should include(""""subregion":"West Midlands"""")
    cityEntry("sandwell")   should include(""""subregion":"West Midlands"""")
    cityEntry("cardiff")    should include(""""region":"Wales","subregion":"Glamorgan"""")
    cityEntry("glamorgan")  should include(""""subregion":"Glamorgan"""")
    cityEntry("antrim")     should include(""""region":"Northern Ireland","subregion":"Antrim"""")
    cityEntry("belfast")    should include(""""subregion":"Antrim"""")
    // A county that collapsed onto its one place carries no subregion — Cheshire
    // reads correctly through `region` alone, with no extra tap to reach it.
    cityEntry("cheshire") should not include "subregion"
    cityEntry("glasgow")  should not include "subregion"
    // Germany and the US group one level deep, so neither ever writes it.
    cityEntry("muenchen")     should not include "subregion"
    cityEntry("los-angeles")  should not include "subregion"
    // The flat countries write neither region nor subregion.
    cityEntry("poznan") should not include "subregion"
  }

  it should "contain exactly the switchable countries' cities" in {
    val cityCount = Catalog.json.split("\"slug\":", -1).length - 1
    cityCount shouldBe Country.switchable.flatMap(_.cities).size
  }

  "Catalog.etag" should "be a quoted 16-hex-char SHA-256 prefix of the json body" in {
    val expected = "\"" + MessageDigest.getInstance("SHA-256")
      .digest(Catalog.json.getBytes(StandardCharsets.UTF_8))
      .take(8)
      .map("%02x".format(_))
      .mkString + "\""
    Catalog.etag shouldBe expected
    Catalog.etag should fullyMatch regex "\"[0-9a-f]{16}\""
  }
}
