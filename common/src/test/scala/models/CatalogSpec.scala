package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * The mobile catalog contract: the JSON lists exactly the DEPLOYED countries
 * (those with a `webUrl` — Poland alone since the UK and German deployments were
 * stopped on 2026-08-02) and their cities keyed by the server country code, and
 * the ETag is a stable content hash the apps can `If-None-Match` against.
 */
class CatalogSpec extends AnyFlatSpec with Matchers {

  "Catalog.json" should "list every deployed country, keyed by the server country code" in {
    val j = Catalog.json
    j should include("""{"code":"pl","name":"Polska","baseUrl":"https://kinowo.fly.dev","language":"pl","brand":"Kinowo","timezone":"Europe/Warsaw"}""")
  }

  it should "omit the countries no deployment serves" in {
    val j = Catalog.json
    // This payload IS the mobile apps' country list, so a stopped deployment
    // must not appear in it — otherwise both pickers keep offering a host that
    // no longer answers.
    j should not include """"code":"uk""""
    j should not include """"code":"de""""
    j should not include "showtimes-uk.fly.dev"
    j should not include "showtimes-de.fly.dev"
  }

  it should "carry each deployed country's local IANA timezone" in {
    val j = Catalog.json
    // The field the mobile apps read to prune past showtimes on local
    // wall-clock — a Warsaw show disappears on Europe/Warsaw.
    j should include(""""code":"pl"""")
    j should include(""""timezone":"Europe/Warsaw"""")
  }

  it should "carry each city with its owning country's code" in {
    val j = Catalog.json
    j should include("""{"slug":"poznan","name":"Poznań","lat":52.4064,"lon":16.9252,"country":"pl"}""")
    // London belongs to the undeployed UK, so its city row is gone too — the
    // apps must not offer a city they can't fetch a repertoire for.
    j should not include """"slug":"london""""
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
