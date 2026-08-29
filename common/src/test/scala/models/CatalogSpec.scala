package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * The mobile catalog contract: the JSON lists exactly the DEPLOYED countries
 * (those with a `webUrl` — Poland, UK and Germany) and their cities keyed by the
 * server country code, and the ETag is a stable content hash the apps can
 * `If-None-Match` against.
 */
class CatalogSpec extends AnyFlatSpec with Matchers {

  "Catalog.json" should "list every deployed country, keyed by the server country code" in {
    val j = Catalog.json
    j should include("""{"code":"pl","name":"Polska","baseUrl":"https://kinowo.fly.dev","language":"pl","brand":"Kinowo","timezone":"Europe/Warsaw"}""")
    j should include("""{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes-uk.fly.dev","language":"en","brand":"Showtimes","timezone":"Europe/London"}""")
    j should include("""{"code":"de","name":"Deutschland","baseUrl":"https://showtimes-de.fly.dev","language":"de","brand":"Showtimes","timezone":"Europe/Berlin"}""")
  }

  it should "carry each deployed country's local IANA timezone" in {
    val j = Catalog.json
    // The field the mobile apps read to prune past showtimes on local
    // wall-clock — a London show disappears on Europe/London, not Warsaw.
    j should include(""""code":"uk"""")
    j should include(""""timezone":"Europe/London"""")
    j should include(""""timezone":"Europe/Berlin"""")
  }

  it should "carry each city with its owning country's code" in {
    val j = Catalog.json
    j should include("""{"slug":"poznan","name":"Poznań","lat":52.4064,"lon":16.9252,"country":"pl"}""")
    j should include("""{"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk"}""")
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
