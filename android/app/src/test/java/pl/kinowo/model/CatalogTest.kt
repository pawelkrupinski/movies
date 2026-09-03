package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * The catalog decode: a `/api/catalog` 200 body and the bundled seed envelope
 * both parse to a [Catalog]; the country wire shape maps to [Country], cities
 * decode directly. Mirrors the iOS `CityDecodeTests`/`CountryDTOTests`.
 */
class CatalogTest {

    @Test
    fun parsesResponseBody() {
        val json = """{"countries":[{"code":"uk","name":"United Kingdom","baseUrl":"https://uk.showtimes.cc","language":"en","brand":"Showtimes"}],"cities":[{"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk"}]}"""
        val c = Catalog.parseBody(json)!!
        assertEquals(1, c.countries.size)
        assertEquals("uk", c.countries[0].code)
        assertEquals("United Kingdom", c.countries[0].displayName)
        assertEquals("https://uk.showtimes.cc", c.countries[0].baseUrl)
        assertEquals("en", c.countries[0].languageTag)
        assertEquals(1, c.cities.size)
        assertEquals("london", c.cities[0].slug)
        assertEquals("uk", c.cities[0].country)
    }

    @Test
    fun parsesSeedEnvelopeWithEtag() {
        val seed = """{"etag":"\"abc123\"","catalog":{"countries":[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.net","language":"pl","brand":"Kinowo"}],"cities":[{"slug":"poznan","name":"Poznań","lat":52.4,"lon":16.9,"country":"pl"}]}}"""
        val (etag, cat) = Catalog.parseSeed(seed)!!
        assertEquals("\"abc123\"", etag)   // the ETag survives, with its quotes, for If-None-Match
        assertEquals("poznan", cat.cities[0].slug)
        assertEquals("pl", cat.countries[0].code)
    }

    @Test
    fun returnsNullOnGarbage() {
        assertNull(Catalog.parseBody("{not json"))
        assertNull(Catalog.parseSeed("nope"))
    }

    /**
     * A city carries its OWN zone only where it differs from its country's, so
     * most cities have none and must fall back rather than fail to decode.
     */
    @Test
    fun parsesPerCityTimezoneWhenPresent() {
        val json = """{"countries":[{"code":"us","name":"United States","baseUrl":"https://showtimes.cc/us","language":"en","brand":"Showtimes","timezone":"America/Los_Angeles"}],"cities":[{"slug":"knoxville","name":"Knoxville","lat":35.9,"lon":-83.9,"country":"us","region":"Tennessee","timezone":"America/New_York"},{"slug":"los-angeles","name":"Los Angeles","lat":34.05,"lon":-118.3,"country":"us","region":"California"}]}"""
        val c = Catalog.parseBody(json)!!
        assertEquals("America/New_York", c.cities[0].timezone)
        assertNull(c.cities[1].timezone)
    }

    /**
     * The country's zone is the fallback, and the point of the field is that a US
     * metro does NOT take it: Tennessee is Central-predominant, so a Knoxville
     * showtime pruned on the US country zone (Pacific, its biggest city) would go
     * three hours wrong.
     */
    @Test
    fun cityZoneFallsBackToTheCountrysOnlyWhenAbsent() {
        val pacific = java.time.ZoneId.of("America/Los_Angeles")
        val cities = listOf(
            City(slug = "knoxville", name = "Knoxville", lat = 35.9, lon = -83.9, country = "us",
                 timezone = "America/New_York"),
            City(slug = "los-angeles", name = "Los Angeles", lat = 34.05, lon = -118.3, country = "us"),
            City(slug = "bogus", name = "Bogus", lat = 0.0, lon = 0.0, country = "us",
                 timezone = "Mars/Olympus"),
        )
        assertEquals(java.time.ZoneId.of("America/New_York"), cities.zoneFor("knoxville", pacific))
        assertEquals(pacific, cities.zoneFor("los-angeles", pacific))
        // A slug this catalog does not know, no slug at all, and an identifier the
        // platform cannot parse all fall back — never a crash on a saved selection.
        assertEquals(pacific, cities.zoneFor("nowhere", pacific))
        assertEquals(pacific, cities.zoneFor(null, pacific))
        assertEquals(pacific, cities.zoneFor("bogus", pacific))
    }
}
