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
        val json = """{"countries":[{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes-uk.fly.dev","language":"en","brand":"Showtimes"}],"cities":[{"slug":"london","name":"London","lat":51.5074,"lon":-0.1278,"country":"uk"}]}"""
        val c = Catalog.parseBody(json)!!
        assertEquals(1, c.countries.size)
        assertEquals("uk", c.countries[0].code)
        assertEquals("United Kingdom", c.countries[0].displayName)
        assertEquals("https://showtimes-uk.fly.dev", c.countries[0].baseUrl)
        assertEquals("en", c.countries[0].languageTag)
        assertEquals(1, c.cities.size)
        assertEquals("london", c.cities[0].slug)
        assertEquals("uk", c.cities[0].country)
    }

    @Test
    fun parsesSeedEnvelopeWithEtag() {
        val seed = """{"etag":"\"abc123\"","catalog":{"countries":[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.fly.dev","language":"pl","brand":"Kinowo"}],"cities":[{"slug":"poznan","name":"Poznań","lat":52.4,"lon":16.9,"country":"pl"}]}}"""
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
}
