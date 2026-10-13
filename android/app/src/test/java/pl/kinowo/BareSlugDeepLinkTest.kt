package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.model.Catalog
import pl.kinowo.model.city
import pl.kinowo.model.currentSlugByFormer
import java.io.File

/**
 * Deep links resolved against the bundled seed — the server's own catalog — so
 * the app opens what the web would: a retired slug another country has live, a
 * town a page covers, and a bare slug that names no country, which the server's
 * `bareSlugOwners` decides.
 */
class BareSlugDeepLinkTest {

    private val catalog: Catalog = run {
        // Gradle runs unit tests with user.dir at the module dir; tolerate the
        // repo-root / android-root cases too.
        val base = File(System.getProperty("user.dir") ?: ".")
        val seed = listOf("src/main/assets", "app/src/main/assets", "android/app/src/main/assets")
            .map { base.resolve("$it/catalog-seed.json") }
            .firstOrNull { it.isFile } ?: error("catalog-seed.json not found from $base")
        Catalog.parseSeed(seed.readText())!!.second
    }

    private fun parse(url: String) =
        DeepLink.parse(url, cities = catalog.cities, bareSlugOwners = catalog.bareSlugOwners)

    @Test fun aRetiredSlugAnotherCountryHasLiveOpensTheNamedCountrysPage() {
        val link = parse("https://showtimes.cc/uk/kent/movie/dune")!!
        assertEquals("uk", link.countryCode)
        assertEquals("chatham", link.citySlug)
        assertEquals("dune", link.filmSlug)
        assertEquals("chatham", catalog.cities.currentSlugByFormer("uk")["kent"])
        assertNull(catalog.cities.currentSlugByFormer("us")["kent"])
    }

    @Test fun aTownAliasOpensThePageCoveringIt() {
        val link = parse("https://showtimes.cc/uk/ilkley/")!!
        assertEquals("uk", link.countryCode)
        assertNotEquals("ilkley", link.citySlug)
        assertNotNull(catalog.cities.city(link.citySlug, "uk"))
        assertEquals("york", parse("https://showtimes.cc/uk/york-north-yorkshire/")!!.citySlug)
    }

    @Test fun aBareBrandSlugOpensTheCountryTheServerChose() {
        val owners = catalog.bareSlugOwners.apex
        assertEquals("us", owners["boston"])
        assertEquals("us", parse("https://showtimes.cc/boston/")?.countryCode)
        assertEquals("uk", parse("https://showtimes.cc/york/")?.countryCode)
        assertEquals("uk", parse("https://showtimes.cc/crosby/")?.countryCode)
        assertEquals("uk", parse("https://showtimes.cc/somerset/")?.countryCode)
        for (slug in listOf("durham", "elgin", "hamilton", "lancaster", "andover", "kilmarnock")) {
            assertEquals(slug, owners[slug], parse("https://showtimes.cc/$slug/")?.countryCode)
        }
        for ((slug, code) in owners) assertEquals(slug, code, parse("https://showtimes.cc/$slug/")?.countryCode)
        assertEquals("uk", parse("https://showtimes.cc/harrogate/")?.countryCode)
    }

    @Test fun aCustomSchemeSlugOpensTheCountryTheServerChose() {
        assertEquals("pl", parse("kinowo://poznan/")?.countryCode)
        assertEquals("us", parse("kinowo://boston/movie/dune")?.countryCode)
        for ((slug, code) in catalog.bareSlugOwners.scheme) assertEquals(slug, code, parse("kinowo://$slug/")?.countryCode)
    }
}
