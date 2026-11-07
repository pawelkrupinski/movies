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

    /** A link the web minted while venues printed their own spelling: the web 301s
     *  `?genre=przygoda` onto "Przygodowy", but the app never sees that redirect.
     *  Read through the catalog's genre table it keeps its genre's served name. */
    @Test fun anOldGenreSpellingKeepsItsGenreThroughTheCatalogTable() {
        val link = DeepLink.parse("https://kinowo.net/poznan/?genre=przygoda&genre=FILM%20TV&genre=Adaptacja",
            cities = catalog.cities, genreSpellings = catalog.genreSpellings)!!.filters
        val universe = setOf("Przygodowy", "Film TV", "Adaptacja", "Dramat")
        assertEquals(setOf("Dramat"), link.excluded(link.includedGenres, universe))
        // An older server's catalog has no table: the link's values as they are.
        assertEquals(listOf("przygoda"), DeepLink.parse("https://kinowo.net/poznan/?genre=przygoda")!!.filters.includedGenres)
    }

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

    /** A page slug two countries share: the catalog names the UK's York
     *  `york-north-yorkshire` for installed builds, and this build reads its
     *  `pageSlug` — so `/uk/york/` opens the UK's York and `/us/york/` Pennsylvania's. */
    @Test fun aSharedPageSlugOpensEachCountrysOwnPage() {
        val uk = parse("https://showtimes.cc/uk/york/")!!
        assertEquals("uk", uk.countryCode)
        assertEquals("york", uk.citySlug)
        assertEquals(53.96, catalog.cities.city("york", "uk")!!.lat, 0.1)
        val us = parse("https://showtimes.cc/us/york/movie/dune")!!
        assertEquals("us", us.countryCode)
        assertEquals("york", us.citySlug)
        assertEquals(39.96, catalog.cities.city("york", "us")!!.lat, 0.1)
    }

    @Test fun aBareSlugOpensTheCountryTheServersTableNames() {
        val owners = catalog.bareSlugOwners
        // The old owner, though the UK has a York page now (catalog order would say UK).
        assertEquals("us", owners["york"])
        assertEquals("us", parse("https://showtimes.cc/york/")?.countryCode)
        assertEquals("us", parse("kinowo://york/movie/dune")?.countryCode)
        assertEquals("uk", parse("https://showtimes.cc/somerset/")?.countryCode)
        assertEquals("pl", parse("kinowo://poznan/")?.countryCode)
        for ((slug, code) in owners) {
            assertEquals(slug, code, parse("https://showtimes.cc/$slug/")?.countryCode)
            assertEquals(slug, code, parse("kinowo://$slug/")?.countryCode)
        }
        // Not in the table: the one country that has it, else the first in order.
        assertNull(owners["harrogate"])
        assertEquals("uk", parse("https://showtimes.cc/harrogate/")?.countryCode)
        assertNull(owners["leeds"])
        assertEquals("uk", parse("https://showtimes.cc/leeds/")?.countryCode)
    }
}
