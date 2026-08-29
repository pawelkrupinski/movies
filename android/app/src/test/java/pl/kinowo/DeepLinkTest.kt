package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption

/**
 * [DeepLink.parse] is the inverse of the web's `buildShareURL()`: every
 * kinowo.net URL the site produces must round back into the right city /
 * film / filter state, and anything that ISN'T a city link (OAuth callbacks,
 * unknown hosts, foreign paths) must be rejected so the activity no-ops.
 */
class DeepLinkTest {

    // MARK: city + film identity

    @Test fun cityListingLink() {
        val dl = DeepLink.parse("https://kinowo.net/poznan/")!!
        assertEquals("poznan", dl.citySlug)
        assertNull(dl.filmTitle)
        assertTrue(dl.filters.isEmpty)
    }

    @Test fun filmDetailSlugLink() {
        // The canonical form the site and the app both mint now.
        val dl = DeepLink.parse("https://kinowo.net/warszawa/film/oppenheimer")!!
        assertEquals("warszawa", dl.citySlug)
        assertEquals("oppenheimer", dl.filmSlug)
        assertNull(dl.filmTitle)
    }

    @Test fun filmDetailSlugLinkOnTheCustomScheme() {
        val dl = DeepLink.parse("kinowo://poznan/film/diuna-czesc-druga")!!
        assertEquals("poznan", dl.citySlug)
        assertEquals("diuna-czesc-druga", dl.filmSlug)
    }

    @Test fun bareFilmPathIsNeitherSlugNorTitle() {
        val dl = DeepLink.parse("https://kinowo.net/warszawa/film")!!
        assertNull(dl.filmSlug)
        assertNull(dl.filmTitle)
    }

    @Test fun filmDetailLink() {
        // The legacy form: still parsed, because links shared before the switch
        // — and by app builds still in the wild — carry it.
        val dl = DeepLink.parse("https://kinowo.net/warszawa/film?title=Oppenheimer")!!
        assertEquals("warszawa", dl.citySlug)
        assertEquals("Oppenheimer", dl.filmTitle)
        assertNull(dl.filmSlug)
    }

    @Test fun filmDetailDecodesEncodedTitle() {
        val dl = DeepLink.parse("https://kinowo.net/wroclaw/film?title=Lilo%20%26%20Stitch")!!
        assertEquals("Lilo & Stitch", dl.filmTitle)
    }

    @Test fun filmDetailParsesEncodedPolishTitle() {
        val dl = DeepLink.parse("https://kinowo.net/poznan/film?title=Minionki%20i%20straszyd%C5%82a")!!
        assertEquals("poznan", dl.citySlug)
        assertEquals("Minionki i straszydła", dl.filmTitle)
    }

    // MIUI/Xiaomi hands the activity the App Link with the query already
    // percent-DECODED, so it arrives with literal spaces (and raw Polish chars).
    // The strict java.net.URI(String) constructor threw URISyntaxException on
    // those → parse returned null → the film page never opened (the app stayed
    // on the current/main screen). parse() must tolerate the decoded delivery.
    @Test fun filmDetailParsesTitleDeliveredDecoded() {
        val dl = DeepLink.parse("https://kinowo.net/poznan/film?title=Minionki i straszydła")!!
        assertEquals("poznan", dl.citySlug)
        assertEquals("Minionki i straszydła", dl.filmTitle)
    }

    @Test fun filmDetailParsesTitleDeliveredDecodedCustomScheme() {
        val dl = DeepLink.parse("kinowo://poznan/film?title=Minionki i straszydła")!!
        assertEquals("poznan", dl.citySlug)
        assertEquals("Minionki i straszydła", dl.filmTitle)
    }

    @Test fun customSchemeCityAndFilm() {
        assertEquals("poznan", DeepLink.parse("kinowo://poznan/")!!.citySlug)
        assertEquals("Wicked", DeepLink.parse("kinowo://krakow/film?title=Wicked")!!.filmTitle)
    }

    // MARK: rejection

    @Test fun rejectsOAuthCallback() {
        assertNull(DeepLink.parse("kinowo://auth-done?code=abc"))
        assertNull(DeepLink.parse("https://kinowo.net/auth/google/callback?code=abc"))
    }

    @Test fun rejectsUnknownCityHostAndScheme() {
        assertNull(DeepLink.parse("https://kinowo.net/uptime"))
        assertNull(DeepLink.parse("https://kinowo.net/nieznane-miasto/"))
        assertNull(DeepLink.parse("https://evil.example.com/poznan/"))
        assertNull(DeepLink.parse("mailto:hi@kinowo.net"))
    }

    @Test fun emptyTitleParamIsNoFilm() {
        assertNull(DeepLink.parse("https://kinowo.net/poznan/film?title=")!!.filmTitle)
    }

    @Test fun ukDeploymentHostOpensInApp() {
        val dl = DeepLink.parse("https://uk.showtimes.cc/london/film?title=Wicked")!!
        assertEquals("london", dl.citySlug)
        assertEquals("Wicked", dl.filmTitle)
    }

    @Test fun deDeploymentHostOpensInApp() {
        // No German city ships in the compile-time `Cities.all` fallback (they
        // arrive via the live catalog), so pass the slug set the ViewModel hands
        // in at runtime (`countryCatalog.value.cities`) — as `handleDeepLink` does.
        val dl = DeepLink.parse("https://de.showtimes.cc/berlin/", setOf("berlin"))!!
        assertEquals("berlin", dl.citySlug)
    }

    // MARK: scalar filters

    @Test fun scalarFilters() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?date=tomorrow&q=duna&dim=2D&lang=NAP&imax=1&from=18:30&sort=rating")!!.filters
        assertEquals(DateFilter.Tomorrow, f.date)
        assertEquals("duna", f.query)
        assertEquals("2D", f.dimension)
        assertEquals("NAP", f.language)
        assertEquals(true, f.imax)
        assertEquals(18, f.fromHour)
        assertEquals(30, f.fromMinute)
        assertEquals(SortOption.RATING, f.sort)
    }

    @Test fun isoDateFilter() {
        assertEquals(DateFilter.Specific("2026-07-01"), DeepLink.parse("https://kinowo.net/poznan/?date=2026-07-01")!!.filters.date)
    }

    @Test fun formatFilterMergesOntoBase() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?dim=3D")!!.filters
        val merged = f.formatFilter(FormatFilter(language = "DUB"))
        assertEquals("3D", merged.dimension)
        assertEquals("DUB", merged.language)
    }

    @Test fun rejectsGarbageScalarValues() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?dim=4D&lang=XX&from=99:99&date=lolwut")!!.filters
        assertNull(f.dimension)
        assertNull(f.language)
        assertNull(f.fromHour)
        assertNull(f.date)
    }

    // MARK: multi-value inclusion → exclusion

    @Test fun repeatedAndCommaListInclusionFlatten() {
        val repeated = DeepLink.parse("https://kinowo.net/poznan/?genre=Komedia&genre=Dramat")!!.filters
        val comma = DeepLink.parse("https://kinowo.net/poznan/?genre=Komedia,Dramat")!!.filters
        assertEquals(setOf("Komedia", "Dramat"), repeated.includedGenres.toSet())
        assertEquals(setOf("Komedia", "Dramat"), comma.includedGenres.toSet())
    }

    @Test fun inclusionConvertsToExclusionAgainstUniverse() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?country=USA&country=Polska")!!.filters
        val universe = setOf("USA", "Polska", "Francja", "Niemcy")
        assertEquals(setOf("Francja", "Niemcy"), f.excluded(f.includedCountries, universe))
    }

    @Test fun emptyInclusionMeansNoExclusion() {
        val f = DeepLink.parse("https://kinowo.net/poznan/")!!.filters
        assertEquals(emptySet<String>(), f.excluded(f.includedCountries, setOf("USA", "Polska")))
    }

    @Test fun cinemaParamInvertsToDisabledSet() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?cinema=Kino%20Muza&cinema=Rialto")!!.filters
        val all = setOf("Kino Muza", "Rialto", "Multikino", "Apollo")
        assertEquals(setOf("Multikino", "Apollo"), f.disabledCinemas(all))
    }

    @Test fun absentCinemaParamLeavesChoiceAlone() {
        val f = DeepLink.parse("https://kinowo.net/poznan/?dim=2D")!!.filters
        assertNull(f.enabledCinemas)
        assertNull(f.disabledCinemas(setOf("A", "B")))
    }
}
