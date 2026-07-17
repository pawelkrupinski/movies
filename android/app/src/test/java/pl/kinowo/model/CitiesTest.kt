package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Pins the per-country location → city mapping used by the first-launch gate:
 * near a supported city (in the SELECTED country) we resolve to it; far from
 * every one we resolve to null so the gate falls back to an explicit pick. The
 * catalogue is the global union of Polish + UK cities, scoped per country.
 */
class CitiesTest {

    @Test
    fun resolvesPoznanFromItsOwnCoordinates() {
        val city = Cities.nearestWithin100km(52.4064, 16.9252, "pl")
        assertEquals("poznan", city?.slug)
    }

    @Test
    fun resolvesPoznanFromNearby() {
        // ~30 km outside Poznań is still within range.
        val city = Cities.nearestWithin100km(52.40, 16.50, "pl")
        assertEquals("poznan", city?.slug)
    }

    @Test
    fun resolvesEachSupportedCityFromItsOwnCoordinates() {
        assertEquals("wroclaw", Cities.nearestWithin100km(51.1079, 17.0385, "pl")?.slug)
        assertEquals("warszawa", Cities.nearestWithin100km(52.2297, 21.0122, "pl")?.slug)
        assertEquals("krakow", Cities.nearestWithin100km(50.0647, 19.9450, "pl")?.slug)
        assertEquals("lodz", Cities.nearestWithin100km(51.7592, 19.4560, "pl")?.slug)
        assertEquals("katowice", Cities.nearestWithin100km(50.2649, 19.0238, "pl")?.slug)
        assertEquals("szczecin", Cities.nearestWithin100km(53.4285, 14.5528, "pl")?.slug)
        assertEquals("bialystok", Cities.nearestWithin100km(53.1325, 23.1688, "pl")?.slug)
        assertEquals("bydgoszcz", Cities.nearestWithin100km(53.1235, 18.0084, "pl")?.slug)
        assertEquals("lublin", Cities.nearestWithin100km(51.2465, 22.5684, "pl")?.slug)
        assertEquals("czestochowa", Cities.nearestWithin100km(50.8118, 19.1203, "pl")?.slug)
        assertEquals("radom", Cities.nearestWithin100km(51.4027, 21.1471, "pl")?.slug)
        assertEquals("sosnowiec", Cities.nearestWithin100km(50.2863, 19.1041, "pl")?.slug)
        assertEquals("torun", Cities.nearestWithin100km(53.0138, 18.5984, "pl")?.slug)
        assertEquals("kielce", Cities.nearestWithin100km(50.8661, 20.6286, "pl")?.slug)
        assertEquals("rzeszow", Cities.nearestWithin100km(50.0413, 21.9990, "pl")?.slug)
        assertEquals("gliwice", Cities.nearestWithin100km(50.2945, 18.6714, "pl")?.slug)
        assertEquals("zabrze", Cities.nearestWithin100km(50.3249, 18.7857, "pl")?.slug)
        // Both ends of the Tri-City resolve to the combined Trójmiasto scope.
        assertEquals("trojmiasto", Cities.nearestWithin100km(54.3520, 18.6466, "pl")?.slug) // Gdańsk
        assertEquals("trojmiasto", Cities.nearestWithin100km(54.5189, 18.5305, "pl")?.slug) // Gdynia
    }

    // ── UK cities + per-country isolation ─────────────────────────

    @Test
    fun resolvesUkCitiesUnderGB() {
        assertEquals("london", Cities.nearestWithin100km(51.5074, -0.1278, "uk")?.slug)
        assertEquals("manchester", Cities.nearestWithin100km(53.4808, -2.2426, "uk")?.slug)
        assertEquals("glasgow", Cities.nearestWithin100km(55.8682, -4.2316, "uk")?.slug)
    }

    @Test
    fun nearestIsScopedToTheSelectedCountry() {
        // A London fix must NOT resolve to any Polish city, and a Poznań fix must
        // NOT resolve to any UK region — each gate only offers its own cities.
        assertNull(Cities.nearestWithin100km(51.5074, -0.1278, "pl"))
        assertNull(Cities.nearestWithin100km(52.4064, 16.9252, "uk"))
    }

    @Test
    fun ukRosterIsTheFullSeventyNineRegions() {
        assertEquals(79, Cities.citiesIn("uk").size)
        assertEquals("london", Cities.citiesIn("uk").first().slug) // hand order
        assertTrue(Cities.citiesIn("uk").all { it.country == "uk" })
    }

    @Test
    fun ukSortedIsAlphabeticalUnderEnglishCollation() {
        val sorted = Cities.sortedIn("uk")
        assertEquals("aberdeenshire", sorted.first().slug)
        assertEquals("yorkshire", sorted.last().slug)
        assertEquals(Cities.citiesIn("uk").map { it.slug }.toSet(), sorted.map { it.slug }.toSet())
    }

    @Test
    fun ukMatchingSearchesUkCitiesOnly() {
        assertEquals(listOf("manchester"), Cities.matching("manch", "uk").map { it.slug })
        assertTrue(Cities.matching("poznan", "uk").isEmpty())
        val yorks = Cities.matching("york", "uk").map { it.slug }
        assertTrue(yorks.contains("east-yorkshire"))
        assertTrue(yorks.contains("yorkshire"))
    }

    // ── catalogue (global union, per-country order) ───────────────

    @Test
    fun allIsTheGlobalUnionOfPolishAndUkCities() {
        assertEquals(120, Cities.all.size)               // 41 PL + 79 GB
        assertEquals(41, Cities.citiesIn("pl").size)
        assertEquals(79, Cities.citiesIn("uk").size)
    }

    @Test
    fun defaultCityIsThatCountrysFirstCity() {
        assertEquals("poznan", Cities.defaultCityIn("pl").slug)
        assertEquals("london", Cities.defaultCityIn("uk").slug)
        assertEquals("poznan", Cities.DEFAULT.slug)
    }

    @Test
    fun listsAllFortyOnePolishCitiesInOrder() {
        assertEquals(
            listOf(
                "poznan", "wroclaw", "warszawa", "krakow", "lodz", "katowice", "szczecin",
                "bialystok", "trojmiasto", "bydgoszcz", "lublin", "czestochowa", "radom",
                "sosnowiec", "torun", "kielce", "rzeszow", "gliwice", "zabrze",
                "olsztyn", "bielsko-biala", "opole", "rybnik", "gorzow-wielkopolski", "elblag",
                "koszalin", "kalisz", "zielona-gora", "tychy", "walbrzych", "tarnow", "wloclawek",
                "legnica", "plock", "bytom", "dabrowa-gornicza", "nowy-sacz", "slupsk",
                "jelenia-gora", "przemysl", "konin",
            ),
            Cities.citiesIn("pl").map { it.slug },
        )
    }

    @Test
    fun polishSortedIsAlphabeticalUnderPolishCollation() {
        // Same cities as `citiesIn("pl")`, just reordered for the UI pickers.
        assertEquals(Cities.citiesIn("pl").map { it.slug }.toSet(), Cities.sortedIn("pl").map { it.slug }.toSet())
        assertEquals(
            listOf(
                "bialystok", "bielsko-biala", "bydgoszcz", "bytom", "czestochowa",
                "dabrowa-gornicza", "elblag", "gliwice", "gorzow-wielkopolski", "jelenia-gora",
                "kalisz", "katowice", "kielce", "konin", "koszalin", "krakow",
                "legnica", "lublin", "lodz", "nowy-sacz", "olsztyn", "opole",
                "plock", "poznan", "przemysl", "radom", "rybnik", "rzeszow",
                "slupsk", "sosnowiec", "szczecin", "tarnow", "torun", "trojmiasto",
                "tychy", "walbrzych", "warszawa", "wloclawek", "wroclaw", "zabrze",
                "zielona-gora",
            ),
            Cities.sortedIn("pl").map { it.slug },
        )
    }

    @Test
    fun polishSortedCollatesLAfterLNotAtTheEnd() {
        // Polish-collation discriminator: a naive code-point sort puts "Łódź"
        // (Ł = U+0141) after every ASCII-initial name, i.e. near the very end.
        val slugs = Cities.sortedIn("pl").map { it.slug }
        assertEquals(slugs.indexOf("lublin") + 1, slugs.indexOf("lodz"))
        assertTrue(slugs.indexOf("lodz") < slugs.indexOf("zabrze"))
    }

    @Test
    fun returnsNullWhenFartherThan100km() {
        // Open Baltic, ~150 km north of Trójmiasto (its nearest served city) —
        // out of range of every supported city.
        assertNull(Cities.nearestWithin100km(55.5, 17.0, "pl"))
    }

    @Test
    fun suggestsTheNearerCityWhenChosenIsElsewhere() {
        // Chosen Poznań, but standing in Wrocław → offer the switch.
        val suggestion = Cities.switchSuggestion("poznan", 51.1079, 17.0385, lastPromptKey = null, countryCode = "pl")
        assertEquals("wroclaw", suggestion?.target?.slug)
        assertEquals("poznan→wroclaw", suggestion?.key)
    }

    @Test
    fun doesNotRepeatTheSamePair() {
        // Already asked poznan→wroclaw — don't ask again for the same pair.
        val suggestion = Cities.switchSuggestion(
            "poznan", 51.1079, 17.0385, lastPromptKey = "poznan→wroclaw", countryCode = "pl",
        )
        assertNull(suggestion)
    }

    @Test
    fun noSuggestionWhenAlreadyInTheNearestCity() {
        // Chosen Wrocław, standing in Wrocław → nothing to switch to.
        assertNull(Cities.switchSuggestion("wroclaw", 51.1079, 17.0385, lastPromptKey = null, countryCode = "pl"))
    }

    @Test
    fun noSuggestionWhenOutOfRangeOfEveryCity() {
        // Open Baltic — out of range of every supported city → no offer.
        assertNull(Cities.switchSuggestion("poznan", 55.5, 17.0, lastPromptKey = null, countryCode = "pl"))
    }

    @Test
    fun switchSuggestionIsCountryScoped() {
        // Chosen London, device in Manchester — GB suggests the switch; a Polish
        // scope sees no nearby city.
        val gb = Cities.switchSuggestion("london", 53.4808, -2.2426, lastPromptKey = null, countryCode = "uk")
        assertEquals("manchester", gb?.target?.slug)
        assertNull(Cities.switchSuggestion("london", 53.4808, -2.2426, lastPromptKey = null, countryCode = "pl"))
    }

    @Test
    fun initialChoiceSuppressKeyMatchesTheSwitchKeyForADifferentCity() {
        // Picking Warszawa at the gate while location placed the user near
        // Poznań must seed exactly the pair switchSuggestion would produce, so
        // the immediate "you're nearer Poznań" prompt is suppressed.
        val key = Cities.initialChoiceSuppressKey("warszawa", "poznan")
        assertEquals("warszawa→poznan", key)

        // End-to-end: feeding that key back suppresses the offer the gate would
        // otherwise raise from Poznań coordinates.
        assertNull(Cities.switchSuggestion("warszawa", 52.4064, 16.9252, lastPromptKey = key, countryCode = "pl"))
    }

    @Test
    fun initialChoiceSuppressKeyIsNullWhenChosenCityIsTheNearest() {
        // Confirming the detected city — nothing to suppress.
        assertNull(Cities.initialChoiceSuppressKey("poznan", "poznan"))
    }

    @Test
    fun initialChoiceSuppressKeyIsNullWithoutALocationFix() {
        // Location unavailable at the gate — a later legitimate prompt stays armed.
        assertNull(Cities.initialChoiceSuppressKey("warszawa", null))
    }

    // ── matching (city-picker search, per country) ────────────────

    @Test
    fun blankQueryMatchesEveryCity() {
        assertEquals(Cities.sortedIn("pl"), Cities.matching("", "pl"))
        assertEquals(Cities.sortedIn("pl"), Cities.matching("   ", "pl"))
    }

    @Test
    fun narrowsToAMatchingName() {
        assertEquals(listOf("wroclaw"), Cities.matching("wroc", "pl").map { it.slug })
    }

    @Test
    fun matchIsCaseInsensitive() {
        assertEquals(listOf("krakow"), Cities.matching("KRAKÓW", "pl").map { it.slug })
    }

    @Test
    fun matchIsDiacriticInsensitiveTypedWithoutPolishLetters() {
        // The whole point: a plain ASCII keyboard finds the diacritic'd city.
        assertEquals(listOf("lodz"), Cities.matching("lodz", "pl").map { it.slug })
        assertEquals(listOf("krakow"), Cities.matching("krakow", "pl").map { it.slug })
        // "Gdańsk" isn't a city name (the Tri-City scope is "Trójmiasto") → no match.
        assertEquals(emptyList<String>(), Cities.matching("gdansk", "pl").map { it.slug })
        assertTrue(Cities.matching("zielona gora", "pl").map { it.slug }.contains("zielona-gora"))
    }

    @Test
    fun matchIsASubstringNotJustAPrefix() {
        // "gora" appears mid-name in "Zielona Góra" and "Jelenia Góra".
        val slugs = Cities.matching("gora", "pl").map { it.slug }
        assertTrue(slugs.contains("zielona-gora"))
        assertTrue(slugs.contains("jelenia-gora"))
    }

    @Test
    fun noMatchReturnsEmpty() {
        assertEquals(emptyList<City>(), Cities.matching("zzzzz", "pl"))
    }

    @Test
    fun matchingKeepsPolishAlphabeticalOrder() {
        // Filtered results stay in sorted order: Gliwice (G) before Łódź (Ł)
        // before Opole (O), not reordered by match.
        val slugs = Cities.matching("l", "pl").map { it.slug }
        assertTrue(slugs.indexOf("gliwice") < slugs.indexOf("lodz"))
        assertTrue(slugs.indexOf("lodz") < slugs.indexOf("opole"))
    }
}
