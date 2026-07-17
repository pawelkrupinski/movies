package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pins the static country registry: Poland is the default (current prod base
 * URL, Polish UI) and the UK entry carries the English deployment + language.
 * Mirrors the iOS `CountryTests` so the two apps can't drift.
 */
class CountryTest {

    @Test
    fun defaultIsPolandOnTheProdDeployment() {
        val pl = Country.default
        assertEquals("pl", pl.code)
        assertEquals("https://kinowo.fly.dev", pl.baseUrl)
        assertEquals("pl", pl.languageTag)
    }

    @Test
    fun ukEntryForcesEnglishOnItsOwnDeployment() {
        val uk = Country.byCode("uk")
        assertEquals("uk", uk.code)
        assertEquals("United Kingdom", uk.displayName)
        assertEquals("https://showtimes-uk.fly.dev", uk.baseUrl)
        assertEquals("en", uk.languageTag)
    }

    @Test
    fun legacyIsoCodesNormalizeToServerCodes() {
        // Earlier builds persisted ISO codes (PL/GB); the catalog keys on pl/uk.
        assertEquals("pl", Country.byCode("PL").code)
        assertEquals("uk", Country.byCode("GB").code)
        assertEquals("pl", Country.normalizeCode("PL"))
        assertEquals("uk", Country.normalizeCode("GB"))
        assertEquals("uk", Country.normalizeCode("uk"))
    }

    @Test
    fun unknownOrNullCodeFallsBackToDefault() {
        assertEquals(Country.default, Country.byCode(null))
        assertEquals(Country.default, Country.byCode("ZZ"))
    }

    @Test
    fun everyCountryHasADistinctCodeAndBaseUrl() {
        assertEquals(Country.all.size, Country.all.map { it.code }.toSet().size)
        assertEquals(Country.all.size, Country.all.map { it.baseUrl }.toSet().size)
    }
}
