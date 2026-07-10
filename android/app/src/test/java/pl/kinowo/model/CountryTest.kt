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
        assertEquals("PL", pl.code)
        assertEquals("https://kinowo.fly.dev", pl.baseUrl)
        assertEquals("pl", pl.languageTag)
    }

    @Test
    fun ukEntryForcesEnglishOnItsOwnDeployment() {
        val gb = Country.byCode("GB")
        assertEquals("United Kingdom", gb.displayName)
        // TODO(§6): placeholder until the real UK deployment URL is set.
        assertEquals("https://kinowo.co.uk", gb.baseUrl)
        assertEquals("en", gb.languageTag)
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
