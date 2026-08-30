package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test
import java.time.ZoneId

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
        assertEquals("https://kinowo.net", pl.baseUrl)
        assertEquals("pl", pl.languageTag)
    }

    @Test
    fun ukEntryForcesEnglishOnItsOwnDeployment() {
        val uk = Country.byCode("uk")
        assertEquals("uk", uk.code)
        assertEquals("United Kingdom", uk.displayName)
        assertEquals("https://uk.showtimes.cc", uk.baseUrl)
        assertEquals("en", uk.languageTag)
    }

    @Test
    fun usEntryForcesEnglishOnItsOwnDeployment() {
        val us = Country.byCode("us")
        assertEquals("us", us.code)
        assertEquals("United States", us.displayName)
        assertEquals("https://us.showtimes.cc", us.baseUrl)
        // The US ships no bundle of its own — it reuses the English one.
        assertEquals("en", us.languageTag)
    }

    @Test
    fun legacyIsoCodesNormalizeToServerCodes() {
        // Earlier builds persisted ISO codes (PL/GB/US); the catalog keys on
        // pl/uk/us.
        assertEquals("pl", Country.byCode("PL").code)
        assertEquals("uk", Country.byCode("GB").code)
        assertEquals("us", Country.byCode("US").code)
        assertEquals("pl", Country.normalizeCode("PL"))
        assertEquals("uk", Country.normalizeCode("GB"))
        assertEquals("us", Country.normalizeCode("US"))
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

    @Test
    fun eachCountryCarriesItsLocalZone() {
        // Drives timezone-correct pruning — a London show disappears on London
        // time, a Berlin one on Berlin time, not a hardcoded Warsaw.
        assertEquals(ZoneId.of("Europe/Warsaw"), Country.byCode("pl").zoneId)
        assertEquals(ZoneId.of("Europe/London"), Country.byCode("uk").zoneId)
        assertEquals(ZoneId.of("Europe/Berlin"), Country.byCode("de").zoneId)
        // The US spans six zones; the fallback carries a nominal Eastern one
        // that the catalog's per-country value replaces as soon as it loads.
        assertEquals(ZoneId.of("America/New_York"), Country.byCode("us").zoneId)
    }

    @Test
    fun countryDtoDecodesTimezoneWithWarsawFallback() {
        assertEquals(
            ZoneId.of("Europe/London"),
            CountryDto("uk", "United Kingdom", "https://uk.showtimes.cc", "en", "Europe/London").toCountry().zoneId,
        )
        // An older seed / a server predating the field: no timezone → Warsaw.
        assertEquals(
            ZoneId.of("Europe/Warsaw"),
            CountryDto("pl", "Polska", "https://kinowo.net", "pl", null).toCountry().zoneId,
        )
        // The catalog's US zone (its first region's) wins over the registry's.
        assertEquals(
            ZoneId.of("America/Chicago"),
            CountryDto("us", "United States", "https://us.showtimes.cc", "en", "America/Chicago")
                .toCountry().zoneId,
        )
    }
}
