package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test
import java.time.ZoneId

/**
 * Pins the static country registry: it holds only countries a deployment
 * actually serves — Poland alone since the UK and German deployments were
 * stopped on 2026-08-02. Mirrors the iOS `CountryTests` so the two apps can't
 * drift.
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
    fun registryHoldsOnlyTheDeployedCountry() {
        assertEquals(listOf("pl"), Country.all.map { it.code })
        assertEquals(emptyList<Country>(), Country.all.filter { it.baseUrl.contains("showtimes-") })
    }

    /** The upgrade path for a user who had UK or Germany selected: `byCode`
     *  resolves the PERSISTED code through this registry and `MainActivity`
     *  builds `KinowoApi` on the result, so a code whose deployment is gone must
     *  land on the live one rather than pin the app to a stopped host. */
    @Test
    fun persistedStoppedCountryFallsBackToTheLiveDeployment() {
        for (stopped in listOf("uk", "de")) {
            assertEquals(Country.default, Country.byCode(stopped))
            assertEquals("https://kinowo.fly.dev", Country.byCode(stopped).baseUrl)
        }
    }

    @Test
    fun legacyIsoCodesNormalizeToServerCodes() {
        // Earlier builds persisted ISO codes (PL/GB); the catalog keys on pl/uk.
        // The code space is unchanged — it's the registry lookup that then falls
        // back, so this mapping stays right if the UK deployment ever returns.
        assertEquals("pl", Country.normalizeCode("PL"))
        assertEquals("uk", Country.normalizeCode("GB"))
        assertEquals("uk", Country.normalizeCode("uk"))
        assertEquals("pl", Country.byCode("PL").code)
        assertEquals(Country.default, Country.byCode("GB"))   // uk is undeployed → Poland
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
        // Drives timezone-correct pruning. The registry now carries one country,
        // so the per-country zone is exercised through the CATALOG shape instead
        // (`countryDtoDecodesTimezoneWithWarsawFallback`) — that is where a
        // second country's zone would come from if one were deployed again.
        assertEquals(ZoneId.of("Europe/Warsaw"), Country.byCode("pl").zoneId)
    }

    /** The visibility rule both country controls gate on — the first-launch
     *  gate's [pl.kinowo.ui.CountryPicker] and the Filtry sheet's country
     *  section. One deployed country means nothing to switch to. */
    @Test
    fun aSingleDeployedCountryIsNotSwitchable() {
        assertEquals(1, Country.all.size)
        assertEquals(false, Country.all.isSwitchable)
    }

    /** …and the rule is about the LIST, not a hardcoded count: a catalog that
     *  carries two countries again turns both controls back on. */
    @Test
    fun twoCountriesInTheCatalogAreSwitchableAgain() {
        val second = Country("uk", "United Kingdom", "https://example.test", "en")
        assertEquals(true, (Country.all + second).isSwitchable)
    }

    @Test
    fun countryDtoDecodesTimezoneWithWarsawFallback() {
        assertEquals(
            ZoneId.of("Europe/London"),
            CountryDto("uk", "United Kingdom", "https://showtimes-uk.fly.dev", "en", "Europe/London").toCountry().zoneId,
        )
        // An older seed / a server predating the field: no timezone → Warsaw.
        assertEquals(
            ZoneId.of("Europe/Warsaw"),
            CountryDto("pl", "Polska", "https://kinowo.fly.dev", "pl", null).toCountry().zoneId,
        )
    }
}
