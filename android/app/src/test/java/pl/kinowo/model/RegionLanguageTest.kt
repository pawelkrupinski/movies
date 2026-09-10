package pl.kinowo.model

import java.util.Locale
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pins the device-region → language table: the groupings [LanguageDefault]
 * falls back to when the device's own preferred language isn't one the app
 * localizes. Mirrors the iOS `StorefrontLanguageTests` so the two apps agree
 * on the same country-code groupings.
 */
class RegionLanguageTest {

    @Test
    fun germanSpeakingRegionsResolveToGerman() {
        assertEquals("de", RegionLanguage.forCountryCode("DE"))
        assertEquals("de", RegionLanguage.forCountryCode("AT"))
        assertEquals("de", RegionLanguage.forCountryCode("CH"))
        assertEquals("de", RegionLanguage.forCountryCode("LI"))
    }

    @Test
    fun spanishSpeakingRegionsResolveToSpanish() {
        assertEquals("es", RegionLanguage.forCountryCode("ES"))
        assertEquals("es", RegionLanguage.forCountryCode("MX"))
        assertEquals("es", RegionLanguage.forCountryCode("AR"))
        assertEquals("es", RegionLanguage.forCountryCode("CO"))
        assertEquals("es", RegionLanguage.forCountryCode("PE"))
        assertEquals("es", RegionLanguage.forCountryCode("CL"))
        assertEquals("es", RegionLanguage.forCountryCode("VE"))
        assertEquals("es", RegionLanguage.forCountryCode("EC"))
        assertEquals("es", RegionLanguage.forCountryCode("GT"))
        assertEquals("es", RegionLanguage.forCountryCode("CU"))
        assertEquals("es", RegionLanguage.forCountryCode("BO"))
        assertEquals("es", RegionLanguage.forCountryCode("DO"))
        assertEquals("es", RegionLanguage.forCountryCode("HN"))
        assertEquals("es", RegionLanguage.forCountryCode("PY"))
        assertEquals("es", RegionLanguage.forCountryCode("SV"))
        assertEquals("es", RegionLanguage.forCountryCode("NI"))
        assertEquals("es", RegionLanguage.forCountryCode("CR"))
        assertEquals("es", RegionLanguage.forCountryCode("PA"))
        assertEquals("es", RegionLanguage.forCountryCode("UY"))
        assertEquals("es", RegionLanguage.forCountryCode("PR"))
    }

    @Test
    fun polandResolvesToPolish() {
        assertEquals("pl", RegionLanguage.forCountryCode("PL"))
    }

    @Test
    fun unmappedRegionFallsBackToEnglish() {
        assertEquals("en", RegionLanguage.forCountryCode("JP"))
    }

    @Test
    fun nullRegionFallsBackToEnglish() {
        assertEquals("en", RegionLanguage.forCountryCode(null))
    }

    @Test
    fun languageDefaultPrefersTheDeviceLanguageWhenLocalized() {
        // German device locale, region irrelevant once the language itself matches.
        assertEquals("de", LanguageDefault.resolve(Locale.GERMANY))
        assertEquals("pl", LanguageDefault.resolve(Locale("pl", "PL")))
    }

    @Test
    fun languageDefaultFallsBackToRegionWhenTheLanguageIsntLocalized() {
        // French isn't one of pl/en/de/es, so a French-Canadian device falls back
        // through its REGION (CA — unmapped) to English, not to French.
        assertEquals("en", LanguageDefault.resolve(Locale("fr", "CA")))
        // A Mexican-Spanish device isn't "es" by language exactly... but Locale's
        // language subtag for Mexican Spanish IS "es", so this actually hits the
        // language branch. Use a genuinely unlocalized language with a Spanish
        // region instead: Basque (eu) spoken in Spain still falls back via region.
        assertEquals("es", LanguageDefault.resolve(Locale("eu", "ES")))
    }
}
