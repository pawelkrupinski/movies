package pl.kinowo.ui

import android.content.Context
import androidx.test.core.app.ApplicationProvider
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.R
import pl.kinowo.model.Country

/**
 * Proves the country → forced-language → resource pipeline end to end: wrapping
 * a context in a language tag (as MainActivity.attachBaseContext does with the
 * selected `Country.languageTag`) makes `getString` resolve `values-en` /
 * `values-de`, and the Polish default otherwise — regardless of the device
 * locale. Fails before the localized `values-*` strings existed (they'd fall
 * back to the Polish default).
 *
 * The English and German cases pass their tag DIRECTLY rather than through
 * `Country.byCode("GB"/"de")`. Those codes resolved to real registry entries
 * until 2026-08-02, when the UK and German deployments were stopped and dropped
 * from the registry — every code now resolves to Poland. The `values-en` /
 * `values-de` resources still ship and are still what a restored country would
 * render, so the wrapper keeps its coverage; only the lookup that picks the tag
 * moved out of the assertion.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class LocaleStringsTest {

    private val base: Context = ApplicationProvider.getApplicationContext()

    @Test
    fun englishTagResolvesEnglishStrings() {
        val en = LocaleWrapper.wrap(base, "en")
        assertEquals("Loading showtimes…", en.getString(R.string.loading_repertoire))
        assertEquals("Country", en.getString(R.string.country_label))
        assertEquals("Try again", en.getString(R.string.retry))
        // Outside Poland the app is branded "Showtimes", not "Kinowo".
        assertEquals("Showtimes", en.getString(R.string.app_name))
    }

    @Test
    fun germanTagResolvesGermanStrings() {
        val de = LocaleWrapper.wrap(base, "de")
        assertEquals("Spielzeiten werden geladen…", de.getString(R.string.loading_repertoire))
        // Strings externalised for the UK translation must also resolve in German
        // (they fall back to the Polish default until values-de carries them).
        assertEquals("Anmelden", de.getString(R.string.sign_in))
        assertEquals("Regie", de.getString(R.string.meta_director))
        assertEquals("Sortieren", de.getString(R.string.sort))
        // Germany is branded "Showtimes", like the UK.
        assertEquals("Showtimes", de.getString(R.string.app_name))
    }

    @Test
    fun polandDefaultResolvesPolishStrings() {
        val pl = LocaleWrapper.wrap(base, Country.default.languageTag)
        assertEquals("Ładowanie repertuaru…", pl.getString(R.string.loading_repertoire))
        assertEquals("Kraj", pl.getString(R.string.country_label))
        // At home the brand keeps its Polish name.
        assertEquals("Kinowo", pl.getString(R.string.app_name))
    }
}
