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
 * a context in the country's language tag (as MainActivity.attachBaseContext
 * does) makes `getString` resolve `values-en`/`values-de` for the UK/German
 * countries and the Polish default otherwise — regardless of the device locale.
 * Fails before the localized `values-*` strings existed (they'd fall back to the
 * Polish default).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class LocaleStringsTest {

    private val base: Context = ApplicationProvider.getApplicationContext()

    @Test
    fun ukCountryResolvesEnglishStrings() {
        val en = LocaleWrapper.wrap(base, Country.byCode("GB").languageTag)
        assertEquals("Loading showtimes…", en.getString(R.string.loading_repertoire))
        assertEquals("Country", en.getString(R.string.country_label))
        assertEquals("Try again", en.getString(R.string.retry))
        // Outside Poland the app is branded "Showtimes", not "Kinowo".
        assertEquals("Showtimes", en.getString(R.string.app_name))
    }

    @Test
    fun germanCountryResolvesGermanStrings() {
        val de = LocaleWrapper.wrap(base, Country.byCode("de").languageTag)
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
