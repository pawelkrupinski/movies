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
 * does) makes `getString` resolve `values-en` for the UK country and the Polish
 * default otherwise — regardless of the device locale. Fails before `values-en`
 * existed (English would fall back to the Polish default).
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
    }

    @Test
    fun polandDefaultResolvesPolishStrings() {
        val pl = LocaleWrapper.wrap(base, Country.default.languageTag)
        assertEquals("Ładowanie repertuaru…", pl.getString(R.string.loading_repertoire))
        assertEquals("Kraj", pl.getString(R.string.country_label))
    }
}
