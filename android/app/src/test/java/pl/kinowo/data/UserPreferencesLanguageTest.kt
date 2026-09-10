package pl.kinowo.data

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * Round-trips the selected-language preference through a real Preferences
 * DataStore (off-device via Robolectric): null until written, the persisted
 * tag afterwards, readable both via the flow and the blocking accessor
 * MainActivity uses at attach time. Also pins the decoupling contract this
 * preference exists for: switching the COUNTRY must never touch the language,
 * and vice versa — see [pl.kinowo.MainActivity]'s two independent watchers.
 *
 * One test method, not several: the DataStore file is shared across this
 * class (see [UserPreferencesCountryTest]), so a second writer in its own
 * method would break the "null until set" assertion below.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesLanguageTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    @Test
    fun languageRoundTripsAndStaysDecoupledFromCountry() = runBlocking {
        assertNull("no language before the user picks one", prefs.selectedLanguageTag.first())
        assertNull(prefs.blockingLanguageTag())

        prefs.setLanguageTag("de")
        assertEquals("de", prefs.selectedLanguageTag.first())
        assertEquals("de", prefs.blockingLanguageTag())

        // A second pick round-trips the same way — same store, same accessor.
        prefs.setLanguageTag("es")
        assertEquals("es", prefs.blockingLanguageTag())

        // A country switch touches only the country key — nothing in
        // UserPreferences.setCountryCode reads or writes the language key.
        prefs.setCountryCode("uk")
        assertEquals(
            "the language pick must survive a country switch",
            "es",
            prefs.blockingLanguageTag(),
        )
        prefs.setCountryCode("us")
        assertEquals(
            "the language pick must survive ANY number of country switches",
            "es",
            prefs.blockingLanguageTag(),
        )

        // And the reverse: picking a language must not move the selected country.
        prefs.setLanguageTag("pl")
        assertEquals(
            "picking a language must not move the selected country",
            "us",
            prefs.blockingCountryCode(),
        )
    }
}
