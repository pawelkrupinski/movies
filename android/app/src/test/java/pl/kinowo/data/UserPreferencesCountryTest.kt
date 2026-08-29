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
import pl.kinowo.model.Country

/**
 * Round-trips the selected-country preference through a real Preferences
 * DataStore (off-device via Robolectric): null (→ default Poland) until written,
 * the persisted code afterwards, readable both via the flow and the blocking
 * accessor MainActivity uses at wiring time.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesCountryTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    @Test
    fun countryIsNullUntilSetThenReadsBack() = runBlocking {
        assertNull("no country before the user picks one", prefs.selectedCountryCode.first())
        assertNull(prefs.blockingCountryCode())
        // A null code resolves to the default (Poland) at the registry.
        assertEquals(Country.default, Country.byCode(prefs.blockingCountryCode()))

        prefs.setCountryCode("GB")
        assertEquals("GB", prefs.selectedCountryCode.first())
        assertEquals("GB", prefs.blockingCountryCode())
        assertEquals("en", Country.byCode(prefs.blockingCountryCode()).languageTag)
    }
}
