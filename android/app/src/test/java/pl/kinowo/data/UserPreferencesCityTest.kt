package pl.kinowo.data

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner

/**
 * Round-trips the selected-city preference through a real Preferences DataStore
 * (off-device via Robolectric): null until written, the persisted slug after.
 */
@RunWith(RobolectricTestRunner::class)
class UserPreferencesCityTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    @Test
    fun selectedCityIsNullUntilSetThenReadsBack() = runBlocking {
        assertNull("no city before the gate resolves one", prefs.selectedCity.first())
        prefs.setCity("poznan")
        assertEquals("poznan", prefs.selectedCity.first())
    }

    @Test
    fun citySwitchPromptKeyIsNullUntilSetThenReadsBack() = runBlocking {
        assertNull("no prompt key before any switch is offered", prefs.citySwitchPromptKey.first())
        prefs.setCitySwitchPromptKey("poznan→wroclaw")
        assertEquals("poznan→wroclaw", prefs.citySwitchPromptKey.first())
    }
}
