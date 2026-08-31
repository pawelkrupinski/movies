package pl.kinowo.data

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * The flag that tells the city gate to present a list rather than offer a
 * located city. It has to survive the activity recreate a country switch
 * triggers, which is why it lives in the store rather than in memory — and it
 * has to stop applying the moment a city is actually chosen.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesExplicitPickTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    @Test
    fun armedUntilACityIsChosen() = runBlocking {
        assertFalse("nothing to honour before a country is picked", prefs.awaitingExplicitCityPick.first())

        prefs.awaitExplicitCityPick()
        assertTrue(prefs.awaitingExplicitCityPick.first())

        // Re-reading must not consume it: the activity recreates between the
        // country switch and the gate reading this, and every read in between
        // has to still see it armed.
        assertTrue(prefs.awaitingExplicitCityPick.first())

        prefs.setCity("berlin")
        assertFalse("choosing a city satisfies the gate", prefs.awaitingExplicitCityPick.first())
    }
}
