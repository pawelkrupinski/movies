package pl.kinowo.data

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.launch
import kotlinx.coroutines.flow.combine
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.yield
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * [UserPreferences.setCityInCountry] backs a located city landing in a
 * different country than the one open in the picker (see
 * [pl.kinowo.ui.KinowoViewModel.adoptDetectedCity]). It must write the country
 * and the city as ONE DataStore transaction: MainActivity recreates the
 * activity (cancelling the ViewModel's coroutine) the instant it observes the
 * country pref change, so a two-step write lets that recreate race ahead of
 * the city write and lose it — the "you're near X" dialog vanishes and the
 * app lands on the new country's tab with no city adopted.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesLocatedCityAcrossCountriesTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    @Test
    fun setsBothCountryAndCityAndClearsExplicitPick() = runBlocking {
        prefs.setCountryCode("uk")
        prefs.awaitExplicitCityPick()

        prefs.setCityInCountry("warszawa", "pl")

        assertEquals("pl", prefs.selectedCountryCode.first())
        assertEquals("warszawa", prefs.selectedCity.first())
        assertFalse("adopting the city satisfies the gate", prefs.awaitingExplicitCityPick.first())
    }

    @Test
    fun neverExposesTheCountrySwitchedWithTheOldCityStillSelected() = runBlocking {
        prefs.setCountryCode("uk")
        prefs.setCity("london")

        val states = mutableListOf<Pair<String?, String?>>()
        val collector = launch {
            combine(prefs.selectedCountryCode, prefs.selectedCity) { country, city -> country to city }
                .toList(states)
        }
        // Let the initial (uk, london) snapshot land before writing.
        while (states.isEmpty()) yield()

        prefs.setCityInCountry("warszawa", "pl")

        // Wait for the final state, then stop collecting.
        while (states.last() != ("pl" to "warszawa")) yield()
        collector.cancel()

        // A two-step write (setCountryCode() then setCity()) would expose this
        // exact intermediate state — country already switched, city still the
        // old country's — which is what let MainActivity's recreate-on-country-
        // change watcher tear the ViewModel down before the city write landed.
        assertTrue(
            "must never observe the new country paired with the stale city",
            states.none { it == ("pl" to "london") },
        )
    }
}
