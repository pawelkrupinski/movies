package pl.kinowo.ui

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.cancel
import kotlinx.coroutines.flow.launchIn
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.runBlocking
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.data.UserPreferences
import java.util.Collections

/**
 * A deep link into another country's city switches the country AND the city.
 * MainActivity recreates (and clears the ViewModel) the instant the country
 * pref changes, so writing the two as separate edits exposes a torn state —
 * the new country with the old country's city, requested against the new
 * deployment — and can lose the city write to that teardown. Same atomic
 * write [KinowoViewModel.adoptDetectedCity] already uses.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class CrossCountryDeepLinkTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    private val prefs get() = UserPreferences(harness.context)

    // Other classes in the fork assert "null until set" on country and city.
    @After
    fun wipe() {
        runBlocking { prefs.clearAllForTest() }
    }

    @Test
    fun aCrossCountryLinkSwitchesCountryAndCityInOneWrite() {
        runBlocking {
            prefs.clearAllForTest()
            prefs.setCityInCountry("warszawa", "pl")
        }
        val seen = Collections.synchronizedList(mutableListOf<Pair<String?, String?>>())
        val watcher = CoroutineScope(Dispatchers.Unconfined)
        prefs.countryAndCity.onEach { seen += it }.launchIn(watcher)
        val vm = harness.viewModel()
        harness.pumpUntil("the view model to see the stored country") { vm.selectedCountryCode.value == "pl" }

        vm.handleDeepLink("https://showtimes.cc/uk/london/")
        harness.pumpUntil("the link's city to be stored") { seen.lastOrNull() == ("uk" to "london") }
        watcher.cancel()

        assertEquals(
            "no intermediate country/city pairing may ever be persisted",
            listOf("pl" to "warszawa", "uk" to "london"),
            seen.distinct(),
        )
    }
}
