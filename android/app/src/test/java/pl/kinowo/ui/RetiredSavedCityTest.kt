package pl.kinowo.ui

import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.data.UserPreferences

/**
 * A city saved before its page was retired — `miedzyrzec-podlaski`, now part of
 * `biala-podlaska` — is adopted under the slug it answers at. Its listing kept
 * arriving through the server's 301, but the Filtry sheet found no city by the
 * old slug and labelled the list with the country's default city instead.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class RetiredSavedCityTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    private val prefs get() = UserPreferences(harness.context)

    private val seed =
        """{"etag":"\"t\"","catalog":{"countries":[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.net","language":"pl","brand":"Kinowo"}],""" +
            """"cities":[{"slug":"biala-podlaska","name":"Biała Podlaska i okolice","lat":52.0324,"lon":23.1165,"country":"pl","formerSlugs":["miedzyrzec-podlaski"]},""" +
            """{"slug":"poznan","name":"Poznań","lat":52.4064,"lon":16.9252,"country":"pl"}]}}"""

    @Test
    fun aSavedRetiredCityIsAdoptedUnderTheSlugItAnswersAtNow() {
        runBlocking { prefs.setCityInCountry("miedzyrzec-podlaski", "pl") }
        val vm = harness.viewModel(catalogSeed = seed)
        vm.start()
        harness.pumpUntil("the saved city to move onto its successor") { vm.selectedCity.value == "biala-podlaska" }
        assertEquals("biala-podlaska", runBlocking { prefs.selectedCity.first() })
    }

    @Test
    fun aCurrentSavedCityIsLeftAlone() {
        runBlocking { prefs.setCityInCountry("poznan", "pl") }
        val vm = harness.viewModel(catalogSeed = seed)
        vm.start()
        harness.pumpFor(300)
        assertEquals("poznan", vm.selectedCity.value)
    }
}
