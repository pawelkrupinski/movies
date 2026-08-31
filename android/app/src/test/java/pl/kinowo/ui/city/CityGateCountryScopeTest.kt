package pl.kinowo.ui.city

import android.Manifest
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import kotlinx.coroutines.flow.MutableStateFlow
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Catalog
import pl.kinowo.model.City
import pl.kinowo.model.Country
import pl.kinowo.model.nearestWithin100km
import pl.kinowo.ui.CityGate

/**
 * The first-launch gate is scoped to the SELECTED country, and the selected
 * country arrives asynchronously (DataStore). Both halves matter: a device
 * sitting in Poznań that has just switched to Germany must be offered the
 * German city list, never a "You're near Poznań" confirmation — Poznań is not
 * a city of the country the user asked for.
 *
 * The gate is driven here with a real Poznań fix and the real country-scoped
 * [nearestWithin100km], so what is under test is which country the gate hands
 * it and when.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityGateCountryScopeTest {

    @get:Rule
    val compose = createComposeRule()

    private val poznan = City("poznan", "Poznań", 52.4064, 16.9252, "pl")
    private val berlin = City("berlin", "Berlin", 52.5200, 13.4050, "de")
    private val muenchen = City("muenchen", "München", 48.1351, 11.5820, "de")

    private val catalog = Catalog(
        countries = Country.all,
        cities = listOf(poznan, berlin, muenchen),
    )

    /** A gate opened the first-launch way: free to offer a located city. */
    private fun locating(code: String) = CityGateStart(countryCode = code, locate = true)

    /** How many times the gate reached for a location fix — 0 proves it never
     *  tried, and so never raised the permission dialog. */
    private var locationAttempts = 0

    /** The device is in Poznań; the resolver is the real country-scoped one. */
    private val fixInPoznan: suspend (String, List<City>) -> City? = { country, cities ->
        locationAttempts++
        cities.nearestWithin100km(poznan.lat, poznan.lon, country)
    }

    @Before
    fun grantLocation() {
        Shadows.shadowOf(org.robolectric.RuntimeEnvironment.getApplication())
            .grantPermissions(Manifest.permission.ACCESS_COARSE_LOCATION)
    }

    @Composable
    private fun gate(
        start: MutableStateFlow<CityGateStart?>,
        onConfirm: (City) -> Unit = {},
    ) = CityGate(
        start = start.collectAsState().value,
        catalog = catalog,
        onPick = { _, _ -> },
        onConfirm = onConfirm,
        onCountry = {},
        resolveNearest = fixInPoznan,
    )

    /**
     * THE BUG: the stored country lands only after the first composition. The
     * gate must not resolve against the default (Poland) in the meantime — if
     * it does, a user who picked Germany gets Poznań confirmed at them.
     */
    @Test
    fun waitsForTheStoredCountryInsteadOfResolvingAsTheDefault() {
        val country = MutableStateFlow<CityGateStart?>(null)
        var confirmed: City? = null
        compose.setContent { gate(country, onConfirm = { confirmed = it }) }

        // Country not read yet: the gate shows nothing and resolves nothing.
        compose.waitForIdle()
        compose.onNodeWithText("Poznań").assertDoesNotExist()
        assertNull("must not resolve a city before the stored country lands", confirmed)

        // Germany arrives. Poznań is >100 km from every German city, so the gate
        // falls through to the German list rather than confirming anything.
        country.value = locating("de")
        compose.waitForIdle()
        compose.onNodeWithText("Berlin").assertIsDisplayed()
        compose.onNodeWithText("Poznań").assertDoesNotExist()
    }

    /** The chooser it falls through to lists the chosen country's cities only. */
    @Test
    fun theFallbackListIsScopedToTheChosenCountry() {
        val country = MutableStateFlow<CityGateStart?>(locating("de"))
        compose.setContent { gate(country) }
        compose.waitForIdle()

        compose.onNodeWithText("Berlin").assertIsDisplayed()
        compose.onNodeWithText("München").assertIsDisplayed()
        compose.onNodeWithText("Poznań").assertDoesNotExist()
    }

    /** The near-you screen still appears when the fix IS in the chosen country. */
    @Test
    fun confirmsWhenTheFixIsInsideTheChosenCountry() {
        val country = MutableStateFlow<CityGateStart?>(locating("pl"))
        compose.setContent { gate(country) }
        compose.waitForIdle()

        compose.onNodeWithText("Poznań").assertIsDisplayed()
        compose.onNodeWithText("Pokaż repertuar", substring = true).assertIsDisplayed()
    }

    /**
     * Switching country re-resolves rather than leaving the outgoing country's
     * answer on screen: Poland confirms Poznań, Germany must drop it for the
     * German list.
     */
    @Test
    fun switchingCountryDropsThePreviousCountrysDetection() {
        val country = MutableStateFlow<CityGateStart?>(locating("pl"))
        compose.setContent { gate(country) }
        compose.waitForIdle()
        compose.onNodeWithText("Poznań").assertIsDisplayed()

        country.value = locating("de")
        compose.waitForIdle()
        compose.onNodeWithText("Poznań").assertDoesNotExist()
        compose.onNodeWithText("Berlin").assertIsDisplayed()
    }

    /**
     * Picking a country from the dropdown is itself a choice, and the gate owes
     * it that country's list. Even standing in Poznań with Poland selected —
     * the one case where a located city IS available — the gate must not offer
     * it, and must not go looking for one.
     */
    @Test
    fun aDeliberateCountryPickGoesStraightToThatCountrysList() {
        val start = MutableStateFlow<CityGateStart?>(CityGateStart("pl", locate = false))
        compose.setContent { gate(start) }
        compose.waitForIdle()

        compose.onNodeWithText("Pokaż repertuar", substring = true).assertDoesNotExist()
        compose.onNodeWithText("Poznań").assertIsDisplayed() // as a row of the list
        assertEquals("a deliberate country pick must not take a location fix", 0, locationAttempts)
    }

    /** The first launch is the other half: nothing has been chosen, so the
     *  nearest city is still the most useful thing to put in front of someone. */
    @Test
    fun theFirstLaunchStillOffersTheNearestCity() {
        val start = MutableStateFlow<CityGateStart?>(CityGateStart("pl", locate = true))
        compose.setContent { gate(start) }
        compose.waitForIdle()

        compose.onNodeWithText("Pokaż repertuar", substring = true).assertIsDisplayed()
        assertEquals(1, locationAttempts)
    }

    /** Sanity: the scoping helper the gate leans on really is country-scoped. */
    @Test
    fun theNearestHelperNeverCrossesTheBorder() {
        assertEquals(poznan, catalog.cities.nearestWithin100km(poznan.lat, poznan.lon, "pl"))
        assertNull(catalog.cities.nearestWithin100km(poznan.lat, poznan.lon, "de"))
    }
}
