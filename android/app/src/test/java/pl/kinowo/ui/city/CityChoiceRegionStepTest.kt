package pl.kinowo.ui.city

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTextInput
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Catalog
import pl.kinowo.model.City
import pl.kinowo.model.Country

/**
 * The US is picked in two steps — state, then city — because 457 metros in one
 * A-to-Z is not a list anybody reads. Every other country keeps the single flat
 * list, and that difference is driven purely by whether the catalog's cities
 * carry a region.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "en")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityChoiceRegionStepTest {

    @get:Rule
    val compose = createComposeRule()

    private val losAngeles = City("los-angeles", "Los Angeles", 34.05, -118.24, "us", "California")
    private val sanDiego = City("san-diego", "San Diego", 32.72, -117.16, "us", "California")
    private val austin = City("austin", "Austin", 30.27, -97.74, "us", "Texas")
    private val poznan = City("poznan", "Poznań", 52.41, 16.93, "pl")

    private val catalog = Catalog(
        countries = Country.all,
        cities = listOf(losAngeles, sanDiego, austin, poznan),
    )

    private fun showUs(onPick: (City) -> Unit = {}) =
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = onPick, selectedCountryCode = "us")
        }

    @Test
    fun theUsOpensOnStatesRatherThanOnFourHundredCities() {
        showUs()

        compose.onNodeWithText("Choose a state").assertIsDisplayed()
        compose.onNodeWithText("California").assertIsDisplayed()
        compose.onNodeWithText("Texas").assertIsDisplayed()
        // No city is offered until a state is: that is the whole point.
        compose.onNodeWithText("Los Angeles").assertDoesNotExist()
        compose.onNodeWithText("Austin").assertDoesNotExist()
    }

    @Test
    fun pickingAStateShowsOnlyThatStatesCities() {
        showUs()
        compose.onNodeWithText("California").performClick()

        compose.onNodeWithText("Los Angeles").assertIsDisplayed()
        compose.onNodeWithText("San Diego").assertIsDisplayed()
        compose.onNodeWithText("Austin").assertDoesNotExist()
        // The state names its own screen — nothing else on it does.
        compose.onNodeWithText("California").assertIsDisplayed()
    }

    @Test
    fun aCityIsReportedFromTheSecondStep() {
        var picked: City? = null
        showUs { picked = it }

        compose.onNodeWithText("Texas").performClick()
        compose.onNodeWithText("Austin").performClick()
        assertEquals(austin, picked)
    }

    @Test
    fun backReturnsToTheStateList() {
        showUs()
        compose.onNodeWithText("California").performClick()
        compose.onNodeWithText("Los Angeles").assertIsDisplayed()

        compose.onNodeWithText("All states").performClick()

        compose.onNodeWithText("Choose a state").assertIsDisplayed()
        compose.onNodeWithText("Texas").assertIsDisplayed()
        compose.onNodeWithText("Los Angeles").assertDoesNotExist()
    }

    @Test
    fun theStateStepIsSearchable() {
        showUs()
        compose.onNode(hasSetTextAction()).performTextInput("tex")

        compose.onNodeWithText("Texas").assertIsDisplayed()
        compose.onNodeWithText("California").assertDoesNotExist()
    }

    /** The search carries no meaning across the step boundary. */
    @Test
    fun theSearchResetsWhenAStateIsEntered() {
        showUs()
        compose.onNode(hasSetTextAction()).performTextInput("cal")
        compose.onNodeWithText("California").performClick()

        // Both California cities are here — "cal" did not follow us in and
        // narrow them to nothing.
        compose.onNodeWithText("Los Angeles").assertIsDisplayed()
        compose.onNodeWithText("San Diego").assertIsDisplayed()
    }

    /** A country whose cities carry no region keeps the one flat list. */
    @Test
    fun anUngroupedCountryStillPicksInOneStep() {
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = {}, selectedCountryCode = "pl")
        }

        compose.onNodeWithText("Choose a city").assertIsDisplayed()
        compose.onNodeWithText("Poznań").assertIsDisplayed()
        compose.onNodeWithText("All states").assertDoesNotExist()
    }
}
