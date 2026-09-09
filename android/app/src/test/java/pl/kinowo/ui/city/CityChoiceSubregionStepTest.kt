package pl.kinowo.ui.city

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import org.junit.Assert.assertEquals
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
 * The UK's THIRD step: a county that actually holds more than one city
 * (West Midlands → Birmingham/Dudley/Sandwell) gets its own tap and its own
 * back button, distinct from the second step's "back to regions". Every
 * other English county stays a flat row on the second step, mirroring
 * [CityChoiceRegionStepTest] one level down.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "en")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityChoiceSubregionStepTest {

    @get:Rule
    val compose = createComposeRule()

    private val birmingham = City("birmingham", "Birmingham", 52.46, -1.9, "uk", "England", "West Midlands")
    private val dudley = City("dudley", "Dudley", 52.5, -2.09, "uk", "England", "West Midlands")
    private val sandwell = City("sandwell", "Sandwell", 52.52, -1.99, "uk", "England", "West Midlands")
    private val cheshire = City("cheshire", "Cheshire", 53.29, -2.5, "uk", "England")
    private val glasgow = City("glasgow", "Glasgow", 55.87, -4.23, "uk", "Scotland")

    private val catalog = Catalog(
        countries = Country.all,
        cities = listOf(birmingham, dudley, sandwell, cheshire, glasgow),
    )

    private fun showEngland(onPick: (City) -> Unit = {}) {
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = onPick, selectedCountryCode = "uk")
        }
        compose.onNodeWithText("England").performClick()
    }

    @Test
    fun englandMixesTheGroupRowAndDirectCities() {
        showEngland()

        // West Midlands is a group row — its members aren't listed individually.
        compose.onNodeWithText("West Midlands").assertIsDisplayed()
        compose.onNodeWithText("Birmingham").assertDoesNotExist()
        // A collapsed county (Cheshire) sits right there as a direct row.
        compose.onNodeWithText("Cheshire").assertIsDisplayed()
    }

    @Test
    fun openingTheCountyShowsOnlyItsOwnCities() {
        showEngland()
        compose.onNodeWithText("West Midlands").performClick()

        compose.onNodeWithText("Birmingham").assertIsDisplayed()
        compose.onNodeWithText("Dudley").assertIsDisplayed()
        compose.onNodeWithText("Sandwell").assertIsDisplayed()
        // Nothing from the rest of England leaks into the county's own list.
        compose.onNodeWithText("Cheshire").assertDoesNotExist()
    }

    @Test
    fun aCityIsReportedFromTheThirdStep() {
        var picked: City? = null
        showEngland { picked = it }
        compose.onNodeWithText("West Midlands").performClick()

        compose.onNodeWithText("Dudley").performClick()
        assertEquals(dudley, picked)
    }

    @Test
    fun backFromTheCountyReturnsToEnglandNotTheTopRegionList() {
        showEngland()
        compose.onNodeWithText("West Midlands").performClick()
        compose.onNodeWithText("Birmingham").assertIsDisplayed()

        compose.onNodeWithText("Back").performClick()

        // Back on England's own list — the group row and Cheshire are back.
        compose.onNodeWithText("West Midlands").assertIsDisplayed()
        compose.onNodeWithText("Cheshire").assertIsDisplayed()
        compose.onNodeWithText("Birmingham").assertDoesNotExist()
        // Only one "back" was hit — the nation list never reappeared.
        compose.onNodeWithText("Scotland").assertDoesNotExist()
    }
}
