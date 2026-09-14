package pl.kinowo.ui.city

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTextInput
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
 * A query now flattens the WHOLE country — region, subregion and city rows
 * merged into one list — instead of only narrowing whichever step is
 * currently open. Mirrors [CityChoiceRegionStepTest]/[CityChoiceSubregionStepTest]'s
 * fixtures, one level deeper.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "en")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityChoiceCrossLevelSearchTest {

    @get:Rule
    val compose = createComposeRule()

    private val losAngeles = City("los-angeles", "Los Angeles", 34.05, -118.24, "us", "California")
    private val austin = City("austin", "Austin", 30.27, -97.74, "us", "Texas")
    private val birmingham = City("birmingham", "Birmingham", 52.46, -1.9, "uk", "England", "West Midlands")
    private val dudley = City("dudley", "Dudley", 52.5, -2.09, "uk", "England", "West Midlands")
    private val cheshire = City("cheshire", "Cheshire", 53.29, -2.5, "uk", "England")

    private val catalog = Catalog(
        countries = Country.all,
        cities = listOf(losAngeles, austin, birmingham, dudley, cheshire),
    )

    @Test
    fun aCityOneLevelDownMatchesFromTheStateRoot() {
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = {}, selectedCountryCode = "us")
        }

        // "Los Angeles" is a city nested under California — the old scoped
        // search never found it until California was already open.
        compose.onNode(hasSetTextAction()).performTextInput("los angeles")
        compose.onNodeWithText("Los Angeles").assertIsDisplayed()
    }

    @Test
    fun pickingAFlattenedCityJumpsStraightToTheRightState() {
        var picked: City? = null
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = { picked = it }, selectedCountryCode = "us")
        }

        compose.onNode(hasSetTextAction()).performTextInput("los angeles")
        compose.onNodeWithText("Los Angeles").performClick()
        assertEquals(losAngeles, picked)
    }

    @Test
    fun aCountyTwoLevelsDownMatchesFromTheNationRoot() {
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = {}, selectedCountryCode = "uk")
        }

        // "West Midlands" is a COUNTY two levels below the nation root — the
        // old scoped search never found it until England was already open.
        compose.onNode(hasSetTextAction()).performTextInput("west midlands")
        compose.onNodeWithText("West Midlands").assertIsDisplayed()
    }

    @Test
    fun pickingAFlattenedCountyLandsTwoLevelsDeepInOneTap() {
        compose.setContent {
            CityChoiceScreen(catalog = catalog, onPick = {}, selectedCountryCode = "uk")
        }

        compose.onNode(hasSetTextAction()).performTextInput("west midlands")
        compose.onNodeWithText("West Midlands").performClick()

        // Landed inside England's own West Midlands county, not merely typed
        // into a narrowed nation list.
        compose.onNodeWithText("Birmingham").assertIsDisplayed()
        compose.onNodeWithText("Dudley").assertIsDisplayed()
        compose.onNodeWithText("Cheshire").assertDoesNotExist()
    }
}
