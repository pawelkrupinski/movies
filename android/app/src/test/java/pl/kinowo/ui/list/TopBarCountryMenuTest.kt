package pl.kinowo.ui.list

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
import pl.kinowo.model.Country

/**
 * Off-device (Robolectric) Compose test for the top-bar in-app country switcher.
 * Proves the switcher renders when more than one country is deployed, shows the
 * current selection's code, and that opening it and tapping a country reports
 * that country's code back to the caller — the hook the ViewModel's
 * [pl.kinowo.ui.KinowoViewModel.setCountry] is wired to.
 *
 * This is a fail-before/pass-after guard: it can't compile until [CountryMenu]
 * exists, and its dropdown assertions fail if the menu stops surfacing the
 * deployed countries or stops reporting the picked one.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class TopBarCountryMenuTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun triggerShowsTheCurrentCountryCode() {
        compose.setContent { CountryMenu(selectedCode = "GB", onSelect = {}) }
        // The compact trigger renders the current country's 2-letter code.
        compose.onNodeWithText("GB").assertIsDisplayed()
        // The menu is closed until tapped, so no option labels are shown yet.
        compose.onNodeWithText(Country.byCode("GB").displayName).assertDoesNotExist()
    }

    @Test
    fun openingAndPickingACountryReportsItsCode() {
        var picked: String? = null
        // null selection resolves to the default (Poland) at the registry.
        compose.setContent { CountryMenu(selectedCode = null, onSelect = { picked = it }) }

        // Default shows Poland's code on the trigger.
        compose.onNodeWithText("PL").assertIsDisplayed()

        // Open the dropdown: every deployed country's display name appears.
        compose.onNodeWithText("PL").performClick()
        compose.onNodeWithText("Polska").assertIsDisplayed()
        compose.onNodeWithText("United Kingdom").assertIsDisplayed()

        // Picking the UK reports its ISO code — the value setCountry persists.
        compose.onNodeWithText("United Kingdom").performClick()
        assertEquals("GB", picked)
    }
}
