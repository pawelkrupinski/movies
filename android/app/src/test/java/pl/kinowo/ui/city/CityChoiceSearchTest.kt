package pl.kinowo.ui.city

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

/**
 * Off-device (Robolectric) Compose test for the manual city picker's search
 * box: typing narrows the rendered list by diacritic-insensitive substring
 * match, and tapping a surviving row reports the right city.
 */
// Pin the resource locale to Polish (the app's default deployment): the screen
// now reads its strings from resources, so under Robolectric's default en-US
// locale the empty-state text would render the values-en translation instead of
// the Polish "Brak miasta…" this test expects.
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityChoiceSearchTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun typingNarrowsTheListDiacriticInsensitively() {
        compose.setContent { CityChoiceScreen(catalog = Catalog.fallback, onPick = {}) }

        // Białystok leads the Polish-collated list, so it's rendered up front.
        compose.onNodeWithText("Białystok").assertExists()

        // "wroc" — typed without diacritics — keeps Wrocław, drops the rest.
        compose.onNode(hasSetTextAction()).performTextInput("wroc")
        compose.onNodeWithText("Wrocław").assertExists()
        compose.onNodeWithText("Białystok").assertDoesNotExist()
        compose.onNodeWithText("Kraków").assertDoesNotExist()
    }

    @Test
    fun diacriticTypedQueryFindsThePolishCity() {
        compose.setContent { CityChoiceScreen(catalog = Catalog.fallback, onPick = {}) }

        // "lodz" must surface "Łódź" (ł/ó folded away on both sides).
        compose.onNode(hasSetTextAction()).performTextInput("lodz")
        compose.onNodeWithText("Łódź").assertExists()
        compose.onNodeWithText("Warszawa").assertDoesNotExist()
    }

    @Test
    fun noMatchShowsTheEmptyState() {
        compose.setContent { CityChoiceScreen(catalog = Catalog.fallback, onPick = {}) }

        compose.onNode(hasSetTextAction()).performTextInput("zzzzz")
        compose.onNodeWithText("Brak miasta", substring = true).assertExists()
        // No city rows survive (Białystok led the unfiltered list).
        compose.onNodeWithText("Białystok").assertDoesNotExist()
    }

    @Test
    fun tappingAFilteredCityReportsIt() {
        var picked: City? = null
        compose.setContent { CityChoiceScreen(catalog = Catalog.fallback, onPick = { picked = it }) }

        compose.onNode(hasSetTextAction()).performTextInput("wroc")
        compose.onNodeWithText("Wrocław").performClick()
        assertEquals("wroclaw", picked?.slug)
    }
}
