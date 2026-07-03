package pl.kinowo.ui.list

import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
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

/**
 * The top-bar cinema pill selector [CinemaPillBar]:
 *
 *  1. [handleTogglesPillRow] — the pill row is hidden until the collapsed handle
 *     is tapped, then unfolds to reveal the leading "Wszystkie" pill and one pill
 *     per cinema (by SHORT name), and folds away again on a second tap.
 *  2. [selectingCinemaPillReportsSingleSelection] — tapping a cinema pill reports
 *     exactly that cinema; tapping "Wszystkie" clears it back to null. The bar is
 *     driven single-select by its `selected` input, so at most one pill is ever
 *     active.
 *
 * Rendered inside a 1280 dp host (as DayPillFitTest does) so Robolectric doesn't
 * clamp the bar to its default 320 dp width.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "w1280dp-h800dp")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaPillBarTest {

    @get:Rule
    val compose = createComposeRule()

    // Real long names → the pill row must render their SHORT labels (pillName).
    private val cinemas = listOf("Kino Apollo", "Helios Posnania")

    @Test
    fun handleTogglesPillRow() {
        compose.setContent {
            MaterialTheme {
                CinemaPillBar(cinemas = cinemas, selected = null, onSelect = {})
            }
        }

        // Collapsed: the handle is shown but no pills are.
        compose.onNodeWithText("Wszystkie kina").assertIsDisplayed()
        compose.onNodeWithText("Wszystkie").assertDoesNotExist()
        compose.onNodeWithText("Apollo").assertDoesNotExist()

        // Expand — the leading pill and both short cinema names appear.
        compose.onNodeWithText("Wszystkie kina").performClick()
        compose.onNodeWithText("Wszystkie").assertIsDisplayed()
        compose.onNodeWithText("Apollo").assertIsDisplayed()
        compose.onNodeWithText("Helios").assertIsDisplayed()

        // Collapse again — pills disappear.
        compose.onNodeWithText("Wszystkie kina").performClick()
        compose.onNodeWithText("Apollo").assertDoesNotExist()
    }

    @Test
    fun selectingCinemaPillReportsSingleSelection() {
        var reported: String? = "unset"
        compose.setContent {
            var selected by remember { mutableStateOf<String?>(null) }
            MaterialTheme {
                CinemaPillBar(
                    cinemas = cinemas,
                    selected = selected,
                    onSelect = { selected = it; reported = it },
                )
            }
        }

        compose.onNodeWithText("Wszystkie kina").performClick() // expand
        compose.onNodeWithText("Apollo").performClick()         // unique while nothing is picked
        assertEquals("Kino Apollo", reported)

        // Selecting "Wszystkie" clears the pick back to null (single-select).
        compose.onNodeWithText("Wszystkie").performClick()
        assertEquals(null, reported)
    }
}
