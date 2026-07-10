package pl.kinowo.ui.city

import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Cities

/**
 * Off-device (Robolectric) Compose test for the first-launch confirm screen:
 * it names the detected city and routes the two choices to the right callbacks
 * (adopt it vs. fall through to the manual picker).
 */
// Pin the resource locale to Polish (the app's default deployment): the screen
// now reads its strings from resources, so under Robolectric's default en-US
// locale it would render the values-en translations instead of the Polish text
// these assertions expect.
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CityConfirmScreenTest {

    @get:Rule
    val compose = createComposeRule()

    private val poznan = Cities.all.first { it.slug == "poznan" }

    @Test
    fun confirmAdoptsTheDetectedCity() {
        var confirmed = false
        var choseOther = false
        compose.setContent {
            CityConfirmScreen(poznan, onConfirm = { confirmed = true }, onChooseOther = { choseOther = true })
        }
        // Exact match → just the title (the confirm button also contains the name).
        compose.onNodeWithText(poznan.name).assertExists()
        compose.onNodeWithText("Pokaż repertuar", substring = true).performClick()
        assertTrue("tapping confirm should adopt the detected city", confirmed)
        assertFalse(choseOther)
    }

    @Test
    fun chooseOtherFallsThroughToManualPick() {
        var confirmed = false
        var choseOther = false
        compose.setContent {
            CityConfirmScreen(poznan, onConfirm = { confirmed = true }, onChooseOther = { choseOther = true })
        }
        compose.onNodeWithText("Wybierz inne miasto").performClick()
        assertTrue("tapping 'choose other' should route to the manual picker", choseOther)
        assertFalse(confirmed)
    }

    /**
     * Big native controls: both choices are tall touch targets, not Material's
     * compact 40dp default. The secondary used to be a thin `TextButton` link;
     * it's now a tall `OutlinedButton`. Fails if either control shrinks back.
     */
    @Test
    fun confirmScreenUsesTallNativeControls() {
        compose.setContent {
            CityConfirmScreen(poznan, onConfirm = {}, onChooseOther = {})
        }
        val minPx = with(compose.density) { 52.dp.toPx() }

        fun height(text: String) =
            compose.onNodeWithText(text, substring = true).fetchSemanticsNode().boundsInRoot.height

        val primary = height("Pokaż repertuar")
        val secondary = height("Wybierz inne miasto")
        assertTrue("primary control should be a tall (~56dp) button, was ${primary}px", primary >= minPx)
        assertTrue("secondary control should be a tall (~56dp) button, was ${secondary}px", secondary >= minPx)
    }
}
