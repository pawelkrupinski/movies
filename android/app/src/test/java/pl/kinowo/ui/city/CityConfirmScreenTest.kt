package pl.kinowo.ui.city

import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
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
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
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
}
