package pl.kinowo.ui.list

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * The first load of a city (before any films arrive — e.g. first app launch)
 * must show a centred spinner, not just a text label. Regression against the
 * old text-only [LoadingState] that rendered no [CircularProgressIndicator].
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
class LoadingStateTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun initialLoadShowsSpinnerAndLabel() {
        compose.setContent { LoadingState("Ładowanie repertuaru…") }
        compose.waitForIdle()

        compose.onNodeWithTag(LoadingSpinnerTag).assertIsDisplayed()
        compose.onNodeWithText("Ładowanie repertuaru…").assertIsDisplayed()
    }
}
