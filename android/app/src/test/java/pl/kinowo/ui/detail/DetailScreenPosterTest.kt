package pl.kinowo.ui.detail

import androidx.compose.ui.test.click
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.longClick
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.test.swipeDown
import androidx.compose.ui.test.swipeUp
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Film
import pl.kinowo.ui.common.FullScreenPosterTag

/**
 * Off-device (Robolectric) Compose test for the full-screen poster viewer:
 * tapping or long-pressing the detail-header poster opens it, and a tap on the
 * backdrop dismisses it. Mirrors the iOS full-screen-cover behaviour.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class DetailScreenPosterTest {

    @get:Rule
    val compose = createComposeRule()

    private val film = Film(title = "Diuna", posterURL = "https://example.com/diuna.jpg")

    @Test
    fun tappingPosterOpensFullScreenViewer() {
        compose.setContent { DetailScreen(film, details = null, onBack = {}) }

        compose.onNodeWithTag(FullScreenPosterTag).assertDoesNotExist()
        compose.onNodeWithTag(DetailPosterTag).performClick()
        compose.onNodeWithTag(FullScreenPosterTag).assertExists()
    }

    @Test
    fun longPressingPosterOpensFullScreenViewer() {
        compose.setContent { DetailScreen(film, details = null, onBack = {}) }

        compose.onNodeWithTag(DetailPosterTag).performTouchInput { longClick() }
        compose.onNodeWithTag(FullScreenPosterTag).assertExists()
    }

    @Test
    fun tappingBackdropDismissesViewer() {
        compose.setContent { DetailScreen(film, details = null, onBack = {}) }

        compose.onNodeWithTag(DetailPosterTag).performClick()
        compose.onNodeWithTag(FullScreenPosterTag).assertExists()

        compose.onNodeWithTag(FullScreenPosterTag).performTouchInput { click() }
        compose.onNodeWithTag(FullScreenPosterTag).assertDoesNotExist()
    }

    @Test
    fun swipingUpDismissesViewer() {
        compose.setContent { DetailScreen(film, details = null, onBack = {}) }

        compose.onNodeWithTag(DetailPosterTag).performClick()
        compose.onNodeWithTag(FullScreenPosterTag).assertExists()

        compose.onNodeWithTag(FullScreenPosterTag).performTouchInput { swipeUp() }
        compose.onNodeWithTag(FullScreenPosterTag).assertDoesNotExist()
    }

    @Test
    fun swipingDownDismissesViewer() {
        compose.setContent { DetailScreen(film, details = null, onBack = {}) }

        compose.onNodeWithTag(DetailPosterTag).performClick()
        compose.onNodeWithTag(FullScreenPosterTag).assertExists()

        compose.onNodeWithTag(FullScreenPosterTag).performTouchInput { swipeDown() }
        compose.onNodeWithTag(FullScreenPosterTag).assertDoesNotExist()
    }

    @Test
    fun posterlessFilmNeverOpensViewer() {
        compose.setContent { DetailScreen(Film(title = "Bez plakatu"), details = null, onBack = {}) }

        compose.onNodeWithTag(DetailPosterTag).performClick()
        compose.onNodeWithTag(FullScreenPosterTag).assertDoesNotExist()
    }
}
