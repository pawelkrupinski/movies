package pl.kinowo.ui.list

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.unit.dp
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * `CinemaGrid`'s empty state ("no showings for this selection", reached from
 * Filtry's cinema-grouped view) used to hardcode the Polish literal
 * "Brak repertuaru." regardless of the app's language — unlike `FilmsGrid`'s
 * identical empty state a few lines away, which correctly reads
 * `R.string.no_showings`. Under `en` this must show the English string, not
 * the Polish one.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "en")
class CinemaGridEmptyStateLocalizationTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun emptyCinemaGridShowsTheLocalizedMessageUnderAnEnglishLocale() {
        compose.setContent {
            CinemaGrid(
                sections = emptyList(),
                showHeaders = true,
                bottomInset = 0.dp,
                scrollResetKey = null,
                onOpen = {},
                onHide = {},
            )
        }

        compose.onNodeWithText("No showings.").assertIsDisplayed()
        compose.onNodeWithText("Brak repertuaru.").assertDoesNotExist()
    }
}
