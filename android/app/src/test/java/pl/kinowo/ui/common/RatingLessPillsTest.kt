package pl.kinowo.ui.common

import androidx.compose.material3.MaterialTheme
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.assertCountEquals
import androidx.compose.ui.test.assertHasClickAction
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Ratings

/**
 * A source we hold a link for but no score yet (a new release) still gets its IMDb / Filmweb
 * pill, only its coloured label tab (no score tab) and an accessible "no rating yet" label; a rated pill is
 * unchanged. Off-device Compose (Robolectric), like [RatingPillPaddingTest].
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class RatingLessPillsTest {

    @get:Rule
    val compose = createComposeRule()

    private fun assertPills(imdbLabel: String, filmwebLabel: String) {
        compose.setContent {
            MaterialTheme {
                RatingBadges(Ratings(
                    imdbURL = "https://www.imdb.com/title/tt7654321/",
                    filmwebURL = "https://www.filmweb.pl/film/X-1", filmwebRatingPending = true,
                ))
            }
        }
        compose.onNodeWithContentDescription(imdbLabel).assertIsDisplayed().assertHasClickAction()
        compose.onNodeWithContentDescription(filmwebLabel).assertIsDisplayed().assertHasClickAction()
        // the visible text is the label tab alone, once per pill, whatever the language
        compose.onAllNodesWithText("IMDb", useUnmergedTree = true).assertCountEquals(1)
        compose.onAllNodesWithText("FW", useUnmergedTree = true).assertCountEquals(1)
    }

    @Test
    @Config(qualifiers = "pl-xhdpi")
    fun polishLabels() = assertPills("IMDb – brak oceny", "Filmweb – brak oceny")

    @Test
    @Config(qualifiers = "de-xhdpi")
    fun germanLabels() = assertPills("IMDb – noch keine Bewertung", "Filmweb – noch keine Bewertung")

    @Test
    @Config(qualifiers = "en-xhdpi")
    fun englishLabels() = assertPills("IMDb, no rating yet", "Filmweb, no rating yet")

    @Test
    @Config(qualifiers = "es-xhdpi")
    fun spanishLabels() = assertPills("IMDb – sin valoración todavía", "Filmweb – sin valoración todavía")

    @Test
    fun aFilmwebSearchLinkDrawsNoPill() {
        compose.setContent {
            MaterialTheme { RatingBadges(Ratings(imdb = 7.0, filmwebURL = "https://www.filmweb.pl/search?query=X")) }
        }
        compose.onNodeWithText("IMDb", useUnmergedTree = true).assertIsDisplayed()
        compose.onNodeWithContentDescription("Filmweb, no rating yet").assertDoesNotExist()
        compose.onNodeWithContentDescription("Filmweb – brak oceny").assertDoesNotExist()
        compose.onNodeWithText("FW", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun ratedPillsKeepTheirScoreAndHaveNoNoRatingLabel() {
        compose.setContent {
            MaterialTheme {
                RatingBadges(Ratings(imdb = 7.0, imdbURL = "https://www.imdb.com/title/tt1/",
                    filmweb = 6.5, filmwebURL = "https://www.filmweb.pl/film/X-1", filmwebRatingPending = true))
            }
        }
        compose.onNodeWithText("7.0", useUnmergedTree = true).assertIsDisplayed()
        compose.onNodeWithText("6.5", useUnmergedTree = true).assertIsDisplayed()
        compose.onNodeWithContentDescription("IMDb, no rating yet").assertDoesNotExist()
        compose.onNodeWithContentDescription("Filmweb, no rating yet").assertDoesNotExist()
        compose.onNodeWithContentDescription("Filmweb – brak oceny").assertDoesNotExist()
    }
}
