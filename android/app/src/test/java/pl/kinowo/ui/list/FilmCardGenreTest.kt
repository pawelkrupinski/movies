package pl.kinowo.ui.list

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.TestData
import pl.kinowo.ui.common.LocalCitySlug

/**
 * The listing card surfaces a film's genres as pills, mirroring the web
 * `_cardTitle` (`movie.genres.take(3)`) and iOS `FilmCardView`. Only the first
 * three render — the detail screen shows them all.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class FilmCardGenreTest {

    @get:Rule
    val compose = createComposeRule()

    private fun render(genres: List<String>) {
        val film = TestData.film(
            title = "Diuna: Część druga",
            days = emptyList(),
            genres = genres,
        )
        compose.setContent {
            CompositionLocalProvider(LocalCitySlug provides "poznan") {
                FilmCard(film = film, showCinemaHeaders = true, onOpen = {}, onHide = {})
            }
        }
    }

    @Test
    fun cardShowsGenrePills() {
        render(listOf("Sci-Fi", "Przygodowy"))

        compose.onNodeWithText("Sci-Fi").assertIsDisplayed()
        compose.onNodeWithText("Przygodowy").assertIsDisplayed()
    }

    @Test
    fun cardShowsAtMostThreeGenres() {
        render(listOf("Sci-Fi", "Przygodowy", "Dramat", "Akcja"))

        compose.onNodeWithText("Sci-Fi").assertIsDisplayed()
        compose.onNodeWithText("Dramat").assertIsDisplayed()
        // The fourth genre is dropped — the card mirrors the web's take(3).
        compose.onNodeWithText("Akcja").assertDoesNotExist()
    }
}
