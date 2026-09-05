package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.material3.MaterialTheme
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.assertCountEquals
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.unit.dp
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime

/**
 * Off-device (Robolectric) render of the real `Showings` tree, checking what a
 * chip is left saying once the tokens every slot on the CARD shares are dropped.
 *
 * The rule is one rule for every token: a tag that is on every chip of the card
 * distinguishes nothing, so it goes — a language version included. A film shown
 * only dubbed here shows bare times; the moment two slots differ — two times at
 * one cinema, two cinemas, or two days — the version is what tells them apart
 * and stays on every chip.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class UniformFormatChipTest {

    @get:Rule
    val compose = createComposeRule()

    private fun film(vararg formats: String) = Film(
        title = "Minimaraton Spider-Man",
        showings = listOf(day("2026-09-04", "piątek", "Multikino Stary Browar", *formats)),
    )

    private fun day(date: String, label: String, cinema: String, vararg formats: String) =
        DayShowings(
            date = date,
            label = label,
            cinemas = listOf(
                CinemaShowings(
                    cinema = cinema,
                    showtimes = formats.mapIndexed { i, f -> Showtime(time = "1${i}:00", format = f) },
                ),
            ),
        )

    private fun render(film: Film, showCinemaHeaders: Boolean) {
        compose.setContent {
            MaterialTheme {
                Box(Modifier.width(360.dp)) {
                    Showings(film = film, showCinemaHeaders = showCinemaHeaders)
                }
            }
        }
    }

    @Test
    fun aDubbedOnlyCinemaShowsNoTagAtAll() {
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = true)
        // Every slot is 2D DUB, so neither token separates one from another.
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(0)
        compose.onNodeWithText("2D DUB", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun aSubtitledOnlyCinemaShowsNoTagEither() {
        render(film("2D NAP", "2D NAP"), showCinemaHeaders = true)
        compose.onAllNodesWithText("NAP", useUnmergedTree = true).assertCountEquals(0)
    }

    @Test
    fun aSharedScreenFormatIsDroppedEntirely() {
        render(film("2D", "2D"), showCinemaHeaders = true)
        compose.onNodeWithText("2D", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun aVersionThatDiffersBetweenSlotsIsOnEachChip() {
        render(film("2D NAP", "2D DUB"), showCinemaHeaders = true)
        compose.onNodeWithText("NAP", useUnmergedTree = true).assertExists()
        compose.onNodeWithText("DUB", useUnmergedTree = true).assertExists()
    }

    @Test
    fun theUniformVersionIsDroppedWithNoCinemaHeaderToo() {
        // The Kina tab drops the per-card cinema label; the chip is unchanged.
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = false)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(0)
    }

    @Test
    fun twoCinemasThatDisagreeSayTheirVersionOnEveryChip() {
        // Neither cinema is mixed on its own; the FILM is, so both keep the tag.
        val film = Film(
            title = "Minimaraton Spider-Man",
            showings = listOf(
                DayShowings(
                    date = "2026-09-04",
                    label = "piątek",
                    cinemas = listOf(
                        CinemaShowings("Multikino Stary Browar", showtimes = listOf(
                            Showtime(time = "14:30", format = "2D DUB"),
                            Showtime(time = "17:00", format = "2D DUB"))),
                        CinemaShowings("Helios", showtimes = listOf(
                            Showtime(time = "15:00", format = "2D NAP"),
                            Showtime(time = "19:00", format = "2D NAP"))),
                    ),
                ),
            ),
        )
        render(film, showCinemaHeaders = true)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(2)
        compose.onAllNodesWithText("NAP", useUnmergedTree = true).assertCountEquals(2)
        // The 2D every chip shares is still dropped.
        compose.onNodeWithText("2D DUB", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun twoDaysThatDisagreeSayTheirVersionOnEveryChip() {
        val film = Film(
            title = "Minimaraton Spider-Man",
            showings = listOf(
                day("2026-09-04", "piątek", "Multikino Stary Browar", "2D NAP", "2D NAP"),
                day("2026-09-05", "sobota", "Multikino Stary Browar", "2D DUB", "2D DUB"),
            ),
        )
        render(film, showCinemaHeaders = true)
        compose.onAllNodesWithText("NAP", useUnmergedTree = true).assertCountEquals(2)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(2)
    }
}
