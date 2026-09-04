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
 * Off-device (Robolectric) render of the real `Showings` tree, checking that a
 * showtime says which language version it is — always, on the chip itself.
 *
 * The regression it pins: the chip tag is built by stripping the tokens every
 * showtime at a cinema shares, so a film Multikino screens only dubbed — every
 * slot tagged `2D DUB` — had `DUB` eaten by that intersection and appeared
 * nowhere on the card. A visitor could no longer tell napisy from dubbing. The
 * version is now excluded from the strip, so it stays on the chip a visitor is
 * about to tap however uniform the cinema is.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaVersionChipTest {

    @get:Rule
    val compose = createComposeRule()

    private fun film(vararg formats: String) = Film(
        title = "Minimaraton Spider-Man",
        showings = listOf(
            DayShowings(
                date = "2026-09-04",
                label = "piątek",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Multikino Stary Browar",
                        showtimes = formats.mapIndexed { i, f ->
                            Showtime(time = "1${i}:00", format = f)
                        },
                    ),
                ),
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
    fun aDubbedOnlyCinemaStillSaysDubOnEveryChip() {
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = true)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(2)
        // The shared screen format is still dropped — that is what the
        // stripping is FOR.
        compose.onNodeWithText("2D DUB", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun aSubtitledOnlyCinemaStillSaysNapOnEveryChip() {
        render(film("2D NAP", "2D NAP"), showCinemaHeaders = true)
        compose.onAllNodesWithText("NAP", useUnmergedTree = true).assertCountEquals(2)
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
    fun theVersionIsOnTheChipsWithNoCinemaHeaderToo() {
        // The Kina tab drops the per-card cinema label; the chip is unchanged.
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = false)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(2)
    }
}
