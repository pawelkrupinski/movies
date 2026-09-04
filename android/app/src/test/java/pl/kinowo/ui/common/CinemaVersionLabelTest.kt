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
 * cinema screening a film in ONE language version still says which.
 *
 * The regression it pins: the chip tag is built by stripping the tokens every
 * showtime at a cinema shares, so a film Multikino screens only dubbed — every
 * slot tagged `2D DUB` — had `DUB` eaten by that intersection and appeared
 * nowhere on the card. A visitor could no longer tell napisy from dubbing. The
 * version is now hoisted into the cinema label (or, where there is no label to
 * hoist it into, left on the chips).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaVersionLabelTest {

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
    fun aDubbedOnlyCinemaSaysDubBesideItsName() {
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = true)
        // Once, in the label — not on either chip, which stay as narrow as they were.
        compose.onNodeWithText("DUB", useUnmergedTree = true).assertExists()
        compose.onNodeWithText("2D DUB", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun aSubtitledOnlyCinemaSaysNapBesideItsName() {
        render(film("2D NAP", "2D NAP"), showCinemaHeaders = true)
        compose.onNodeWithText("NAP", useUnmergedTree = true).assertExists()
    }

    @Test
    fun aSharedScreenFormatIsStillDroppedEntirely() {
        render(film("2D", "2D"), showCinemaHeaders = true)
        // "2D" on every chip tells a visitor nothing — that is what the
        // stripping is FOR, and it keeps working.
        compose.onNodeWithText("2D", useUnmergedTree = true).assertDoesNotExist()
    }

    @Test
    fun aVersionThatDiffersBetweenSlotsStaysOnTheChips() {
        render(film("2D NAP", "2D DUB"), showCinemaHeaders = true)
        // Nothing to hoist — the chips are the only place that can distinguish them.
        compose.onNodeWithText("NAP", useUnmergedTree = true).assertExists()
        compose.onNodeWithText("DUB", useUnmergedTree = true).assertExists()
    }

    @Test
    fun withNoCinemaLabelTheVersionStaysOnEveryChip() {
        // The Kina tab's section already names the cinema, but is shared by films
        // with different versions — so there the chip is the only place it fits,
        // and it has to be on BOTH chips (there is no label to say it once).
        render(film("2D DUB", "2D DUB"), showCinemaHeaders = false)
        compose.onAllNodesWithText("DUB", useUnmergedTree = true).assertCountEquals(2)
    }
}
