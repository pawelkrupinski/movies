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
 * chip is left saying once the tokens every slot at a cinema shares are dropped.
 *
 * The rule is one rule for every token: a tag that is on every chip in the group
 * distinguishes nothing, so it goes — a language version included. A cinema that
 * screens a film only dubbed shows bare times; the moment two slots differ, the
 * version is what tells them apart and stays on each chip.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class UniformFormatChipTest {

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
}
