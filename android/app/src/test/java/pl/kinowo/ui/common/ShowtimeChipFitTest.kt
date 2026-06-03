package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.material3.MaterialTheme
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertTrue
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
 * Off-device (Robolectric) Compose layout test for the showtime chips. At the
 * 11sp time font the chips no longer fit two per row on a portrait card — that
 * earlier guarantee was deliberately traded for legibility — so the contract this
 * pins is narrower: a single chip (time + a format token) still fits inside the
 * narrowest card's showings column without overflowing or wrapping its own text.
 * Runs on the JVM via `./gradlew testDebugUnitTest`, so it guards CI.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE) // real text metrics, so widths are real
class ShowtimeChipFitTest {

    @get:Rule
    val compose = createComposeRule()

    /** Showings-column width inside one card on the portrait two-column grid:
     *  `ListScreen`'s grid is `GridCells.Fixed(2)` with 12 dp content padding
     *  each side + 12 dp between the columns (36 dp total), and `FilmCard`'s
     *  inner `Column` padding takes 24 dp off. 360 dp (Galaxy S, budget phones)
     *  is the binding narrowest case. */
    private fun cardContentDp(screenDp: Int) = (screenDp - 36) / 2 - 24

    private fun oneShowtimeFilm(time: String, format: String) = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        showtimes = listOf(Showtime(time = time, format = format)),
                    ),
                ),
            ),
        ),
    )

    @Test
    fun aSingleChipFitsTheNarrowestCardWithoutOverflowing() {
        val cardWidthDp = cardContentDp(360)
        compose.setContent {
            MaterialTheme {
                Box(Modifier.width(cardWidthDp.dp)) {
                    // Worst case: time plus a format token.
                    Showings(film = oneShowtimeFilm("22:55", "3D NAP"), showCinemaHeaders = false)
                }
            }
        }

        val chip = compose.onNodeWithTag(ShowtimeChipTestTag).fetchSemanticsNode().boundsInRoot
        // mdpi (density 1) → px ≈ dp; a 1dp slack covers rounding.
        assertTrue(
            "a single showtime chip overflowed the ${cardWidthDp}dp card: " +
                "chip spans ${chip.left}..${chip.right}px",
            chip.right <= cardWidthDp + 1f,
        )
        // Guard the measurement is real (cf. the wrap control below): a stub
        // zero-width renderer would make this trivially true.
        assertTrue("chip measured no width — metrics are stubbed", chip.right - chip.left > 0f)
    }

    @Test
    fun twoChipsWrapWhenTheCardIsFarTooNarrow_provesMeasurementIsReal() {
        // A 50 dp card can't hold one chip beside another. If Robolectric were
        // handing back stub zero-width text, the chips would never wrap and this
        // would fail — so it guards that widths aren't phantom.
        compose.setContent {
            MaterialTheme {
                Box(Modifier.width(50.dp)) {
                    Showings(
                        film = Film(
                            title = "T",
                            showings = listOf(
                                DayShowings(
                                    date = "2026-06-03",
                                    label = "środa",
                                    cinemas = listOf(
                                        CinemaShowings(
                                            cinema = "Kino",
                                            showtimes = listOf(
                                                Showtime(time = "10:00", format = "2D"),
                                                Showtime(time = "22:00", format = "2D"),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        showCinemaHeaders = false,
                    )
                }
            }
        }
        val a = compose.onNodeWithText("10:00", useUnmergedTree = true).fetchSemanticsNode().boundsInRoot
        val b = compose.onNodeWithText("22:00", useUnmergedTree = true).fetchSemanticsNode().boundsInRoot
        assertTrue(
            "expected the two chips to wrap onto separate rows in a 50 dp card",
            b.top > a.top,
        )
    }
}
