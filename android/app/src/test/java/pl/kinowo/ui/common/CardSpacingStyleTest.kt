package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
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
import pl.kinowo.ui.list.FilmCard
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Off-device (Robolectric) Compose layout test pinning that the
 * [LocalCardSpacingStyle] gaps actually drive the card. It renders a [FilmCard]
 * with two day sections and measures the vertical gap between the two day
 * labels; the gap is the `showingsBlock` spacing. With a 40 dp override the gap
 * must be meaningfully larger than at the 8 dp default — which only holds if the
 * lever is wired through to the showings block. NATIVE graphics for real
 * metrics; `xhdpi` so sub-dp differences resolve.
 *
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CardSpacingStyleTest {

    @get:Rule
    val compose = createComposeRule()

    // Two films with distinct day labels so the two stacked cards' labels can be
    // told apart in one composition.
    private fun twoDayFilm(title: String, dayA: String, dayB: String) = Film(
        title = title,
        showings = listOf(
            DayShowings(
                date = "2026-06-08",
                label = dayA,
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "12:55", format = "2D"))),
                ),
            ),
            DayShowings(
                date = "2026-06-09",
                label = dayB,
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "16:00", format = "2D"))),
                ),
            ),
        ),
    )

    @androidx.compose.runtime.Composable
    private fun card(film: Film, style: CardSpacingStyle) {
        CompositionLocalProvider(LocalCardSpacingStyle provides style) {
            Box(Modifier.width(180.dp)) {
                FilmCard(film = film, showCinemaHeaders = false, onOpen = {}, onHide = {})
            }
        }
    }

    /** Vertical span from the top of [dayA]'s label to the top of [dayB]'s — the
     *  `showingsBlock` gap is part of this distance. */
    private fun daySpan(dayA: String, dayB: String): Float {
        // The card's combinedClickable merges its descendants, so the day labels
        // are only addressable in the unmerged tree.
        val first = compose.onNodeWithText(dayA.uppercase(), useUnmergedTree = true).getUnclippedBoundsInRoot()
        val second = compose.onNodeWithText(dayB.uppercase(), useUnmergedTree = true).getUnclippedBoundsInRoot()
        return (second.top - first.top).value
    }

    /**
     * One composition, two cards: the first driven by a 40 dp `showingsBlock`, the
     * second by the 8 dp default. The wide card's day-to-day span must exceed the
     * default card's — only true if [LocalCardSpacingStyle] feeds the showings
     * block. Stub the lever (or hard-code 8 dp) and the two spans are equal → fail.
     */
    @Test
    fun showingsBlockGapWidensTheDaySpacing() {
        compose.setContent {
            KinowoTheme {
                Row {
                    card(twoDayFilm("WIDE", "WIDEPON", "WIDEWTO"), CardSpacingStyle(showingsBlock = 40.dp))
                    card(twoDayFilm("DEF", "DEFPON", "DEFWTO"), CardSpacingStyle())
                }
            }
        }

        val wide = daySpan("WIDEPON", "WIDEWTO")
        val default = daySpan("DEFPON", "DEFWTO")

        assertTrue("day-label span measured no height — metrics are stubbed", default > 0f)
        assertTrue(
            "showingsBlock=40dp must widen the day-to-day span vs default 8dp: " +
                "wide=$wide default=$default",
            wide - default > 20f,
        )
    }
}
