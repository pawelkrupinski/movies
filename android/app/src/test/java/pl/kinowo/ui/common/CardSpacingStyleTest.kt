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
import pl.kinowo.model.DateLabel
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
 * must be meaningfully larger than at the 4.5 dp default — which only holds if the
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

    // Two films with distinct DATES so the two stacked cards' day labels — which
    // `Showings` renders from `date` via `DateLabel.format`, not `label` — can be
    // told apart in one composition. `label` is set to something else entirely,
    // to prove nothing reads it.
    private fun twoDayFilm(title: String, dateA: String, dateB: String) = Film(
        title = title,
        showings = listOf(
            DayShowings(
                date = dateA,
                label = "unused",
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "12:55", format = "2D"))),
                ),
            ),
            DayShowings(
                date = dateB,
                label = "unused",
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "16:00", format = "2D"))),
                ),
            ),
        ),
    )

    // One day, one named cinema, so the day-label → cinema-name gap is measurable.
    private fun oneCinemaFilm(title: String, date: String, cinema: String) = Film(
        title = title,
        showings = listOf(
            DayShowings(
                date = date,
                label = "unused",
                cinemas = listOf(
                    CinemaShowings(cinema = cinema, showtimes = listOf(Showtime(time = "12:55", format = "2D"))),
                ),
            ),
        ),
    )

    @androidx.compose.runtime.Composable
    private fun card(film: Film, style: CardSpacingStyle, showCinemaHeaders: Boolean = false) {
        CompositionLocalProvider(LocalCardSpacingStyle provides style) {
            Box(Modifier.width(180.dp)) {
                FilmCard(film = film, showCinemaHeaders = showCinemaHeaders, onOpen = {}, onHide = {})
            }
        }
    }

    /** Vertical span from the top of [dateA]'s rendered label to the top of
     *  [dateB]'s — the `showingsBlock` gap is part of this distance. */
    private fun daySpan(dateA: String, dateB: String): Float {
        // The card's combinedClickable merges its descendants, so the day labels
        // are only addressable in the unmerged tree.
        val first = compose.onNodeWithText(DateLabel.format(dateA).uppercase(), useUnmergedTree = true).getUnclippedBoundsInRoot()
        val second = compose.onNodeWithText(DateLabel.format(dateB).uppercase(), useUnmergedTree = true).getUnclippedBoundsInRoot()
        return (second.top - first.top).value
    }

    /**
     * One composition, two cards: the first driven by a 40 dp `showingsBlock`, the
     * second by the default. The wide card's day-to-day span must exceed the
     * default card's — only true if [LocalCardSpacingStyle] feeds the showings
     * block. Stub the lever (or hard-code the gap) and the two spans are equal → fail.
     */
    @Test
    fun showingsBlockGapWidensTheDaySpacing() {
        // Short weekday + short month (well under the card's wrap width at
        // 180dp — "WEDNESDAY 10 JUNE" wraps to a second line there, which
        // would inflate that card's OWN span and corrupt the isolation this
        // test relies on).
        compose.setContent {
            KinowoTheme {
                Row {
                    card(twoDayFilm("WIDE", "2026-06-01", "2026-06-05"), CardSpacingStyle(showingsBlock = 40.dp))
                    card(twoDayFilm("DEF", "2026-06-07", "2026-06-02"), CardSpacingStyle())
                }
            }
        }

        val wide = daySpan("2026-06-01", "2026-06-05")
        val default = daySpan("2026-06-07", "2026-06-02")

        assertTrue("day-label span measured no height — metrics are stubbed", default > 0f)
        assertTrue(
            "showingsBlock=40dp must widen the day-to-day span vs default: " +
                "wide=$wide default=$default",
            wide - default > 20f,
        )
    }

    /** Gap from the bottom of the day label to the top of the cinema name — the
     *  `dayToCinema` lever is exactly this distance. */
    private fun dayToCinemaGap(date: String, cinema: String): Float {
        val dayNode = compose.onNodeWithText(DateLabel.format(date).uppercase(), useUnmergedTree = true).getUnclippedBoundsInRoot()
        val cinemaNode = compose.onNodeWithText(cinema, useUnmergedTree = true).getUnclippedBoundsInRoot()
        return (cinemaNode.top - dayNode.bottom).value
    }

    /**
     * `dayToCinema` is a SEPARATE lever from `showingsBlock`: with both cards held
     * at the same `showingsBlock`, bumping only `dayToCinema` to 40 dp must widen
     * the day-label → cinema-name gap while leaving everything else alone. Before
     * the lever existed that gap was driven by `showingsBlock`, so a `dayToCinema`
     * override would do nothing → fail. Both cards use an explicit `dayToCinema`
     * (the shipping default is now 0, which would measure no gap for the
     * baseline), so the difference is purely the lever.
     */
    @Test
    fun dayToCinemaGapIsTunableIndependentlyOfShowingsBlock() {
        compose.setContent {
            KinowoTheme {
                Row {
                    card(oneCinemaFilm("WIDE", "2026-06-08", "WideKino"), CardSpacingStyle(dayToCinema = 40.dp), showCinemaHeaders = true)
                    card(oneCinemaFilm("DEF", "2026-06-09", "DefKino"), CardSpacingStyle(dayToCinema = 8.dp), showCinemaHeaders = true)
                }
            }
        }

        val wide = dayToCinemaGap("2026-06-08", "WideKino")
        val narrow = dayToCinemaGap("2026-06-09", "DefKino")

        assertTrue("day→cinema gap measured no height — metrics are stubbed", narrow > 0f)
        assertTrue(
            "dayToCinema=40dp must widen the day→cinema gap vs 8dp (both share showingsBlock): " +
                "wide=$wide narrow=$narrow",
            wide - narrow > 20f,
        )
    }
}
