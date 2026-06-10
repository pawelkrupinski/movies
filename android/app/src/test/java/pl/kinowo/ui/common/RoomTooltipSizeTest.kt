package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.down
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import org.junit.Assert.assertEquals
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
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Off-device (Robolectric) Compose test for the room tooltip that pops on a
 * press-and-hold of a showtime pill. Pins the two things the tooltip must keep:
 *
 *  1. The room name renders at 24sp — half the original oversized 48sp bubble,
 *     still 1.5× a 16sp reference so it stays legible around the finger. A
 *     regression back to 48sp (too big) or down to 16sp (too small) fails here.
 *  2. Popping the tooltip must NOT move the pill. The bubble rides in a Popup
 *     precisely so it can't grow the pill's Box; if it ever went back to being an
 *     in-layout child the Box would resize and shove the chip sideways — caught
 *     here by comparing the chip's bounds before and during the hold.
 *
 * NATIVE graphics gives real text metrics; `xhdpi` (density 2) so dp resolve.
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class RoomTooltipSizeTest {

    @get:Rule
    val compose = createComposeRule()

    private val room = "Sala 8"

    // Sibling references: a 16sp baseline and the tooltip's 24sp (= 1.5×) font.
    // The test pins the tooltip against these rather than an absolute dp that
    // drifts with metrics (font padding doesn't scale, so a raw box-height ratio
    // undershoots the nominal 1.5×).
    private val ref16 = "REF16"
    private val ref24 = "REF24"

    private fun roomFilm() = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        showtimes = listOf(Showtime(time = "12:55", format = "2D", room = room)),
                    ),
                ),
            ),
        ),
    )

    private fun setUp() {
        // The tooltip lives only while the finger stays down (release clears
        // `holding`), so a `longClick()` — which releases — would never leave it
        // on screen. Drive the press manually with autoAdvance off so the pointer
        // is still down when we look.
        compose.mainClock.autoAdvance = false
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.width(200.dp)) {
                        Showings(film = roomFilm(), showCinemaHeaders = false)
                    }
                    Text(ref16, fontSize = 16.sp, fontWeight = FontWeight.SemiBold)
                    Text(ref24, fontSize = 24.sp, fontWeight = FontWeight.SemiBold)
                }
            }
        }
    }

    private fun hold() {
        compose.onNodeWithTag(ShowtimeChipTestTag).performTouchInput { down(center) }
        compose.mainClock.advanceTimeBy(1000L)
        compose.mainClock.advanceTimeByFrame()
    }

    private fun renderAndHold() {
        setUp()
        hold()
    }

    @Test
    fun tooltipTextRendersAt24sp() {
        renderAndHold()

        val tooltipHeight = heightOf(room)
        val small = heightOf(ref16)
        val mid = heightOf(ref24)

        assertTrue("reference text measured no height — metrics are stubbed", small > 0f)
        // The room name matches a 24sp reference — half the original 48sp bubble.
        // The old 48sp tooltip measured ~2× this, and a regression to 16sp would
        // match `small` instead, so both directions fail this bound.
        assertTrue(
            "room name should render at 24sp (half the old 48sp): tooltip=$tooltipHeight dp, " +
                "24sp ref=$mid dp, 16sp ref=$small dp",
            kotlin.math.abs(tooltipHeight - mid) <= 2f,
        )
        // 24sp must still stand clearly above the 16sp baseline (nominal 1.5×;
        // font padding compresses the box ratio, so assert a modest margin).
        assertTrue(
            "24sp must exceed 16sp baseline: mid=$mid dp, small=$small dp",
            mid > small * 1.2f,
        )
    }

    private fun heightOf(text: String): Float =
        compose.onNodeWithText(text).getUnclippedBoundsInRoot().let { (it.bottom - it.top).value }

    @Test
    fun pillDoesNotMoveWhenTheTooltipPops() {
        setUp()
        val before = compose.onNodeWithTag(ShowtimeChipTestTag).getUnclippedBoundsInRoot()
        hold()
        val after = compose.onNodeWithTag(ShowtimeChipTestTag).getUnclippedBoundsInRoot()

        // The bubble pops in a Popup, so it must not resize the pill's Box and
        // shove the chip. Before the Popup fix the in-layout bubble grew the Box
        // and the chip jumped sideways (and the row taller); here it stays put.
        assertEquals("pill left must not move when the tooltip pops",
            before.left.value, after.left.value, 0.5f)
        assertEquals("pill top must not move when the tooltip pops",
            before.top.value, after.top.value, 0.5f)
        assertEquals("pill width must not change when the tooltip pops",
            (before.right - before.left).value, (after.right - after.left).value, 0.5f)
    }
}
