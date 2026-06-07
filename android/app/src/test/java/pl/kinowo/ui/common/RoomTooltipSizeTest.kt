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
 * press-and-hold of a showtime pill. Pins the two things a fat-finger demands:
 *
 *  1. The room name is rendered ~3× its old size — its glyph box is roughly three
 *     times a 16sp reference (the previous tooltip font). A regression back to
 *     16sp collapses the ratio to ~1 and fails here.
 *  2. The bubble's bottom edge floats well clear of the pill the thumb is
 *     pressing, so the finger doesn't obscure the name. Drop the lift and the
 *     bubble overlaps the chip top, failing the clearance check.
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

    // Sibling references: the tooltip's old 16sp font and its new 48sp (= 3×) font.
    // The test pins the tooltip against these rather than an absolute dp that
    // drifts with metrics (font padding doesn't scale, so a raw box-height ratio
    // undershoots 3×).
    private val ref16 = "REF16"
    private val ref48 = "REF48"

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

    private fun renderAndHold() {
        // The tooltip lives only while the finger stays down (release clears
        // `holding`), so a `longClick()` — which releases — would never leave it
        // on screen. Press, hold, and advance the clock past the long-press
        // timeout with autoAdvance off so the pointer is still down when we look.
        compose.mainClock.autoAdvance = false
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.width(200.dp)) {
                        Showings(film = roomFilm(), showCinemaHeaders = false)
                    }
                    Text(ref16, fontSize = 16.sp, fontWeight = FontWeight.SemiBold)
                    Text(ref48, fontSize = 48.sp, fontWeight = FontWeight.SemiBold)
                }
            }
        }
        compose.onNodeWithTag(ShowtimeChipTestTag).performTouchInput { down(center) }
        compose.mainClock.advanceTimeBy(1000L)
        compose.mainClock.advanceTimeByFrame()
    }

    @Test
    fun tooltipTextIsThreeTimesItsOldSize() {
        renderAndHold()

        val tooltipHeight = heightOf(room)
        val small = heightOf(ref16)
        val big = heightOf(ref48)

        assertTrue("reference text measured no height — metrics are stubbed", small > 0f)
        // The room name now matches a 48sp reference (3× the old 16sp), and that
        // 48sp reference towers over the 16sp one. The old 16sp tooltip matched
        // `small`, not `big`, so this fails before the change.
        assertTrue(
            "room name should render at 48sp (3× old 16sp): tooltip=$tooltipHeight dp, " +
                "48sp ref=$big dp, 16sp ref=$small dp",
            kotlin.math.abs(tooltipHeight - big) <= 2f,
        )
        assertTrue(
            "48sp must dwarf 16sp: big=$big dp, small=$small dp",
            big > small * 2f,
        )
    }

    private fun heightOf(text: String): Float =
        compose.onNodeWithText(text).getUnclippedBoundsInRoot().let { (it.bottom - it.top).value }

    @Test
    fun tooltipBottomFloatsClearOfTheHeldPill() {
        renderAndHold()

        val chip = compose.onNodeWithTag(ShowtimeChipTestTag).getUnclippedBoundsInRoot()
        val tooltip = compose.onNodeWithText(room).getUnclippedBoundsInRoot()

        // Gap between the bubble's bottom and the pill's top: the finger is on the
        // pill, so a positive, generous gap means the name sits above the thumb.
        val gap = (chip.top - tooltip.bottom).value
        assertTrue(
            "room tooltip must float clear of the held pill (gap above chip top = $gap dp)",
            gap >= 30f,
        )
    }
}
