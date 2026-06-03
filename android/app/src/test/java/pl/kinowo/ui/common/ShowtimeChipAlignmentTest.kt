package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.unit.dp
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
 * Off-device (Robolectric) Compose layout test pinning that the showtime chip's
 * time and format tag share one baseline. The bug it guards: the chip's `Row`
 * had no per-child `alignByBaseline`, so Compose defaulted to `Alignment.Top`
 * and the smaller format text rode the top of the larger time instead of
 * sitting on its baseline.
 *
 * It measures each text's box top from the layout and adds that font's
 * baseline-within-box offset (the line's first-baseline, captured from a
 * reference `Text` in the same style) to get each glyph baseline in root space;
 * baseline alignment means the two coincide. NATIVE graphics gives real text
 * metrics. Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class ShowtimeChipAlignmentTest {

    @get:Rule
    val compose = createComposeRule()

    // Two showtimes with different token sets, so the common-token filter strips
    // nothing and the "2D DUB" format actually renders (a single showtime would
    // have all its tokens treated as common and stripped to "").
    private fun film() = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        showtimes = listOf(
                            Showtime(time = "12:55", format = "2D DUB"),
                            Showtime(time = "14:00", format = "3D"),
                        ),
                    ),
                ),
            ),
        ),
    )

    @Test
    fun timeAndFormatShareABaseline() {
        val style = ShowtimeChipStyle()
        var density = 0f
        // First-baseline (px from the layout top) of each font, learned from a
        // reference Text in the exact chip style — independent of the glyphs, so
        // any string works; equals the chip text's baseline-within-box.
        var timeBaselineInBoxPx = -1f
        var formatBaselineInBoxPx = -1f

        compose.setContent {
            KinowoTheme {
                density = LocalDensity.current.density
                Box(Modifier.width(220.dp)) {
                    Showings(film = film(), showCinemaHeaders = false)
                }
                Text(
                    "R",
                    style = pillTextStyle(style.timeFontSize, style.timeWeight),
                    onTextLayout = { timeBaselineInBoxPx = it.firstBaseline },
                )
                Text(
                    "R",
                    style = pillTextStyle(style.formatFontSize, style.formatWeight),
                    onTextLayout = { formatBaselineInBoxPx = it.firstBaseline },
                )
            }
        }

        val time = compose.onNodeWithText("12:55", useUnmergedTree = true).getUnclippedBoundsInRoot()
        val format = compose.onNodeWithText("2D DUB", useUnmergedTree = true).getUnclippedBoundsInRoot()

        assertTrue("baseline metrics weren't captured — onTextLayout didn't fire",
            timeBaselineInBoxPx > 0f && formatBaselineInBoxPx > 0f)
        // The test only means something if the fonts genuinely differ in size.
        assertTrue(
            "time ascent (${timeBaselineInBoxPx}px) should exceed format ascent (${formatBaselineInBoxPx}px)",
            timeBaselineInBoxPx - formatBaselineInBoxPx > 1f,
        )

        // Glyph baseline in root pixels = box top (→px) + baseline-within-box.
        val timeBaselinePx = time.top.value * density + timeBaselineInBoxPx
        val formatBaselinePx = format.top.value * density + formatBaselineInBoxPx

        // Baseline-aligned ⇒ they coincide. Top-aligned (the bug) would put the
        // time baseline below the format's by (timeAscent − formatAscent).
        assertEquals(
            "time and format must share a baseline; got time=${timeBaselinePx}px " +
                "format=${formatBaselinePx}px (Δ=${timeBaselinePx - formatBaselinePx}px). " +
                "Top-aligned regression?",
            timeBaselinePx.toDouble(), formatBaselinePx.toDouble(), 1.5,
        )
    }
}
