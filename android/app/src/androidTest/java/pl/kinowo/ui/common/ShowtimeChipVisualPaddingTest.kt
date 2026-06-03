package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asAndroidBitmap
import androidx.compose.ui.test.captureToImage
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.unit.dp
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.ui.theme.KinowoTheme

/**
 * On-device (emulator) **visual** regression for the screening-time chip's
 * vertical tightness — the sibling of [RatingPillVisualPaddingTest] for the blue
 * showtime pills.
 *
 * Why this exists on top of the off-device [ShowtimeChipPaddingTest]: that test
 * measures only the chip's `padding` inset in dp, so it stayed green while the
 * chip still *looked* tall. The chip kept the font's full leading
 * (`includeFontPadding`), so the time digits floated in a band of fill the inset
 * number never saw. This test pixel-samples the real chip and measures the empty
 * band above and below the digit ink.
 *
 * The chip is a single [pl.kinowo.ui.theme.ShowtimeChipBackground] fill under
 * lighter [pl.kinowo.ui.theme.CinemaBlue] time text — one uniform background, so
 * ink vs. fill is unambiguous. The fixture drops the format token so only the
 * "12:55" time renders (one clean glyph band).
 *
 * Runs on a connected device/emulator: `./gradlew app:connectedDebugAndroidTest`.
 * Not part of CI (no emulator layer there).
 */
@RunWith(AndroidJUnit4::class)
class ShowtimeChipVisualPaddingTest {

    @get:Rule
    val compose = createComposeRule()

    private fun oneTimeOnlyFilm() = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        // No format token → just the time, one clean glyph band.
                        showtimes = listOf(Showtime(time = "12:55", format = "")),
                    ),
                ),
            ),
        ),
    )

    @Test
    fun chipHugsTheTimeWithLittleVerticalWhitespace() {
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.width(200.dp)) {
                        Showings(film = oneTimeOnlyFilm(), showCinemaHeaders = false)
                    }
                }
            }
        }

        val bmp = compose.onNodeWithTag(ShowtimeChipTestTag)
            .captureToImage().asAndroidBitmap()

        // Fill reference: top-centre, above the digits and clear of the rounded
        // corners (which clip to the dark background behind).
        val bg = bmp.getPixel(bmp.width / 2, 1)
        val band = GlyphInkMetrics.measure(
            bmp, bg, CORNER_PX until (bmp.width - CORNER_PX), INK_THRESHOLD,
        )

        assertTrue(
            "Showtime chip is too tall: ${band.topGap}px above + ${band.bottomGap}px below a " +
                "${band.inkHeight}px time (chip ${bmp.width}x${bmp.height}, " +
                "gap/ink=${"%.2f".format(band.gapRatio)}). Expected ≤ $MAX_GAP_RATIO — the chip " +
                "should hug the time, not pad it.",
            band.gapRatio <= MAX_GAP_RATIO,
        )
    }

    private companion object {
        /** CinemaBlue on ShowtimeChipBackground is ~400 channel-sum apart. */
        const val INK_THRESHOLD = 150

        /** Rounded-corner radius (6dp ≈ 16px at density 2.625) clipped to the
         *  background behind; sample and scan past it. */
        const val CORNER_PX = 16

        /** Total empty fill (above + below) as a fraction of the time height.
         *  With the font trim and no inset the chip sits at its font-box floor
         *  (≈0.65 here — ≈5px above the caps, ≈6px below the baseline), the same
         *  place the rating pill sits. The threshold guards that floor: the chip
         *  measured 3.06 untrimmed and 1.00 with the inset back, so either
         *  regression fails this. */
        const val MAX_GAP_RATIO = 0.75f
    }
}
