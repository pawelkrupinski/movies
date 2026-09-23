package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Column
import androidx.compose.material3.MaterialTheme
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.graphics.asAndroidBitmap
import androidx.compose.ui.test.captureToImage
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.model.Ratings

/**
 * On-device (emulator) **visual** regression for the rating pill's vertical
 * tightness — the thing the eye reads as "padding".
 *
 * Why this exists on top of the off-device [RatingPillPaddingTest]: that test
 * measures only the `padding` modifier in dp, so it stays green even when the
 * pill *looks* tall, because the digits float inside the font's reserved leading
 * that the padding number knows nothing about. This test instead pixel-samples
 * the actually-rendered IMDb label cell and measures the empty band of pill
 * fill above and below the glyph ink. That band — padding *plus* whatever line
 * box the glyph sits in — is what makes a pill read as over-padded. The tuned
 * `vPad` (3dp since the June tuning pass) is deliberate, so the test subtracts
 * it and gates what's left: the font box around the glyph, which must stay
 * trimmed however the padding is later tuned.
 *
 * The IMDb label cell is sampled because it's a single uniform fill
 * ([pl.kinowo.ui.theme.ImdbYellow]) under black text — maximal ink/background
 * contrast — and its caps/ascenders ("IMDb", no descenders) give a clean ink
 * band ≈ cap height. The chosen cell shares `vPad` + `pillTextStyle` with every
 * other pill, so it stands in for all of them.
 *
 * Runs on a connected device/emulator: `./gradlew app:connectedDebugAndroidTest`.
 * Not part of CI (no emulator layer there) — it's the screenshot-in-code gauge
 * for tuning the pill height, per the project's "pixel-sample the rendered
 * result" rule for visible UX changes.
 */
@RunWith(AndroidJUnit4::class)
class RatingPillVisualPaddingTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun pillHugsTheScoreWithLittleVerticalWhitespace() {
        var vPadPx = 0f
        compose.setContent {
            // The same scaled vPad RatingBadges applies (see RatingBadgeMetrics).
            val scale = RatingBadgeMetrics.scale(layoutWidthDp())
            vPadPx = with(LocalDensity.current) { (LocalRatingPillStyle.current.vPad * scale).toPx() }
            MaterialTheme {
                Column {
                    RatingBadges(Ratings(imdb = 7.4, imdbURL = "https://imdb.com/x"))
                }
            }
        }

        // Capture the whole pill (merged clickable row) so the bitmap includes the
        // vertical padding — an unmerged text node is just the font box and hides
        // the padding entirely. The pill is the yellow "IMDb" label cell beside the
        // dark value cell; we sample only the yellow cell, where black-on-yellow is
        // unambiguous.
        val bmp = compose.onNodeWithText("IMDb", substring = true)
            .captureToImage().asAndroidBitmap()

        // Yellow fill reference: top row, but past the rounded-corner radius
        // (~5dp ≈ 13px) which clips to the dark background behind. Row 1 sits above
        // the caps, so it's always fill, never glyph ink.
        val yellow = bmp.getPixel(CORNER_PX + 4, 1)
        // Right edge of the yellow label cell: scan the top row until the fill turns
        // dark (the value cell begins) — so ink scanning stays inside the label.
        var labelRight = CORNER_PX + 4
        while (labelRight < bmp.width &&
            GlyphInkMetrics.distance(bmp.getPixel(labelRight, 1), yellow) <= INK_THRESHOLD
        ) {
            labelRight++
        }

        val band = GlyphInkMetrics.measure(
            bmp, yellow, CORNER_PX until (labelRight - 1), INK_THRESHOLD,
        )

        val fontBoxGap = band.topGap + band.bottomGap - 2 * vPadPx
        val fontBoxRatio = fontBoxGap / band.inkHeight
        assertTrue(
            "Rating pill is too tall: ${band.topGap}px above + ${band.bottomGap}px below a " +
                "${band.inkHeight}px glyph (pill ${bmp.width}x${bmp.height}); minus the " +
                "2×${"%.1f".format(vPadPx)}px vPad that leaves font-box gap/ink=" +
                "${"%.2f".format(fontBoxRatio)}. Expected ≤ $MAX_GAP_RATIO — the text box " +
                "should hug the score, not pad it.",
            fontBoxRatio <= MAX_GAP_RATIO,
        )
    }

    private companion object {
        /** Black-on-yellow is a ~450 channel-sum apart; 150 ignores anti-aliasing. */
        const val INK_THRESHOLD = 150

        /** Safely past the rounded corner (3dp ≈ 8px at density 2.625) that
         *  clips the row's top-left to the dark background; sample/scan past it. */
        const val CORNER_PX = 14

        /** Empty fill (above + below) BEYOND the configured vPad, as a fraction
         *  of the glyph height. The trimmed font box measures ≈0.56 on the
         *  Pixel 6 emulator (the pill text has no descenders, so the font
         *  descent sits empty below the baseline). The threshold sits above that
         *  but below the ≈0.78 an untrimmed box (includeFontPadding on,
         *  LineHeightStyle.Trim.None) measures there, so losing the trim fails. */
        const val MAX_GAP_RATIO = 0.68f
    }
}
