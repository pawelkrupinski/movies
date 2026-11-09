package pl.kinowo.ui.list

import android.graphics.Bitmap
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.asAndroidBitmap
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.test.captureToImage
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.unit.dp
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.filters.SdkSuppress
import dev.chrisbanes.haze.HazeState
import dev.chrisbanes.haze.hazeSource
import kotlin.math.sqrt
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.RecreationProofComposeRule
import pl.kinowo.ui.theme.KinowoTheme

/**
 * On-device (emulator) **visual** guard for the floating search pill's frosted
 * glass: the grid scrolling under the pill must come through blurred, yet still
 * see-through (not a solid fill).
 *
 * The pill floats over a backdrop of 1px black/white vertical bars — the
 * hardest-edged content there is. Along a row inside the pill's top padding
 * (clear of the corners, the border and the glyphs) the bars must be smeared
 * into a near-flat grey: their luminance spread a small fraction of the
 * unblurred bars beside the pill, and their mean still mid-grey (the backdrop
 * shows through rather than the app background). Removing the blur leaves the
 * bars sharp (spread ≈ the reference) and fails; an opaque fill drags the mean
 * to the background's near-black and fails.
 *
 * API 31+ only: below that Haze has no RenderEffect and, as designed, the pill
 * falls back to plain see-through glass with no blur.
 */
@RunWith(AndroidJUnit4::class)
@SdkSuppress(minSdkVersion = 31)
class FloatingSearchBarBlurTest {

    @get:Rule
    val compose = RecreationProofComposeRule()

    @Test
    fun pillBlursTheGridScrollingUnderIt() {
        var dpPx = 0f
        compose.setContent {
            dpPx = with(LocalDensity.current) { 1.dp.toPx() }
            val hazeState = remember { HazeState() }
            KinowoTheme {
                Box(Modifier.fillMaxSize()) {
                    Box(Modifier.fillMaxSize().hazeSource(hazeState)) {
                        Canvas(Modifier.fillMaxSize()) {
                            var x = 0f
                            var white = true
                            while (x < size.width) {
                                drawRect(
                                    if (white) Color.White else Color.Black,
                                    topLeft = Offset(x, 0f),
                                    size = Size(1f, size.height),
                                )
                                white = !white
                                x += 1f
                            }
                        }
                    }
                    FloatingSearchBar(
                        value = "",
                        onValueChange = {},
                        hazeState = hazeState,
                        modifier = Modifier.align(Alignment.BottomCenter),
                    )
                }
            }
        }
        compose.waitForIdle()

        val pill = compose.onNodeWithTag(FloatingSearchBarTag).captureToImage().asAndroidBitmap()
        val root = compose.onRoot().captureToImage().asAndroidBitmap()

        // Inside the 13dp top padding, below the 1dp border; 30dp in from each
        // end clears the 28dp corner radius at that height and the search icon.
        val sampleY = (7 * dpPx).toInt()
        val inset = (30 * dpPx).toInt()
        val blurred = rowLuminance(pill, sampleY, inset until pill.width - inset)
        // The same bars, unblurred: near the top of the screen, far from the pill.
        val sharp = rowLuminance(root, root.height / 4, inset until root.width - inset)

        val sharpSpread = spread(sharp)
        val blurredSpread = spread(blurred)
        val blurredMean = blurred.average()
        assertTrue(
            "Reference bars not sharp (spread $sharpSpread): the backdrop did not render, " +
                "so the blur measurement means nothing.",
            sharpSpread >= MIN_SHARP_SPREAD,
        )
        assertTrue(
            "Pill backdrop not blurred: luminance spread under the pill $blurredSpread vs " +
                "$sharpSpread unblurred (expected ≤ ${MAX_SPREAD_RATIO}×).",
            blurredSpread <= sharpSpread * MAX_SPREAD_RATIO,
        )
        assertTrue(
            "Pill not see-through: mean luminance under it ${"%.1f".format(blurredMean)} " +
                "(the bars average ≈128; expected $MIN_MEAN..$MAX_MEAN).",
            blurredMean in MIN_MEAN..MAX_MEAN,
        )
    }

    private fun rowLuminance(bmp: Bitmap, y: Int, xs: IntRange): List<Double> = xs.map { x ->
        val p = bmp.getPixel(x, y)
        0.299 * ((p shr 16) and 0xFF) + 0.587 * ((p shr 8) and 0xFF) + 0.114 * (p and 0xFF)
    }

    /** Standard deviation of the samples. */
    private fun spread(samples: List<Double>): Double {
        val mean = samples.average()
        return sqrt(samples.sumOf { (it - mean) * (it - mean) } / samples.size)
    }

    private companion object {
        /** 1px black/white bars measure ≈127 (half of 255). */
        const val MIN_SHARP_SPREAD = 100.0

        /** Blurred bars must keep at most this fraction of the sharp spread. */
        const val MAX_SPREAD_RATIO = 0.25

        /** Mid-grey: the bars show through the glass. */
        const val MIN_MEAN = 70.0
        const val MAX_MEAN = 190.0
    }
}
