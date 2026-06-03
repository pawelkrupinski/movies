package pl.kinowo.ui.common

import android.content.res.Configuration
import androidx.compose.foundation.layout.Column
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Ratings

/**
 * Off-device (Robolectric) Compose layout test pinning the rating pill's
 * vertical padding. It renders the real IMDb pill beside a bare reference
 * `Text` of the same font and no padding; the pill label's extra height over
 * the reference is exactly the top+bottom padding (`2 × vPad`). With the screen
 * width forced to the Pixel 9a reference (411 dp → scale 1.0) that should be
 * `2 × 0.5 dp = 1 dp`. Before the padding was halved it was `2 × 1 dp = 2 dp`, so
 * this fails-before / passes-after. The display is forced to `xhdpi` (density 2)
 * because at Robolectric's default mdpi (density 1) both `0.5.dp` and `1.dp` round
 * to a single pixel — the sub-dp change is only resolvable above density 1.
 * NATIVE graphics gives real text metrics.
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class RatingPillPaddingTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun imdbPillVerticalPaddingIsHalfDpEachSide() {
        compose.setContent {
            // Force scale 1.0 (RatingBadgeMetrics anchors at 411 dp) so the
            // measured padding maps to a known base dp value.
            val config = Configuration(LocalConfiguration.current).apply { screenWidthDp = 411 }
            CompositionLocalProvider(LocalConfiguration provides config) {
                MaterialTheme {
                    Column {
                        RatingBadges(Ratings(imdb = 7.4, imdbURL = "https://imdb.com/x"))
                        // Reference: same font as the pill label, zero padding.
                        Text("REF", fontWeight = FontWeight.Bold, fontSize = RatingBadgeMetrics.BaseFontSp.sp)
                    }
                }
            }
        }

        // The pill's `clickable` Row is a merged semantics node, so a (merged-tree)
        // substring match on "IMDb" returns the whole Row — its bounds include the
        // top+bottom padding. The bare reference Text carries only the font's line
        // height, so (pill − reference) is exactly the vertical padding.
        val pill = compose.onNodeWithText("IMDb", substring = true).getUnclippedBoundsInRoot()
        val reference = compose.onNodeWithText("REF").getUnclippedBoundsInRoot()
        val pillHeight = (pill.bottom - pill.top).value
        val referenceHeight = (reference.bottom - reference.top).value

        // Real measurement guard (cf. ShowtimeChipFitTest): a stub renderer
        // would hand back zero-height text and this would fail.
        assertTrue("reference text measured no height — metrics are stubbed", referenceHeight > 0f)
        assertTrue("padded pill must be taller than the bare reference (pill=$pillHeight ref=$referenceHeight)", pillHeight > referenceHeight)

        val verticalPadding = pillHeight - referenceHeight
        assertEquals(
            "IMDb pill vertical padding should be 1 dp total (0.5 dp each side); " +
                "got $verticalPadding dp (pill $pillHeight dp, reference $referenceHeight dp)",
            1.0, verticalPadding.toDouble(), 0.7,
        )
    }
}
