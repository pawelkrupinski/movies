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
 * Off-device (Robolectric) Compose layout tests pinning the rating pill's
 * vertical size. NATIVE graphics gives real text metrics, so measured heights
 * reflect the actual font line box. The display is forced to `xhdpi` (density 2)
 * so sub-dp differences resolve to more than one pixel. Screen width is forced to
 * the Pixel 9a reference (411 dp) so `RatingBadgeMetrics.scale` is exactly 1.0 and
 * the base dp/sp values render unchanged.
 *
 * These pin the two height levers cheaply in CI: that the pill adds no padding,
 * and that `pillTextStyle` drops the `includeFontPadding` leading. How the result
 * actually *looks* (the residual font-box whitespace) is the emulator-side
 * `RatingPillVisualPaddingTest`.
 *
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class RatingPillPaddingTest {

    @get:Rule
    val compose = createComposeRule()

    /**
     * The pill carries **no** extra vertical padding: its height is just the
     * trimmed font box. Rendered beside a zero-padding reference `Text` in the
     * same pill style, the pill's height should equal the reference (within a
     * sub-dp rounding margin). Re-add any `vPad` and the pill grows past the
     * reference, failing this. (The remaining visible whitespace is the font's
     * own descent box, not padding — that's the emulator-side
     * `RatingPillVisualPaddingTest`'s concern.)
     */
    @Test
    fun imdbPillAddsNoVerticalPaddingBeyondTheFontBox() {
        compose.setContent {
            ReferenceWidth {
                MaterialTheme {
                    Column {
                        RatingBadges(Ratings(imdb = 7.4, imdbURL = "https://imdb.com/x"))
                        // Reference: the pill's own trimmed style, zero padding.
                        Text("TRIM", style = pillTextStyle(RatingBadgeMetrics.BaseFontSp.sp, FontWeight.Bold))
                    }
                }
            }
        }

        val pillHeight = mergedHeightOf("IMDb")
        val referenceHeight = heightOf("TRIM")

        assertTrue("reference text measured no height — metrics are stubbed", referenceHeight > 0f)
        assertEquals(
            "IMDb pill should add no vertical padding over the bare trimmed text; " +
                "got pill $pillHeight dp vs reference $referenceHeight dp " +
                "(${pillHeight - referenceHeight} dp of padding).",
            referenceHeight.toDouble(), pillHeight.toDouble(), 0.7,
        )
    }

    /**
     * The height fix the padding cut alone couldn't deliver: a bare `Text` adds
     * `includeFontPadding` leading on top of the font box. `pillTextStyle` turns
     * it off, so a reference carrying that style is meaningfully shorter than an
     * untrimmed one of the same font. Remove `includeFontPadding = false` from
     * `pillTextStyle` and this fails. (That the pill itself uses the trimmed style
     * is pinned by the no-padding test above: the pill matches the trimmed
     * reference height, which only holds if the pill is trimmed too.)
     */
    @Test
    fun pillStyleTrimsTheFontLeading() {
        compose.setContent {
            ReferenceWidth {
                MaterialTheme {
                    Column {
                        Text("TRIM", style = pillTextStyle(RatingBadgeMetrics.BaseFontSp.sp, FontWeight.Bold))
                        // Same font, but the platform's default full line box (untrimmed).
                        Text("FULL", fontWeight = FontWeight.Bold, fontSize = RatingBadgeMetrics.BaseFontSp.sp)
                    }
                }
            }
        }

        val trimmedHeight = heightOf("TRIM")
        val untrimmedHeight = heightOf("FULL")

        assertTrue("untrimmed reference measured no height — metrics are stubbed", untrimmedHeight > 0f)
        assertTrue(
            "trimming must claw back real font leading: untrimmed=$untrimmedHeight should exceed trimmed=$trimmedHeight",
            untrimmedHeight - trimmedHeight > 1f,
        )
    }

    @androidx.compose.runtime.Composable
    private fun ReferenceWidth(content: @androidx.compose.runtime.Composable () -> Unit) {
        val config = Configuration(LocalConfiguration.current).apply { screenWidthDp = 411 }
        CompositionLocalProvider(LocalConfiguration provides config, content = content)
    }

    /** A pill's `clickable` Row is a merged semantics node; a substring match on
     *  its label returns the whole Row, whose bounds include the padding. */
    private fun mergedHeightOf(label: String): Float =
        compose.onNodeWithText(label, substring = true).getUnclippedBoundsInRoot()
            .let { (it.bottom - it.top).value }

    private fun heightOf(text: String): Float =
        compose.onNodeWithText(text).getUnclippedBoundsInRoot()
            .let { (it.bottom - it.top).value }
}
