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
 * because at Robolectric's default mdpi (density 1) sub-dp padding rounds to a
 * single pixel and the change is unresolvable. Screen width is forced to the
 * Pixel 9a reference (411 dp) so `RatingBadgeMetrics.scale` is exactly 1.0 and
 * the base dp/sp values render unchanged.
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
     * The real IMDb pill rendered beside a zero-padding reference `Text` carrying
     * the *same* trimmed pill style. Their only height difference is the pill's
     * top+bottom padding, so `pill − reference` isolates `2 × vPad`. At scale 1.0
     * that's `2 × 1 dp = 2 dp`; before the padding was doubled it was `2 × 0.5 dp`,
     * so this fails-before / passes-after on the padding change.
     */
    @Test
    fun imdbPillVerticalPaddingIsOneDpEachSide() {
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
        assertTrue(
            "padded pill must be taller than the bare reference (pill=$pillHeight ref=$referenceHeight)",
            pillHeight > referenceHeight,
        )

        val verticalPadding = pillHeight - referenceHeight
        assertEquals(
            "IMDb pill vertical padding should be 2 dp total (1 dp each side); " +
                "got $verticalPadding dp (pill $pillHeight dp, reference $referenceHeight dp)",
            2.0, verticalPadding.toDouble(), 0.7,
        )
    }

    /**
     * The height fix the padding cut alone couldn't deliver: a bare `Text`
     * reserves the font's full leading (ascent padding + descent), which is what
     * made the pills read tall. `pillTextStyle` drops `includeFontPadding` and
     * trims the line box to the glyph height, so a reference carrying that style
     * is meaningfully shorter than an untrimmed one of the same font. Remove the
     * trim from `pillTextStyle` and this fails. (That the pill itself uses the
     * trimmed style is pinned by the padding test above: pill − trimmed reference
     * comes out as just the padding, which only holds if the pill is trimmed too.)
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
