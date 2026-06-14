package pl.kinowo.ui.common

import android.content.res.Configuration
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.test.down
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithText
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
 * Off-device (Robolectric) Compose test that the card's text elements scale with
 * the viewport width, off the 360 dp baseline (scale 1.0) up to the 1.4 clamp —
 * the same `ShowtimeChipMetrics.scale(layoutWidthDp())` factor that drives the
 * showtime chips.
 *
 * Each previously-fixed element is rendered at a 504 dp width (s = 1.4) next to a
 * FIXED-size reference holding the SAME string at the element's baseline size,
 * and the scaled copy must be measurably WIDER — a fixed-`.sp` element matches its
 * reference and fails. Width (not height) is the probe: `KinowoTheme` applies a
 * fixed line-height, so a sub-24sp text reports the line-box height regardless of
 * font size; its glyph ADVANCE still grows with the font, so width tracks scale.
 *
 * The room tooltip (24sp → 33.6sp at s=1.4, above the line box) is the one element
 * whose height does grow, so it's checked by height against a 24sp reference.
 *
 * Not independently asserted here: the card TITLE (its `fillMaxWidth` pins the node
 * width and its <24sp font is line-box-clamped in height, so neither bound reflects
 * its font — it uses the identical `(14 * s).sp` wiring proven on the day label),
 * the "+N seansów" overflow (same `(11 * s).sp` as the day label) and the dp-only
 * chrome (card corner, hide button, tooltip padding/border) which carry no glyph to
 * measure. NATIVE graphics for real metrics; `./gradlew app:testDebugUnitTest`.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CardScalingTest {

    @get:Rule
    val compose = createComposeRule()

    /** The 504 dp width: `ShowtimeChipMetrics.scale(504) == 1.4`, the clamp. */
    private val wideWidth = 504
    private val wideScale = ShowtimeChipMetrics.scale(wideWidth) // 1.4

    @Composable
    private fun AtWidth(screen: Int, content: @Composable () -> Unit) {
        val config = Configuration(LocalConfiguration.current).apply {
            screenWidthDp = screen
            smallestScreenWidthDp = screen
        }
        CompositionLocalProvider(LocalConfiguration provides config, content = content)
    }

    private fun labelFilm(day: String, cinema: String) = Film(
        title = "T",
        releaseYear = 2020,
        runtimeMinutes = 130,
        showings = listOf(
            DayShowings(
                date = "2026-06-08",
                label = day,
                cinemas = listOf(
                    CinemaShowings(cinema = cinema, showtimes = listOf(Showtime(time = "12:55", format = "2D"))),
                ),
            ),
        ),
    )

    private fun roomFilm() = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "12:55", format = "2D", room = "Sala 8"))),
                ),
            ),
        ),
    )

    /** Asserts exactly two nodes carry [text] — the scaled element and its fixed
     *  reference — and the wider (scaled) one beats the reference by a clear margin. */
    private fun assertScaledWider(name: String, text: String) {
        val nodes = compose.onAllNodesWithText(text, useUnmergedTree = true)
        val count = nodes.fetchSemanticsNodes().size
        assertEquals("$name: expected the scaled element + its reference for '$text'", 2, count)
        val widths = (0 until count).map {
            nodes[it].getUnclippedBoundsInRoot().let { b -> (b.right - b.left).value }
        }
        val scaled = widths.maxOrNull()!!
        val reference = widths.minOrNull()!!
        assertTrue("$name reference measured no width — metrics are stubbed", reference > 0f)
        assertTrue(
            "$name at ${wideWidth}dp (s=$wideScale) must render wider than its fixed " +
                "baseline reference: scaled=$scaled reference=$reference",
            scaled > reference + 2f,
        )
    }

    /** Day label (11sp), cinema label (12sp) and the meta pills (11sp runtime /
     *  13sp year) all grow with the viewport, rendered next to same-string fixed
     *  references. */
    @Test
    fun labelsAndMetaScaleWithScreenWidth() {
        compose.setContent {
            KinowoTheme {
                AtWidth(wideWidth) {
                    Column {
                        // Fixed-size references (NOT scaled), same strings as below.
                        Text("WIDEDAY", fontSize = 11.sp, fontWeight = FontWeight.SemiBold)
                        Text("WIDECINEMA", fontSize = 12.sp, fontWeight = FontWeight.Medium)
                        Text("2020", fontSize = 13.sp, fontWeight = FontWeight.Medium)
                        Text("2h 10min", fontSize = 11.sp, fontWeight = FontWeight.Medium)
                        Box(Modifier.width(220.dp)) {
                            Showings(film = labelFilm("WIDEDAY", "WIDECINEMA"), showCinemaHeaders = true)
                        }
                        MetaPills(runtimeMinutes = 130, releaseYear = 2020, scale = wideScale)
                    }
                }
            }
        }

        assertScaledWider("day label", "WIDEDAY")
        assertScaledWider("cinema label", "WIDECINEMA")
        assertScaledWider("meta year", "2020")
        assertScaledWider("meta runtime pill", "2h 10min")
    }

    /** The room tooltip (24sp → 33.6sp at s=1.4) grows past the line box, so its
     *  height does track the font: pressed open, it must out-tall a fixed 24sp ref. */
    @Test
    fun roomTooltipScalesWithScreenWidth() {
        compose.mainClock.autoAdvance = false
        compose.setContent {
            KinowoTheme {
                AtWidth(wideWidth) {
                    Column {
                        Box(Modifier.width(220.dp)) {
                            Showings(film = roomFilm(), showCinemaHeaders = false)
                        }
                        Text("REF24", fontSize = 24.sp, fontWeight = FontWeight.SemiBold)
                    }
                }
            }
        }
        compose.onNodeWithTag(ShowtimeChipTestTag).performTouchInput { down(center) }
        compose.mainClock.advanceTimeBy(1000L)
        compose.mainClock.advanceTimeByFrame()

        fun heightOf(text: String) = compose.onNodeWithText(text, useUnmergedTree = true)
            .getUnclippedBoundsInRoot().let { (it.bottom - it.top).value }
        val tooltip = heightOf("Sala 8")
        val ref = heightOf("REF24")
        assertTrue("reference measured no height — metrics are stubbed", ref > 0f)
        assertTrue(
            "room tooltip at ${wideWidth}dp (s=$wideScale) must out-tall a fixed 24sp " +
                "reference: tooltip=$tooltip ref=$ref",
            tooltip > ref + 1f,
        )
    }
}
