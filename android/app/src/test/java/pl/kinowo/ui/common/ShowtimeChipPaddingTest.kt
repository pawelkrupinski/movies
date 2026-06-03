package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.material3.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
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
 * Off-device (Robolectric) Compose layout test pinning the showtime chip's
 * vertical inset. It renders the real chip beside a bare reference `Text` at the
 * chip's time font (9sp SemiBold) with no padding; the chip's extra height over
 * the reference is exactly the top+bottom inset. That should be `2 × 1 dp = 2 dp`
 * after halving the inset to ~.1em, where it was `2 × 2 dp = 4 dp` — so this
 * fails-before / passes-after. NATIVE graphics gives real text metrics. Runs on
 * the JVM via `./gradlew app:testDebugUnitTest`.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class ShowtimeChipPaddingTest {

    @get:Rule
    val compose = createComposeRule()

    private fun oneShowtimeFilm() = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        showtimes = listOf(Showtime(time = "12:55", format = "2D")),
                    ),
                ),
            ),
        ),
    )

    @Test
    fun showtimeChipVerticalInsetIsOneDpEachSide() {
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.width(200.dp)) {
                        Showings(film = oneShowtimeFilm(), showCinemaHeaders = false)
                    }
                    // Reference: the chip's time font, no padding. A distinct
                    // string (line height is font-size driven, not glyph-specific)
                    // so it doesn't collide with the chip's own "12:55".
                    Text("REF", fontSize = 9.sp, fontWeight = FontWeight.SemiBold)
                }
            }
        }

        val chip = compose.onNodeWithTag(ShowtimeChipTestTag).getUnclippedBoundsInRoot()
        val reference = compose.onNodeWithText("REF").getUnclippedBoundsInRoot()
        val chipHeight = (chip.bottom - chip.top).value
        val referenceHeight = (reference.bottom - reference.top).value

        // Real measurement guard (cf. ShowtimeChipFitTest): a stub renderer
        // would hand back zero-height text and this would fail.
        assertTrue("reference text measured no height — metrics are stubbed", referenceHeight > 0f)
        assertTrue("padded chip must be taller than the bare reference (chip=$chipHeight ref=$referenceHeight)", chipHeight > referenceHeight)

        val verticalPadding = chipHeight - referenceHeight
        assertEquals(
            "chip vertical inset should be 2 dp total (1 dp each side); " +
                "got $verticalPadding dp (chip $chipHeight dp, reference $referenceHeight dp)",
            2.0, verticalPadding.toDouble(), 1.0,
        )
    }
}
