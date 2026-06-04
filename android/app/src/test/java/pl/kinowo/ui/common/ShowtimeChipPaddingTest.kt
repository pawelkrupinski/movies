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
 * Off-device (Robolectric) Compose layout tests pinning the showtime chip's
 * vertical size — the sibling of [RatingPillPaddingTest] for the blue time chips.
 * NATIVE graphics gives real text metrics; `xhdpi` (density 2) so sub-dp
 * differences resolve.
 *
 * They pin the two height levers in CI: the 4 dp vertical inset over the trimmed
 * time, and (transitively) that the time uses the `includeFontPadding`-off
 * `pillTextStyle` — drop the trim and the chip grows past the 8 dp inset the
 * first test allows.
 *
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
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

    /**
     * The chip's height is the trimmed time font box plus a 4.5 dp inset top and
     * bottom. Rendered beside a zero-padding reference `Text` in the chip's time
     * style, the chip's extra height over the reference is exactly `2 × 4.5 dp`.
     */
    @Test
    fun chipVerticalInsetIsFourPointFiveDpEachSide() {
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.width(200.dp)) {
                        Showings(film = oneShowtimeFilm(), showCinemaHeaders = false)
                    }
                    // Reference: the chip's own trimmed time style, zero padding.
                    Text("TRIM", style = pillTextStyle(11.sp, FontWeight.SemiBold))
                }
            }
        }

        val chipHeight = heightOfTag(ShowtimeChipTestTag)
        val referenceHeight = heightOfText("TRIM")

        assertTrue("reference text measured no height — metrics are stubbed", referenceHeight > 0f)
        val inset = chipHeight - referenceHeight
        assertEquals(
            "showtime chip inset should be 9 dp total (4.5 dp each side); got $inset dp " +
                "(chip $chipHeight dp, reference $referenceHeight dp).",
            9.0, inset.toDouble(), 1.0,
        )
    }

    /**
     * The chip's time uses `pillTextStyle`, so its `includeFontPadding`-off box is
     * meaningfully shorter than an untrimmed `Text` of the same font — the leading
     * that made the chip read tall. Drop `includeFontPadding = false` and this
     * fails. (That the chip uses the trimmed style is pinned by the no-inset test
     * above: the chip matches the trimmed reference height.)
     */
    @Test
    fun chipTimeTrimsTheFontLeading() {
        compose.setContent {
            KinowoTheme {
                Column {
                    Text("TRIM", style = pillTextStyle(11.sp, FontWeight.SemiBold))
                    Text("FULL", fontSize = 11.sp, fontWeight = FontWeight.SemiBold)
                }
            }
        }

        val trimmedHeight = heightOfText("TRIM")
        val untrimmedHeight = heightOfText("FULL")

        assertTrue("untrimmed reference measured no height — metrics are stubbed", untrimmedHeight > 0f)
        assertTrue(
            "trimming must claw back real font leading: untrimmed=$untrimmedHeight should exceed trimmed=$trimmedHeight",
            untrimmedHeight - trimmedHeight > 1f,
        )
    }

    private fun heightOfTag(tag: String): Float =
        compose.onNodeWithTag(tag).getUnclippedBoundsInRoot().let { (it.bottom - it.top).value }

    private fun heightOfText(text: String): Float =
        compose.onNodeWithText(text).getUnclippedBoundsInRoot().let { (it.bottom - it.top).value }
}
