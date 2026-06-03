package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.ui.Modifier
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
 * time and smaller format tag are vertically centred against each other (the
 * same `.center` alignment iOS's HStack uses). The bug it guards: the chip's
 * `Row` had no `verticalAlignment`, so Compose defaulted to `Alignment.Top` and
 * the format text rode the top of the larger time. NATIVE graphics gives real
 * text metrics so the two boxes have genuinely different heights.
 *
 * Runs on the JVM via `./gradlew app:testDebugUnitTest` — no emulator.
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
    fun formatTagIsVerticallyCentredWithTheTime() {
        compose.setContent {
            KinowoTheme {
                Box(Modifier.width(220.dp)) {
                    Showings(film = film(), showCinemaHeaders = false)
                }
            }
        }

        val time = compose.onNodeWithText("12:55", useUnmergedTree = true).getUnclippedBoundsInRoot()
        val format = compose.onNodeWithText("2D DUB", useUnmergedTree = true).getUnclippedBoundsInRoot()

        val timeHeight = (time.bottom - time.top).value
        val formatHeight = (format.bottom - format.top).value

        // The test only means something if the two boxes differ in height — that's
        // the whole reason alignment is visible. (Also guards stubbed metrics.)
        assertTrue(
            "format box ($formatHeight dp) should be shorter than the time box ($timeHeight dp)",
            timeHeight - formatHeight > 1f,
        )

        // Centred ⇒ the box mid-lines coincide. Top-aligned (the bug) would put
        // the format's centre well above the time's by half the height gap.
        val timeCentre = (time.top.value + time.bottom.value) / 2f
        val formatCentre = (format.top.value + format.bottom.value) / 2f
        assertEquals(
            "format tag should be vertically centred with the time; centres " +
                "time=$timeCentre format=$formatCentre (Δ=${timeCentre - formatCentre} dp), " +
                "boxes time=${time.top.value}..${time.bottom.value} " +
                "format=${format.top.value}..${format.bottom.value}. Top-aligned regression?",
            timeCentre.toDouble(), formatCentre.toDouble(), 1.0,
        )
    }
}
