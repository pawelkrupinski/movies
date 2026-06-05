package pl.kinowo.ui.common

import android.content.res.Configuration
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.width
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalConfiguration
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

/**
 * Off-device (Robolectric) check that a showtime chip is sized identically in
 * landscape and portrait on the same phone. The chip sizing is keyed off the
 * device's PORTRAIT width (`layoutWidthDp` → `smallestScreenWidthDp`), which is
 * invariant across rotation, so the rendered chip must measure the same in both
 * orientations.
 *
 * Before that change the call sites fed the live `screenWidthDp`, which grows in
 * landscape (here 800 dp), pushing `ShowtimeChipMetrics.scale` to its 1.4 clamp
 * and rendering the chip ~40 % larger than in portrait — this test fails in that
 * world and passes once landscape locks to the portrait width.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE) // real text metrics, so heights are real
class ShowtimeChipOrientationTest {

    @get:Rule
    val compose = createComposeRule()

    /** A phone whose portrait width is 360 dp, rendered in the given orientation.
     *  Landscape widens `screenWidthDp` to 800 (the long edge) but keeps
     *  `smallestScreenWidthDp` at 360 — exactly what a real device reports. */
    @Composable
    private fun AsPhone(landscape: Boolean, content: @Composable () -> Unit) {
        val cfg = Configuration(LocalConfiguration.current).apply {
            smallestScreenWidthDp = 360
            if (landscape) {
                orientation = Configuration.ORIENTATION_LANDSCAPE
                screenWidthDp = 800
                screenHeightDp = 360
            } else {
                orientation = Configuration.ORIENTATION_PORTRAIT
                screenWidthDp = 360
                screenHeightDp = 800
            }
        }
        CompositionLocalProvider(LocalConfiguration provides cfg, content = content)
    }

    private fun filmAt(time: String) = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = time, format = "2D"))),
                ),
            ),
        ),
    )

    @Test
    fun chipSameSizeInLandscapeAndPortrait() {
        compose.setContent {
            MaterialTheme {
                Column {
                    AsPhone(landscape = false) {
                        Box(Modifier.width(200.dp)) { Showings(film = filmAt("11:00"), showCinemaHeaders = false) }
                    }
                    AsPhone(landscape = true) {
                        Box(Modifier.width(200.dp)) { Showings(film = filmAt("22:00"), showCinemaHeaders = false) }
                    }
                }
            }
        }

        val portrait = compose.onNodeWithText("11:00", useUnmergedTree = true)
            .fetchSemanticsNode().boundsInRoot
        val landscape = compose.onNodeWithText("22:00", useUnmergedTree = true)
            .fetchSemanticsNode().boundsInRoot

        // Guard against phantom zero-size measurements (NATIVE mode should give
        // real metrics, but make the equality meaningful).
        assertTrue("portrait chip text has no height", portrait.height > 1f)
        assertEquals(
            "landscape chip rendered a different height than portrait — it is not " +
                "locked to the portrait width (portrait=${portrait.height}, landscape=${landscape.height})",
            portrait.height.toDouble(), landscape.height.toDouble(), 0.5,
        )
        assertEquals(
            "landscape chip rendered a different width than portrait",
            portrait.width.toDouble(), landscape.width.toDouble(), 0.5,
        )
    }
}
