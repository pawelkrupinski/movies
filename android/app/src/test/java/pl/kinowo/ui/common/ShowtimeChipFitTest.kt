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
 * Off-device (Robolectric) Compose layout test for the showtime chips — the
 * Android twin of iOS `ShowtimePillMetricsTests`. It renders the real
 * `Showings` composable constrained to one card's showings column and checks
 * that two canonical chips ("12:55 2D DUB" + "22:55 3D NAP") share a single
 * row across every common phone width.
 *
 * Two chips per row is a HARD, inviolable constraint — fonts are sized up to the
 * largest that keeps it, never past it. If this test fails, a font/padding/gap
 * change broke two-per-row: shrink it back or reclaim width (trim the chip inset
 * or inter-chip gap), don't relax the assertion.
 *
 * Each width is rendered under its OWN `LocalConfiguration.screenWidthDp` (via
 * [AtWidth]) so [ShowtimeChipMetrics] applies that width's scale — this verifies
 * that the width-scaled chips STILL fit two-per-row at every width, including the
 * wide tablet-portrait widths past the 1.4 clamp. Runs on the JVM via
 * `./gradlew testDebugUnitTest`, so no emulator is needed and it guards CI.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE) // real text metrics, so wrapping is real
class ShowtimeChipFitTest {

    @get:Rule
    val compose = createComposeRule()

    /** Logical dp widths, narrowest first. 360 dp (Galaxy S, many budget devices)
     *  is the binding case and the chip scale's reference. The 504 dp case is the
     *  exact width where the 1.4 scale clamp kicks in; 600 dp is a portrait tablet
     *  (still `GridCells.Fixed(2)`) past the clamp — both must still fit two-up. */
    private val phoneWidths = listOf(
        "Galaxy S (360dp)" to 360,
        "Pixel 8 (393dp)" to 393,
        "Pixel 5 (411dp)" to 411,
        "large (430dp)" to 430,
        "scale-clamp (504dp)" to 504,
        "tablet portrait (600dp)" to 600,
    )

    /** Render [content] as if the device's portrait width were [screen] dp, so
     *  [ShowtimeChipMetrics.scale] picks up that width. */
    @Composable
    private fun AtWidth(screen: Int, content: @Composable () -> Unit) {
        val cfg = Configuration(LocalConfiguration.current).apply { screenWidthDp = screen }
        CompositionLocalProvider(LocalConfiguration provides cfg, content = content)
    }

    /** Showings-column width inside one card on the portrait two-column grid:
     *  `ListScreen`'s grid is `GridCells.Fixed(2)` with 12 dp content padding
     *  each side + 12 dp between the columns (36 dp total), and `FilmCard`'s
     *  inner `Column` padding takes 24 dp off. Mirrors iOS
     *  `ShowtimePillMetrics.cardShowingsWidth`. */
    private fun cardContentDp(screenDp: Int) = (screenDp - 36) / 2 - 24

    private fun twoShowtimeFilm(t1: String, t2: String) = Film(
        title = "T",
        showings = listOf(
            DayShowings(
                date = "2026-06-03",
                label = "środa",
                cinemas = listOf(
                    CinemaShowings(
                        cinema = "Kino",
                        showtimes = listOf(
                            Showtime(time = t1, format = "2D DUB"),
                            Showtime(time = t2, format = "3D NAP"),
                        ),
                    ),
                ),
            ),
        ),
    )

    @Test
    fun twoCanonicalChipsShareOneRowAtEveryPhoneWidth() {
        compose.setContent {
            MaterialTheme {
                Column {
                    phoneWidths.forEachIndexed { i, (_, screen) ->
                        AtWidth(screen) {
                            Box(Modifier.width(cardContentDp(screen).dp)) {
                                Showings(
                                    film = twoShowtimeFilm("10:0$i", "22:0$i"),
                                    showCinemaHeaders = false,
                                )
                            }
                        }
                    }
                }
            }
        }

        phoneWidths.forEachIndexed { i, (name, _) ->
            val a = compose.onNodeWithText("10:0$i", useUnmergedTree = true)
                .fetchSemanticsNode().boundsInRoot
            val b = compose.onNodeWithText("22:0$i", useUnmergedTree = true)
                .fetchSemanticsNode().boundsInRoot
            assertEquals(
                "two showtime chips wrapped onto separate rows on $name",
                a.top.toDouble(), b.top.toDouble(), 1.0,
            )
            assertTrue(
                "second chip should sit to the right of the first on $name " +
                    "(left a=${a.left}, b=${b.left})",
                b.left > a.left,
            )
        }
    }

    @Test
    fun chipsWrapWhenCardFarTooNarrow_provesMeasurementIsReal() {
        // A 50 dp card can't hold one chip beside another. If Robolectric were
        // handing back stub zero-width text, the chips would never wrap and this
        // would fail — so it's the guard that the positive test isn't passing
        // vacuously on phantom measurements.
        compose.setContent {
            MaterialTheme {
                AtWidth(360) {
                    Box(Modifier.width(50.dp)) {
                        Showings(
                            film = twoShowtimeFilm("10:00", "22:00"),
                            showCinemaHeaders = false,
                        )
                    }
                }
            }
        }
        val a = compose.onNodeWithText("10:00", useUnmergedTree = true)
            .fetchSemanticsNode().boundsInRoot
        val b = compose.onNodeWithText("22:00", useUnmergedTree = true)
            .fetchSemanticsNode().boundsInRoot
        assertTrue(
            "expected the two chips to wrap onto separate rows in a 50 dp card",
            b.top > a.top,
        )
    }
}
