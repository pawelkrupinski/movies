package pl.kinowo.ui.common

import androidx.compose.ui.graphics.Color
import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.ui.theme.CinemaBlue
import pl.kinowo.ui.theme.ShowtimeChipBackground
import pl.kinowo.ui.theme.ShowtimeChipBackgroundPressed

/**
 * Pins the showtime chip's palette to the web `.badge-time` pill, which
 * `ShowtimeChip` (in Showings.kt) renders from: `#3a3a6e` fill, `#5a5a9e`
 * pressed/hover fill, and `#aad4ff` (CinemaBlue) time text — the format tag is
 * that same blue at 0.7 alpha. The Android twin of iOS
 * `ShowtimePillMetricsTests.testPillColoursMatchTheWebBadgePalette`.
 *
 * Why a palette assertion and not a rendered-pixel one: off-device Robolectric
 * can't drive `captureToImage` (its `forceRedraw` waits on a draw callback the
 * paused looper never fires), and the project has no emulator layer in CI. So
 * we pin the colour source the composable reads; the one-line wiring in
 * `ShowtimeChip` is verified by the diff.
 */
class ShowtimeChipColorTest {

    @Test
    fun chipPaletteMatchesTheWebBadgeTimePill() {
        assertEquals("chip fill must be the web #3a3a6e", Color(0xFF3A3A6E), ShowtimeChipBackground)
        assertEquals("pressed fill must be the web #5a5a9e hover", Color(0xFF5A5A9E), ShowtimeChipBackgroundPressed)
        assertEquals("time text must be the web #aad4ff", Color(0xFFAAD4FF), CinemaBlue)
    }
}
