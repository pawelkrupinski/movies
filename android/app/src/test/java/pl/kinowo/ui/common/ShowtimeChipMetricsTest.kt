package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Pure unit test for the showtime-chip viewport scale. The reference is the
 * 360 dp floor (scale 1.0 — the baseline renders the dialled values), it only
 * grows for wider screens, never shrinks below the baseline, and clamps at 1.4
 * so tablets don't balloon the chips. The "still fits two-per-row after scaling"
 * guarantee is pinned separately by `ShowtimeChipFitTest`.
 */
class ShowtimeChipMetricsTest {

    @Test
    fun referenceWidthIsScaleOne() {
        assertEquals(1.0f, ShowtimeChipMetrics.scale(360), 0.0001f)
    }

    @Test
    fun growsLinearlyWithWidth() {
        assertEquals(411f / 360f, ShowtimeChipMetrics.scale(411), 0.0001f)
        assertTrue(ShowtimeChipMetrics.scale(430) > ShowtimeChipMetrics.scale(393))
    }

    @Test
    fun neverShrinksBelowTheBaseline() {
        // Below the 360 dp floor (unsupported) the factor clamps to 1.0 — we
        // never render the chips smaller than the dialled baseline.
        assertEquals(1.0f, ShowtimeChipMetrics.scale(320), 0.0001f)
    }

    @Test
    fun clampsWideScreens() {
        // 504/360 = 1.4 exactly — the clamp point; anything wider stays at 1.4.
        assertEquals(1.4f, ShowtimeChipMetrics.scale(504), 0.0001f)
        assertEquals(1.4f, ShowtimeChipMetrics.scale(800), 0.0001f)
    }
}
