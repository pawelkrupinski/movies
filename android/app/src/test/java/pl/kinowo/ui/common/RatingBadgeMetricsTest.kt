package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class RatingBadgeMetricsTest {

    @Test
    fun pixel9aWidthIsTheUnscaledBaseline() {
        // The pill sizes (11sp font, 6/2dp padding) were tuned on the Pixel 9a's
        // ~411dp portrait width, so that width must map to scale 1.0 exactly.
        assertEquals(1.0f, RatingBadgeMetrics.scale(411), 0.01f)
    }

    @Test
    fun narrowerPhonesShrinkThePills() {
        assertTrue(RatingBadgeMetrics.scale(360) < 1.0f)
        assertTrue(RatingBadgeMetrics.scale(320) < RatingBadgeMetrics.scale(360))
    }

    @Test
    fun widerScreensGrowThePills() {
        assertTrue(RatingBadgeMetrics.scale(480) > 1.0f)
        assertTrue(RatingBadgeMetrics.scale(600) > RatingBadgeMetrics.scale(480))
    }

    @Test
    fun extremesAreClampedSoPillsStayLegibleAndDontBalloon() {
        // Tiny/edge configs don't shrink to illegibility; tablets/foldables don't
        // balloon the row.
        assertEquals(RatingBadgeMetrics.MinScale, RatingBadgeMetrics.scale(100), 0.001f)
        assertEquals(RatingBadgeMetrics.MaxScale, RatingBadgeMetrics.scale(2000), 0.001f)
    }
}
