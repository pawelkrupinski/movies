package pl.kinowo.ui.common

import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Pure-math tests for the room tooltip's Popup placement. The bubble rides in a
 * Popup (so popping it never resizes the pill); these pin where that Popup lands
 * relative to the pill it's anchored to — centred, lifted by a fixed gap, and
 * clamped on-screen.
 */
class RoomTooltipPositionTest {

    private val gap = 40 // px

    @Test
    fun centresHorizontallyOverThePill() {
        val anchor = IntRect(left = 100, top = 200, right = 140, bottom = 230) // 40 wide
        val bubble = IntSize(width = 80, height = 50)
        val offset = roomTooltipPopupOffset(anchor, bubble, gapPx = gap, windowWidth = 1080)
        // Bubble centre lines up with the pill centre (120).
        assertEquals(120, offset.x + bubble.width / 2)
    }

    @Test
    fun sitsExactlyGapAboveThePillTop() {
        val anchor = IntRect(left = 100, top = 200, right = 140, bottom = 230)
        val bubble = IntSize(width = 80, height = 50)
        val offset = roomTooltipPopupOffset(anchor, bubble, gapPx = gap, windowWidth = 1080)
        // Bubble's bottom edge is `gap` px above the pill's top.
        assertEquals(anchor.top - gap, offset.y + bubble.height)
    }

    @Test
    fun aRightEdgePillDoesNotPushTheBubbleOffScreen() {
        val windowWidth = 1080
        val anchor = IntRect(left = 1000, top = 300, right = 1040, bottom = 330) // far right
        val bubble = IntSize(width = 200, height = 50)
        val offset = roomTooltipPopupOffset(anchor, bubble, gapPx = gap, windowWidth = windowWidth)
        assertTrue("bubble left must stay on-screen", offset.x >= 0)
        assertTrue(
            "bubble right (${offset.x + bubble.width}) must stay within $windowWidth",
            offset.x + bubble.width <= windowWidth,
        )
    }

    @Test
    fun aLeftEdgePillDoesNotPushTheBubbleOffScreen() {
        val anchor = IntRect(left = 0, top = 300, right = 40, bottom = 330)
        val bubble = IntSize(width = 200, height = 50)
        val offset = roomTooltipPopupOffset(anchor, bubble, gapPx = gap, windowWidth = 1080)
        assertEquals("bubble must pin to the left edge", 0, offset.x)
    }
}
