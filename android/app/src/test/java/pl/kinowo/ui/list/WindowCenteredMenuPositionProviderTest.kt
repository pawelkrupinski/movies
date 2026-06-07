package pl.kinowo.ui.list

import androidx.compose.ui.unit.IntRect
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.LayoutDirection
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Pins the filter-menu positioning rule: the popup opens horizontally centred
 * in the window (not stuck to the anchor's left edge) and drops below the
 * anchor, flipping above it only when there's no room below.
 */
class WindowCenteredMenuPositionProviderTest {

    private val window = IntSize(width = 1080, height = 1920)
    private val gap = 4
    private val provider = WindowCenteredMenuPositionProvider(verticalGapPx = gap)

    private fun positionOf(
        anchor: IntRect,
        popup: IntSize,
    ) = provider.calculatePosition(anchor, window, LayoutDirection.Ltr, popup)

    @Test
    fun `x centres the popup in the window regardless of a left-edge anchor`() {
        // Full-width anchor near the left margin (the Miasto picker).
        val anchor = IntRect(left = 48, top = 800, right = 1032, bottom = 920)
        val popup = IntSize(width = 400, height = 600)

        val pos = positionOf(anchor, popup)

        assertEquals((1080 - 400) / 2, pos.x) // 340 — centred, not the anchor's left (48)
    }

    @Test
    fun `x centres a popup under a half-width left anchor`() {
        // The Od-godziny hour picker: weight(1f) → left half of the row.
        val anchor = IntRect(left = 48, top = 800, right = 520, bottom = 920)
        val popup = IntSize(width = 200, height = 400)

        val pos = positionOf(anchor, popup)

        assertEquals((1080 - 200) / 2, pos.x) // 440 — mid-window, not under the anchor
    }

    @Test
    fun `drops below the anchor when there is room`() {
        val anchor = IntRect(left = 48, top = 800, right = 1032, bottom = 920)
        val popup = IntSize(width = 400, height = 300)

        val pos = positionOf(anchor, popup)

        assertEquals(920 + gap, pos.y)
    }

    @Test
    fun `flips above the anchor when the menu would overflow the bottom`() {
        // Anchor low on screen, tall menu that wouldn't fit below.
        val anchor = IntRect(left = 48, top = 1500, right = 1032, bottom = 1620)
        val popup = IntSize(width = 400, height = 700)

        val pos = positionOf(anchor, popup)

        assertEquals(1500 - gap - 700, pos.y)
    }

    @Test
    fun `clamps x to zero when the popup is wider than the window`() {
        val anchor = IntRect(left = 0, top = 100, right = 1080, bottom = 220)
        val popup = IntSize(width = 1200, height = 300)

        val pos = positionOf(anchor, popup)

        assertTrue(pos.x >= 0)
    }
}
