package pl.kinowo.ui.list

import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.unit.Velocity
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pure-JVM test for the Filtry drag-to-close gate (no Compose runtime). The gate
 * lets the [androidx.compose.material3.ModalBottomSheet] close on a downward drag
 * ONLY when the drag started with the list already at the top; a drag that began
 * below the top must have its leftover downward scroll/fling swallowed so the
 * sheet never sees it. Behavioural confirmation on a real device lives in
 * FiltersSheetDragDismissTest.
 */
class TopOnlyDismissScrollGateTest {

    private val downScroll = Offset(0f, 30f)
    private val upScroll = Offset(0f, -30f)
    private val downFling = Velocity(0f, 800f)
    private val upFling = Velocity(0f, -800f)

    private fun gate(atTop: Boolean): TopOnlyDismissScrollGate {
        var top = atTop
        return TopOnlyDismissScrollGate(atTop = { top }).also { it.onDragStart() }
    }

    @Test
    fun dragStartedBelowTheTop_swallowsLeftoverDownwardScrollAndFling() {
        val gate = gate(atTop = false)
        // Swallowed (kept from the sheet) — this is the fix.
        assertEquals(downScroll, gate.swallowedPostScroll(downScroll))
        assertEquals(downFling, gate.swallowedPostFling(downFling))
    }

    @Test
    fun dragStartedBelowTheTop_doesNotTouchUpwardScrollOrFling() {
        val gate = gate(atTop = false)
        // Upward leftover is the list expanding/over-scrolling up — leave it alone.
        assertEquals(Offset.Zero, gate.swallowedPostScroll(upScroll))
        assertEquals(Velocity.Zero, gate.swallowedPostFling(upFling))
    }

    @Test
    fun dragStartedAtTheTop_letsTheSheetCloseNormally() {
        val gate = gate(atTop = true)
        // Nothing swallowed → the sheet's own connection drags it closed.
        assertEquals(Offset.Zero, gate.swallowedPostScroll(downScroll))
        assertEquals(Velocity.Zero, gate.swallowedPostFling(downFling))
    }

    @Test
    fun snapshotIsTakenAtDragStartNotLater() {
        var atTop = false
        val gate = TopOnlyDismissScrollGate(atTop = { atTop })
        gate.onDragStart() // finger went down below the top
        atTop = true        // list then scrolled to the top mid-gesture
        // The gesture started below the top, so the leftover stays swallowed —
        // the sheet must not close just because the list reached the top.
        assertEquals(downScroll, gate.swallowedPostScroll(downScroll))
    }
}
