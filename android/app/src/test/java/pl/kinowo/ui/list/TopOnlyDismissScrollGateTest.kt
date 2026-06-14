package pl.kinowo.ui.list

import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.input.nestedscroll.NestedScrollSource
import androidx.compose.ui.unit.Velocity
import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Pure-JVM test for the Filtry drag-to-close gate (no Compose runtime). The gate
 * lets the [androidx.compose.material3.ModalBottomSheet] close on a downward drag
 * ONLY when the drag started with the list already at the top; a drag that began
 * below the top must have its leftover swallowed so the sheet never sees it.
 * Drag-scroll leftover ([NestedScrollSource.UserInput]) is swallowed, but FLING
 * frames ([NestedScrollSource.SideEffect]) pass through so the list's fling can
 * cancel at the top instead of looping in place. Behavioural confirmation on a
 * real device lives in FiltersSheetDragDismissTest.
 */
class TopOnlyDismissScrollGateTest {

    private val drag = NestedScrollSource.UserInput
    private val fling = NestedScrollSource.SideEffect
    private val downScroll = Offset(0f, 30f)
    private val upScroll = Offset(0f, -30f)
    private val downFling = Velocity(0f, 800f)
    private val upFling = Velocity(0f, -800f)

    private fun gate(atTop: Boolean): TopOnlyDismissScrollGate {
        var top = atTop
        return TopOnlyDismissScrollGate(atTop = { top }).also { it.onDragStart() }
    }

    @Test
    fun dragStartedBelowTheTop_swallowsLeftoverDownwardDragScrollAndFling() {
        val gate = gate(atTop = false)
        // Swallowed (kept from the sheet) — this is the fix.
        assertEquals(downScroll, gate.swallowedPostScroll(downScroll, drag))
        assertEquals(downFling, gate.swallowedPostFling(downFling))
    }

    @Test
    fun flingFramesPassThroughSoTheListCanStopAtTheTop() {
        val gate = gate(atTop = false)
        // A fling's per-frame leftover must NOT be swallowed — otherwise the list's
        // fling never cancels at the boundary and keeps looping, eating the next tap.
        // (The sheet ignores SideEffect in its own onPostScroll, so this can't close it.)
        assertEquals(Offset.Zero, gate.swallowedPostScroll(downScroll, fling))
    }

    @Test
    fun dragStartedBelowTheTop_doesNotTouchUpwardScrollOrFling() {
        val gate = gate(atTop = false)
        // Upward leftover is the list expanding/over-scrolling up — leave it alone.
        assertEquals(Offset.Zero, gate.swallowedPostScroll(upScroll, drag))
        assertEquals(Velocity.Zero, gate.swallowedPostFling(upFling))
    }

    @Test
    fun dragStartedAtTheTop_letsTheSheetCloseNormally() {
        val gate = gate(atTop = true)
        // Nothing swallowed → the sheet's own connection drags it closed.
        assertEquals(Offset.Zero, gate.swallowedPostScroll(downScroll, drag))
        assertEquals(Velocity.Zero, gate.swallowedPostFling(downFling))
    }

    @Test
    fun snapshotIsTakenAtDragStartNotLater() {
        var atTop = false
        val gate = TopOnlyDismissScrollGate(atTop = { atTop })
        gate.onDragStart() // drag began below the top
        atTop = true        // list then scrolled to the top mid-gesture
        // The gesture started below the top, so the leftover stays swallowed —
        // the sheet must not close just because the list reached the top.
        assertEquals(downScroll, gate.swallowedPostScroll(downScroll, drag))
    }

    @Test
    fun eachDragReSnapshotsAtItsStart() {
        var atTop = false
        val gate = TopOnlyDismissScrollGate(atTop = { atTop })
        gate.onDragStart()                                        // drag 1: below the top
        assertEquals(downScroll, gate.swallowedPostScroll(downScroll, drag))
        atTop = true
        gate.onDragStart()                                        // drag 2: at the top
        assertEquals(Offset.Zero, gate.swallowedPostScroll(downScroll, drag))
    }
}
