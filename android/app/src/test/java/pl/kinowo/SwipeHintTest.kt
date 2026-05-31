package pl.kinowo

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.ui.SwipeHint

class SwipeHintTest {

    @Test
    fun showsWhenNeverSwipedAndNotShownToday() {
        assertTrue(SwipeHint.shouldShow(hasSwiped = false, lastShownDate = "", today = "2026-05-31"))
        assertTrue(SwipeHint.shouldShow(hasSwiped = false, lastShownDate = "2026-05-30", today = "2026-05-31"))
    }

    @Test
    fun hiddenOnceAlreadyShownToday() {
        assertFalse(SwipeHint.shouldShow(hasSwiped = false, lastShownDate = "2026-05-31", today = "2026-05-31"))
    }

    @Test
    fun neverShownAgainAfterFirstSwipe() {
        // Even on a fresh day, a user who has swiped never sees the hint again.
        assertFalse(SwipeHint.shouldShow(hasSwiped = true, lastShownDate = "", today = "2026-05-31"))
        assertFalse(SwipeHint.shouldShow(hasSwiped = true, lastShownDate = "2026-05-30", today = "2026-05-31"))
    }
}
