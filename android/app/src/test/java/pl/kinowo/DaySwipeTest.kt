package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.filter.wrappedDayIndex

class DaySwipeTest {

    // The day list is [Dziś, Jutro, 7 dni, Wszystkie] → four stops.
    private val count = 4

    @Test
    fun nextStepsForward() {
        assertEquals(1, wrappedDayIndex(current = 0, delta = +1, count = count))
        assertEquals(2, wrappedDayIndex(current = 1, delta = +1, count = count))
        assertEquals(3, wrappedDayIndex(current = 2, delta = +1, count = count))
    }

    @Test
    fun previousStepsBackward() {
        assertEquals(2, wrappedDayIndex(current = 3, delta = -1, count = count))
        assertEquals(0, wrappedDayIndex(current = 1, delta = -1, count = count))
    }

    @Test
    fun wrapsPastTheEnd() {
        // Swiping next from the last stop (Wszystkie) lands back on the first.
        assertEquals(0, wrappedDayIndex(current = 3, delta = +1, count = count))
    }

    @Test
    fun wrapsBeforeTheStart() {
        // Swiping previous from the first stop (Dziś) lands on the last.
        assertEquals(3, wrappedDayIndex(current = 0, delta = -1, count = count))
    }

    @Test
    fun degenerateCounts() {
        // Single stop: every swipe stays put. Empty/negative: no movement.
        assertEquals(0, wrappedDayIndex(current = 0, delta = +1, count = 1))
        assertEquals(0, wrappedDayIndex(current = 0, delta = -1, count = 1))
        assertEquals(5, wrappedDayIndex(current = 5, delta = +1, count = 0))
    }
}
