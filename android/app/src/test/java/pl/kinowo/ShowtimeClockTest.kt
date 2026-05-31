package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.slot
import pl.kinowo.TestData.warsawInstant
import pl.kinowo.filter.ShowtimeClock

class ShowtimeClockTest {

    private val date = "2026-05-22"
    private val now = warsawInstant(2026, 5, 22, 18, 0) // 18:00 Warsaw

    @Test
    fun futureSlotIsFuture() {
        assertTrue(ShowtimeClock.isFuture(slot("19:00"), date, now))
    }

    @Test
    fun slotStartedWithinThirtyMinutesIsStillFuture() {
        // 17:45 is 15 min before now; the cutoff is now-30min (17:30), so it
        // is still "future" (a screening that started 15 min ago counts as live).
        assertTrue(ShowtimeClock.isFuture(slot("17:45"), date, now))
        // Exactly the boundary is kept (strictly-after now-30min).
        assertTrue(ShowtimeClock.isFuture(slot("17:31"), date, now))
    }

    @Test
    fun slotMoreThanThirtyMinutesAgoIsPast() {
        assertFalse(ShowtimeClock.isFuture(slot("17:29"), date, now))
        assertFalse(ShowtimeClock.isFuture(slot("09:00"), date, now))
    }

    @Test
    fun unparseableTimeIsKept() {
        assertTrue(ShowtimeClock.isFuture(slot("abc"), date, now))
        assertTrue(ShowtimeClock.isFuture(slot(""), date, now))
    }

    @Test
    fun slotOnAFutureDateIsFutureRegardlessOfTime() {
        assertTrue(ShowtimeClock.isFuture(slot("00:05"), "2026-05-23", now))
    }

    @Test
    fun earliestMinutesPicksTheSmallestMinuteOfDay() {
        val cg = cinema("X", listOf(slot("14:10"), slot("09:00"), slot("12:00")))
        assertEquals(9 * 60, ShowtimeClock.earliestMinutes(cg))
    }

    @Test
    fun earliestMinutesSentinelForNoParseableTimes() {
        val cg = cinema("X", listOf(slot("abc"), slot("--:--")))
        assertEquals(Int.MAX_VALUE, ShowtimeClock.earliestMinutes(cg))
    }
}
