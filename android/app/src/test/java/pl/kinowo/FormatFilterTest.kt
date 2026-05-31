package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.filter.FormatFilter
import pl.kinowo.model.Showtime

class FormatFilterTest {

    private fun slot(time: String, format: String): Showtime =
        Showtime(time = time, format = format, room = null, bookingURL = null)

    @Test
    fun emptyFilterMatchesEverything() {
        val f = FormatFilter()
        assertTrue(f.isEmpty)
        assertTrue(f.matches(slot("10:00", "2D NAP")))
        assertTrue(f.matches(slot("23:59", "3D DUB IMAX")))
        assertTrue(f.matches(slot("abc", "")))
    }

    @Test
    fun dimensionConstraint() {
        val f = FormatFilter(dimension = "3D")
        assertFalse(f.isEmpty)
        assertTrue(f.matches(slot("18:00", "3D NAP")))
        assertFalse(f.matches(slot("18:00", "2D NAP")))
    }

    @Test
    fun languageConstraint() {
        val f = FormatFilter(language = "NAP")
        assertTrue(f.matches(slot("18:00", "2D NAP")))
        assertFalse(f.matches(slot("18:00", "2D DUB")))
    }

    @Test
    fun imaxRequiresImaxToken() {
        val f = FormatFilter(imax = true)
        assertTrue(f.matches(slot("20:00", "IMAX 3D")))
        assertFalse(f.matches(slot("20:00", "3D NAP")))
    }

    @Test
    fun fromHourMinuteBoundary() {
        val f = FormatFilter(fromHour = 18, fromMinute = 30)
        assertEquals(18 * 60 + 30, f.fromMinutes)
        assertTrue(f.matches(slot("18:30", "2D NAP")))
        assertFalse(f.matches(slot("18:29", "2D NAP")))
        assertTrue(f.matches(slot("19:00", "2D NAP")))
        // Unparseable time is kept — mirrors the web's `timeMin < 0` guard.
        assertTrue(f.matches(slot("abc", "2D NAP")))
    }

    @Test
    fun fromHourDowolnaIsNoConstraint() {
        val f = FormatFilter(fromHour = -1, fromMinute = 30)
        assertNull(f.fromMinutes)
        assertTrue(f.isEmpty)
        assertTrue(f.matches(slot("00:00", "2D NAP")))
    }

    @Test
    fun multipleConstraintsCombine() {
        val f = FormatFilter(dimension = "3D", language = "NAP", fromHour = 18, fromMinute = 30)
        assertTrue(f.matches(slot("19:00", "3D NAP")))
        assertFalse(f.matches(slot("18:00", "3D NAP")))
        assertFalse(f.matches(slot("19:00", "2D NAP")))
        assertFalse(f.matches(slot("19:00", "3D DUB")))
        assertTrue(f.matches(slot("18:30", "3D NAP IMAX")))
    }
}
