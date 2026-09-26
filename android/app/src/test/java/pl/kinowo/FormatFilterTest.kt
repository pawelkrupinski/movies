package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.hasImaxShowtime
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
    fun languageConstraintOnAnotherCountrysToken() {
        // The Wersja choice now offers the selected country's own pair, so a
        // German user's "subtitles" is `OmU` — and it must match German
        // showtimes, not Poland's `NAP` which no German screening carries.
        val f = FormatFilter(language = "OmU")
        assertTrue(f.matches(slot("18:00", "2D OmU")))
        assertFalse(f.matches(slot("18:00", "2D DF")))
        assertFalse(f.matches(slot("18:00", "2D NAP")))
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

    private fun film(vararg formatsByDay: List<String>) = TestData.film(
        "Film",
        formatsByDay.mapIndexed { i, formats ->
            TestData.day("2026-05-2$i", listOf(TestData.cinema("Kino", formats.map { TestData.slot("18:00", it) })))
        },
    )

    // The Filtry sheet offers "IMAX only" just where a showtime on ANY loaded
    // day carries the token — elsewhere it could only blank the list.
    @Test
    fun hasImaxShowtimeLooksAtEveryDay() {
        assertFalse(emptyList<pl.kinowo.model.Film>().hasImaxShowtime())
        assertFalse(listOf(film(listOf("2D NAP"), listOf("3D DUB"))).hasImaxShowtime())
        assertTrue(listOf(film(listOf("2D NAP")), film(listOf("2D"), listOf("IMAX 3D"))).hasImaxShowtime())
    }

    // An IMAX pick carried into a city without IMAX (city switch, deep link)
    // is dropped rather than blanking a list whose sheet has no toggle to undo it.
    @Test
    fun applicableDropsImaxWhereNoShowtimeHasIt() {
        val f = FormatFilter(imax = true, dimension = "3D")
        assertEquals(FormatFilter(dimension = "3D"), f.applicable(listOf(film(listOf("3D NAP")))))
        assertEquals(f, f.applicable(listOf(film(listOf("IMAX 3D")))))
    }
}
