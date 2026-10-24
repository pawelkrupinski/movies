package pl.kinowo.filter

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import java.time.Instant
import java.time.ZoneId

/** A search looks through every day: the first letter moves the listing to "All" and remembers
 *  the day it was on; clearing goes back there, unless a day was picked while it ran. */
class SearchDayTest {

    @Test
    fun `a search moves to All and clearing goes back`() {
        val started = SearchDay().queryChanged("s", showing = DateFilter.Tomorrow)
        assertEquals(DateFilter.Anytime, started.moveTo)
        val typing = started.next.queryChanged("st", showing = DateFilter.Anytime)
        assertNull("typing on moves nothing", typing.moveTo)
        assertEquals(DateFilter.Tomorrow, typing.next.outsideOf)
        val cleared = typing.next.queryChanged("  ", showing = DateFilter.Anytime)
        assertEquals(DateFilter.Tomorrow, cleared.moveTo)
        assertNull(cleared.next.outsideOf)
    }

    @Test
    fun `a day picked during the search stays when it is cleared`() {
        val picked = SearchDay().queryChanged("dune", showing = DateFilter.Today).next.dayPicked()
        assertNull("a picked day shows only itself: nothing is drawn lighter", picked.outsideOf)
        val cleared = picked.queryChanged("", showing = DateFilter.Week)
        assertNull(cleared.moveTo)
        // The next search starts afresh from wherever the listing is.
        val again = cleared.next.queryChanged("a", showing = DateFilter.Week)
        assertEquals(DateFilter.Anytime, again.moveTo)
        assertEquals(DateFilter.Week, again.next.queryChanged("", showing = DateFilter.Anytime).moveTo)
    }

    @Test
    fun `a pick with no search running is nothing`() {
        val day = SearchDay().dayPicked()
        assertEquals(SearchDay(), day)
    }

    @Test
    fun `the days outside the searched-from day are drawn lighter, only while the listing shows every day`() {
        val zone = ZoneId.of("Europe/Warsaw")
        val now = Instant.parse("2026-06-08T10:00:00Z")
        val searching = SearchDay().queryChanged("a", showing = DateFilter.Today).next
        val outside = searching.outsideDays(DateFilter.Anytime, now, zone)!!
        assertFalse(outside("2026-06-08"))
        assertTrue(outside("2026-06-09"))
        assertNull("a page of one day draws nothing lighter", searching.outsideDays(DateFilter.Tomorrow, now, zone))
        assertNull(SearchDay().outsideDays(DateFilter.Anytime, now, zone))
    }
}
