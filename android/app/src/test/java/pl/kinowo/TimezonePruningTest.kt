package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.ShowtimeClock
import pl.kinowo.filter.prunedPastShowings
import pl.kinowo.model.Showtime
import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime

/**
 * The UK/DE fix: past-showtime pruning and the Dziś/Jutro day buckets must reason
 * in the SELECTED country's zone, not a hardcoded Europe/Warsaw. Warsaw runs an
 * hour ahead of London, so the old code dropped London screenings ~1h early and
 * mis-bucketed them near midnight.
 */
class TimezonePruningTest {

    private val london: ZoneId = ZoneId.of("Europe/London")
    private val warsaw: ZoneId = ZoneId.of("Europe/Warsaw")

    private fun instant(zone: ZoneId, y: Int, mo: Int, d: Int, h: Int, mi: Int): Instant =
        ZonedDateTime.of(y, mo, d, h, mi, 0, 0, zone).toInstant()

    private fun slot(time: String): Showtime =
        Showtime(time = time, format = "2D", room = null, bookingURL = null)

    // The crux: it's 20:00 in London (= 21:00 in Warsaw). A 20:30 London show is
    // still 30 min away and must stay; judged on Warsaw wall-clock the same slot
    // reads an hour earlier and gets dropped before it starts.
    @Test
    fun londonShowKeptOnLondonTimeDroppedOnWarsaw() {
        val now = instant(london, 2026, 5, 22, 20, 0)
        assertTrue(ShowtimeClock.isFuture(slot("20:30"), "2026-05-22", now, london))
        assertFalse(ShowtimeClock.isFuture(slot("20:30"), "2026-05-22", now, warsaw))
    }

    @Test
    fun pruneKeepsLondonFilmThatWarsawWouldDrop() {
        val now = instant(london, 2026, 5, 22, 20, 0)
        val films = listOf(
            TestData.film("Evening Show", listOf(
                TestData.day("2026-05-22", listOf(
                    TestData.cinema("Picturehouse", listOf(slot("20:30"))),
                )),
            )),
        )
        assertEquals(1, films.prunedPastShowings(now, london).size)
        assertTrue(films.prunedPastShowings(now, warsaw).isEmpty())
    }

    // Near midnight the day bucket must follow the local calendar day. At 23:30
    // London it's already 00:30 the next day in Warsaw, so "today" differs.
    @Test
    fun todayBucketFollowsLocalCalendarDayNearMidnight() {
        val now = instant(london, 2026, 5, 22, 23, 30) // = 2026-05-23 00:30 Warsaw
        assertTrue(DateFilter.Today.matches("2026-05-22", now, london))
        assertFalse(DateFilter.Today.matches("2026-05-22", now, warsaw))
        assertTrue(DateFilter.Today.matches("2026-05-23", now, warsaw))
    }

    @Test
    fun isoRendersLocalCalendarDate() {
        val now = instant(london, 2026, 5, 22, 23, 30)
        assertEquals("2026-05-22", DateFilter.iso(now, london))
        assertEquals("2026-05-23", DateFilter.iso(now, warsaw))
    }

    // The default zone stays Warsaw, so every existing PL-only call site and test
    // keeps its prior behaviour without passing a zone.
    @Test
    fun defaultZoneIsWarsaw() {
        val now = instant(warsaw, 2026, 5, 22, 18, 0)
        assertEquals(DateFilter.iso(now, warsaw), DateFilter.iso(now))
        assertEquals(
            ShowtimeClock.isFuture(slot("17:30"), "2026-05-22", now, warsaw),
            ShowtimeClock.isFuture(slot("17:30"), "2026-05-22", now),
        )
    }
}
