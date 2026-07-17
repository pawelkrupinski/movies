package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.TestData.warsawInstant
import pl.kinowo.filter.DateFilter

class DateFilterTest {

    @Test
    fun anytimeMatchesAnyDate() {
        val now = warsawInstant(2026, 5, 22)
        assertTrue(DateFilter.Anytime.matches("2026-05-22", now))
        assertTrue(DateFilter.Anytime.matches("2099-12-31", now))
        assertTrue(DateFilter.Anytime.matches("1999-01-01", now))
    }

    @Test
    fun todayMatchesOnlyTodayInWarsaw() {
        val now = warsawInstant(2026, 5, 22, 12, 0)
        assertTrue(DateFilter.Today.matches("2026-05-22", now))
        assertFalse(DateFilter.Today.matches("2026-05-23", now))
        assertFalse(DateFilter.Today.matches("2026-05-21", now))
    }

    @Test
    fun tomorrowMatchesNextDay() {
        val now = warsawInstant(2026, 5, 22, 12, 0)
        assertTrue(DateFilter.Tomorrow.matches("2026-05-23", now))
        assertFalse(DateFilter.Tomorrow.matches("2026-05-22", now))
        assertFalse(DateFilter.Tomorrow.matches("2026-05-24", now))
    }

    @Test
    fun weekMatchesTodayThroughDayPlus7Inclusive() {
        val now = warsawInstant(2026, 5, 22, 12, 0)
        assertTrue(DateFilter.Week.matches("2026-05-22", now))
        assertTrue(DateFilter.Week.matches("2026-05-26", now))
        assertTrue(DateFilter.Week.matches("2026-05-29", now))
        assertFalse(DateFilter.Week.matches("2026-05-30", now))
        assertFalse(DateFilter.Week.matches("2026-05-21", now))
    }

    @Test
    fun specificMatchesExactDateOnly() {
        val now = warsawInstant(2026, 5, 22)
        assertTrue(DateFilter.Specific("2026-05-22").matches("2026-05-22", now))
        assertFalse(DateFilter.Specific("2026-05-22").matches("2026-05-23", now))
        assertFalse(DateFilter.Specific("2026-05-22").matches("2026-05-21", now))
    }

    @Test
    fun isoReturnsWarsawLocalDate() {
        val noon = warsawInstant(2026, 5, 22, 12, 0)
        assertEquals("2026-05-22", DateFilter.iso(noon))

        // 23:30 Warsaw on 2026-05-22 is still 2026-05-22 locally, not the
        // UTC date — the formatter must honour the Warsaw timezone.
        val lateNight = warsawInstant(2026, 5, 22, 23, 30)
        assertEquals("2026-05-22", DateFilter.iso(lateNight))
    }

    @Test
    fun presetsAreFourNonNullEntriesInNarrowToBroadOrder() {
        // Guards the sealed-class init-order hazard that crashed DateChips:
        // a circular companion ⇄ object init can leave preset entries null.
        val presets = DateFilter.presets
        assertEquals(4, presets.size)
        assertTrue("no preset may be null", presets.none { @Suppress("SENSELESS_COMPARISON") (it == null) })
        // Narrow → broad order (labels now resolve per-locale via labelText()).
        assertEquals(
            listOf(DateFilter.Today, DateFilter.Tomorrow, DateFilter.Week, DateFilter.Anytime),
            presets,
        )
    }

    @Test
    fun todayAcrossDSTSpringForward() {
        // Poland flips to summer time on the last Sunday of March (2026-03-29);
        // any "today" right before/after that boundary still resolves to a
        // calendar date in Warsaw, not a UTC-shifted one.
        val beforeDST = warsawInstant(2026, 3, 28, 23, 0)
        assertTrue(DateFilter.Today.matches("2026-03-28", beforeDST))
        val afterDST = warsawInstant(2026, 3, 29, 12, 0)
        assertTrue(DateFilter.Today.matches("2026-03-29", afterDST))
    }
}
