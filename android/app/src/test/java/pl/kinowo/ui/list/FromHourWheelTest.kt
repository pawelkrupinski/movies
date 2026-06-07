package pl.kinowo.ui.list

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.filter.FormatFilter

/**
 * Pins the value lists, labels, and selection rules behind the Od-godziny
 * drums: row 0 is the "Dowolna" (no-floor) sentinel, hours/minutes label
 * zero-padded, and choosing "Dowolna" also clears the minute floor.
 */
class FromHourWheelTest {

    @Test
    fun `hour list leads with the Dowolna sentinel then 0 to 23`() {
        assertEquals(25, FromHourWheel.hours.size)
        assertEquals(-1, FromHourWheel.hours.first())
        assertEquals(0, FromHourWheel.hours[1])
        assertEquals(23, FromHourWheel.hours.last())
    }

    @Test
    fun `hour label is Dowolna for the sentinel and zero-padded otherwise`() {
        assertEquals("Dowolna", FromHourWheel.hourLabel(-1))
        assertEquals("00", FromHourWheel.hourLabel(0))
        assertEquals("09", FromHourWheel.hourLabel(9))
        assertEquals("18", FromHourWheel.hourLabel(18))
    }

    @Test
    fun `minute list steps in quarters and labels zero-padded`() {
        assertEquals(listOf(0, 15, 30, 45), FromHourWheel.minutes)
        assertEquals("00", FromHourWheel.minuteLabel(0))
        assertEquals("45", FromHourWheel.minuteLabel(45))
    }

    @Test
    fun `choosing a real hour keeps the stored minute`() {
        val filter = FormatFilter(fromHour = -1, fromMinute = 30)
        val updated = FromHourWheel.withHour(filter, 18)
        assertEquals(18, updated.fromHour)
        assertEquals(30, updated.fromMinute)
    }

    @Test
    fun `choosing Dowolna clears the minute floor`() {
        val filter = FormatFilter(fromHour = 18, fromMinute = 45)
        val updated = FromHourWheel.withHour(filter, -1)
        assertEquals(-1, updated.fromHour)
        assertEquals(0, updated.fromMinute)
    }
}
