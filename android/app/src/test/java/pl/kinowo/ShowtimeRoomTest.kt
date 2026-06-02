package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test
import pl.kinowo.model.Showtime

/**
 * `displayRoom` decides whether a long-press on a showtime pill pops a room
 * tooltip — a blank or whitespace-only room must never show an empty bubble.
 * Mirrors iOS `Showtime.displayRoom`.
 */
class ShowtimeRoomTest {
    private fun showtime(room: String?) =
        Showtime(time = "18:30", format = "2D NAP", room = room)

    @Test
    fun `a real room name is surfaced verbatim`() {
        assertEquals("Sala 8", showtime("Sala 8").displayRoom)
    }

    @Test
    fun `surrounding whitespace is trimmed`() {
        assertEquals("Sala 8", showtime("  Sala 8 ").displayRoom)
    }

    @Test
    fun `a null room yields no tooltip`() {
        assertNull(showtime(null).displayRoom)
    }

    @Test
    fun `a blank or whitespace-only room yields no tooltip`() {
        assertNull(showtime("").displayRoom)
        assertNull(showtime("   ").displayRoom)
    }
}
