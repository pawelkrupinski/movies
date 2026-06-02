package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Test
import java.util.Locale

/**
 * Locks the IMDb / Filmweb pill score format. The bug this guards: a
 * whole-number score (7.0) used to render "7" because the old `trimNum`
 * dropped the decimal for integers — out of step with web + iOS, which always
 * show one decimal.
 */
class RatingFormatTest {

    @Test
    fun `whole-number score keeps the tenths place`() {
        assertEquals("7.0", oneDecimal(7.0))
        assertEquals("8.0", oneDecimal(8.0))
    }

    @Test
    fun `fractional score is shown to one decimal`() {
        assertEquals("7.7", oneDecimal(7.7))
    }

    @Test
    fun `raw score is rounded to one decimal`() {
        // A real filmweb value: rounds to 7.0 and must still show the ".0".
        assertEquals("7.0", oneDecimal(6.97571))
        assertEquals("7.8", oneDecimal(7.84))
    }

    @Test
    fun `uses a dot separator even under a comma-decimal locale`() {
        val previous = Locale.getDefault()
        try {
            Locale.setDefault(Locale.forLanguageTag("pl-PL"))
            assertEquals("7.0", oneDecimal(7.0))
        } finally {
            Locale.setDefault(previous)
        }
    }
}
