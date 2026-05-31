package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.slot
import pl.kinowo.filter.FormatTokenFilter

class FormatTokenFilterTest {

    @Test
    fun commonTokensIsTheIntersectionAcrossShowtimes() {
        // Every slot is 2D, but language differs → only "2D" is common.
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "2D DUB"), slot("20:00", "2D NAP")))
        assertEquals(setOf("2D"), FormatTokenFilter.commonTokens(cg))
    }

    @Test
    fun commonTokensKeepsAllWhenEveryShowtimeMatches() {
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "2D NAP")))
        assertEquals(setOf("2D", "NAP"), FormatTokenFilter.commonTokens(cg))
    }

    @Test
    fun commonTokensEmptyWhenNoSharedToken() {
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "3D DUB")))
        assertEquals(emptySet<String>(), FormatTokenFilter.commonTokens(cg))
    }

    @Test
    fun filterRemovesTheCommonTokens() {
        // "2D" is redundant when every slot is 2D — drop it from the per-badge label.
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", setOf("2D")))
        assertEquals("IMAX 3D", FormatTokenFilter.filter("IMAX 3D DUB", setOf("DUB")))
    }

    @Test
    fun filterIsANoOpWhenNothingIsCommon() {
        assertEquals("2D NAP", FormatTokenFilter.filter("2D NAP", emptySet()))
    }
}
