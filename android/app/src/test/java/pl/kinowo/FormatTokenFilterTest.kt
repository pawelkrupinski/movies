package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.slot
import pl.kinowo.filter.FormatTokenFilter

class FormatTokenFilterTest {

    @Test
    fun tokensToStripIsTheIntersectionAcrossShowtimes() {
        // Every slot is 2D, but language differs → only "2D" is common.
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "2D DUB"), slot("20:00", "2D NAP")))
        assertEquals(setOf("2D"), FormatTokenFilter.tokensToStrip(cg))
    }

    @Test
    fun tokensToStripKeepsAllWhenEveryShowtimeMatches() {
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "2D NAP")))
        assertEquals(setOf("2D", "NAP"), FormatTokenFilter.tokensToStrip(cg))
    }

    @Test
    fun tokensToStripEmptyWhenNoSharedToken() {
        val cg = cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "3D DUB")))
        assertEquals(emptySet<String>(), FormatTokenFilter.tokensToStrip(cg))
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
    // ── what a chip is left showing ──────────────────────────────────────
    //
    // The whole rendering decision: what a chip drops, and therefore what is
    // left to read. `UniformFormatChipTest` renders it; these pin the rule.

    @Test
    fun aVersionEverySlotSharesIsDroppedLikeAnyOtherToken() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))
        val common = FormatTokenFilter.tokensToStrip(cg)
        assertEquals(setOf("2D", "DUB"), common)
        // Six chips all saying DUB tell a visitor as little as six all saying
        // 2D — the cinema screens the film no other way.
        assertEquals("", FormatTokenFilter.filter("2D DUB", common))
    }

    @Test
    fun aSharedScreenFormatIsDropped() {
        val cg = cinema("X", listOf(slot("14:30", "IMAX NAP"), slot("17:00", "IMAX 3D NAP")))
        assertEquals(setOf("IMAX", "NAP"), FormatTokenFilter.tokensToStrip(cg))
        assertEquals("3D", FormatTokenFilter.filter("IMAX 3D NAP", setOf("IMAX", "NAP")))
    }

    @Test
    fun aVersionThatDiffersBetweenSlotsStaysOnEveryChip() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D NAP"), slot("17:00", "2D DUB")))
        val common = FormatTokenFilter.tokensToStrip(cg)
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", common))
        assertEquals("DUB", FormatTokenFilter.filter("2D DUB", common))
    }

}
