package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
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
    // ── commonVersion ────────────────────────────────────────────────────
    //
    // The version a whole cinema shares is what the LABEL says, so it is not
    // simply dropped with the rest of the intersection. This is the napisy-vs-
    // dubbing bug: every slot tagged "2D DUB" left no DUB anywhere on screen.

    @Test
    fun commonVersionIsTheSharedLanguageToken() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))
        assertEquals(listOf("DUB"), FormatTokenFilter.commonVersion(cg))
    }

    @Test
    fun commonVersionKeepsSourceOrderAndDropsScreenFormat() {
        val cg = cinema("Multikino", listOf(slot("14:30", "IMAX NAP ATMOS"), slot("17:00", "IMAX NAP ATMOS")))
        assertEquals(listOf("NAP"), FormatTokenFilter.commonVersion(cg))
    }

    @Test
    fun commonVersionEmptyWhenTheVersionDiffersBetweenSlots() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D NAP"), slot("17:00", "2D DUB")))
        assertEquals(emptyList<String>(), FormatTokenFilter.commonVersion(cg))
    }

    @Test
    fun commonVersionEmptyWhenNothingIsCommon() {
        val cg = cinema("X", listOf(slot("10:00", "2D"), slot("12:00", "IMAX 3D")))
        assertEquals(emptyList<String>(), FormatTokenFilter.commonVersion(cg))
    }

    // ── tokensToStrip ────────────────────────────────────────────────────
    //
    // The whole rendering decision: what a chip drops, and therefore what is
    // left to read. `CinemaVersionLabelTest` renders it; these pin the rule.

    @Test
    fun withACinemaLabelTheSharedVersionLeavesTheChip() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))
        val strip = FormatTokenFilter.tokensToStrip(cg, hasLabel = true)
        assertEquals(setOf("2D", "DUB"), strip)
        // Nothing on the chip — the label says DUB once instead.
        assertEquals("", FormatTokenFilter.filter("2D DUB", strip))
    }

    @Test
    fun withNoCinemaLabelTheSharedVersionStaysOnTheChip() {
        val cg = cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))
        val strip = FormatTokenFilter.tokensToStrip(cg, hasLabel = false)
        assertEquals(setOf("2D"), strip)
        assertEquals("DUB", FormatTokenFilter.filter("2D DUB", strip))
    }

    @Test
    fun aSharedScreenFormatIsStillDroppedWithNoLabel() {
        val cg = cinema("X", listOf(slot("14:30", "2D"), slot("17:00", "2D")))
        // "2D" on every chip tells a visitor nothing — that is what the
        // stripping is FOR, and it keeps working with or without a label.
        assertEquals(setOf("2D"), FormatTokenFilter.tokensToStrip(cg, hasLabel = false))
    }

    // ── isLanguageVersion ────────────────────────────────────────────────

    @Test
    fun everyMarketsVersionSpellingCountsAsAVersion() {
        for (token in listOf("NAP", "DUB", "LEK", "LEC", "ORG", "SUB",
                             "VO", "VOSE", "VOSI", "DOB", "CAT", "OV", "OmU", "OmeU", "DF")) {
            assertTrue("$token is a version", FormatTokenFilter.isLanguageVersion(token))
        }
        // An audio language a source names is a version too — at a UK multiplex
        // "Hindi" is the whole difference between two screenings of one film.
        assertTrue(FormatTokenFilter.isLanguageVersion("HINDI"))
    }

    @Test
    fun screenFormatAndAccessibilityAreNotVersions() {
        for (token in listOf("2D", "3D", "IMAX", "4DX", "SCREENX", "ATMOS", "DOLBY",
                             "LASER", "70MM", "VIP", "PREMIUM", "AD", "OC")) {
            assertFalse("$token is not a version", FormatTokenFilter.isLanguageVersion(token))
        }
    }

}
