package pl.kinowo

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.ui.TopBarLayout

class TopBarLayoutTest {

    @Test
    fun narrowPortraitPhoneKeepsSearchFloating() {
        assertFalse(TopBarLayout.searchInline(360))
        assertFalse(TopBarLayout.searchInline(411))
        assertFalse(TopBarLayout.searchInline(440))
    }

    @Test
    fun wideScreensInlineSearch() {
        assertTrue(TopBarLayout.searchInline(640)) // phone landscape
        assertTrue(TopBarLayout.searchInline(800)) // tablet portrait
        assertTrue(TopBarLayout.searchInline(1280)) // tablet landscape
    }

    @Test
    fun thresholdIsInclusive() {
        assertFalse(TopBarLayout.searchInline(599))
        assertTrue(TopBarLayout.searchInline(600))
    }

    @Test
    fun wideRowFillsEveryPillIncludingWszystkie() {
        // Landscape phones / tablets: all four pills share the row equally, so
        // "Wszystkie" (Anytime) stretches to the same width as the others.
        assertTrue(TopBarLayout.datePillFillsRow(isAnytime = true, wide = true))
        assertTrue(TopBarLayout.datePillFillsRow(isAnytime = false, wide = true))
    }

    @Test
    fun narrowRowKeepsWszystkieIntrinsic() {
        // Portrait phones: the three dated pills get weight, "Wszystkie" keeps
        // its intrinsic width so the row fits without clipping.
        assertTrue(TopBarLayout.datePillFillsRow(isAnytime = false, wide = false))
        assertFalse(TopBarLayout.datePillFillsRow(isAnytime = true, wide = false))
    }
}
