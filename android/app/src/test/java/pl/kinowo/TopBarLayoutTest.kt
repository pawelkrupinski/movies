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
    fun wideRowFillsEveryPill() {
        // Landscape phones / tablets: every pill shares the row equally and
        // reads as one evenly-spaced segmented control.
        assertTrue(TopBarLayout.datePillFillsRow(wide = true))
    }

    @Test
    fun narrowRowKeepsEveryPillIntrinsic() {
        // Portrait phones: no pill stretches — each takes its intrinsic width so
        // a label can't be squeezed below its text and clip (Galaxy S24, 360dp).
        assertFalse(TopBarLayout.datePillFillsRow(wide = false))
    }
}
