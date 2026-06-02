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
}
