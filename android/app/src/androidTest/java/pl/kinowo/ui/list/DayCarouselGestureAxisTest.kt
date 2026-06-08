package pl.kinowo.ui.list

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.performTouchInput
import androidx.test.ext.junit.runners.AndroidJUnit4
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.filter.DateFilter

/**
 * On-device (emulator) gesture-axis arbitration for the day-swipe carousel.
 * Real touch arbitration only happens on a device — Robolectric's synthetic
 * gestures decide the axis differently, so this can't live in the JVM suite.
 *
 * The bug this pins (reproduced on the emulator): at the top of a column, a
 * mostly-horizontal swipe with a slight UPWARD drift let the inner grid win the
 * gesture — the list scrolled down and the day never changed. A horizontal swipe
 * must change the day and leave the column at the top; a vertical drag must
 * scroll the column and leave the day alone.
 *
 * Run: ./gradlew app:connectedDebugAndroidTest --tests *DayCarouselGestureAxisTest
 */
@RunWith(AndroidJUnit4::class)
class DayCarouselGestureAxisTest {

    @get:Rule
    val compose = createComposeRule()

    private class Probe(val day: () -> DateFilter, val colIndex: () -> Int)

    private fun mount(): Probe {
        var current by mutableStateOf(DateFilter.Today)
        lateinit var shared: LazyGridState
        compose.setContent {
            shared = rememberLazyGridState()
            DayCarousel(
                current = current,
                sharedScroll = shared,
                onCommitDay = { current = it },
                modifier = Modifier.testTag("carousel"),
            ) { day, state, columnModifier ->
                if (day == current) ScrollToTopOnChange(state, current)
                LazyVerticalGrid(
                    columns = GridCells.Fixed(2),
                    state = state,
                    modifier = columnModifier.fillMaxSize(),
                ) {
                    items((0 until 40).toList()) { i ->
                        Box(Modifier.fillMaxWidth().aspectRatio(2f / 3f).background(Color.Gray).testTag("card$i"))
                    }
                }
            }
        }
        return Probe({ current }, { shared.firstVisibleItemIndex })
    }

    /** One leftward swipe over [hFrac] of the width with a vertical travel of
     *  hTravel/[hToV] (negative dy = upward drift). */
    private fun Probe.swipe(hToV: Float, hFrac: Float = 0.7f) {
        compose.onNodeWithTag("carousel").performTouchInput {
            val h = width * hFrac
            val v = h / hToV
            val startX = right * 0.9f
            val y = center.y + v / 2f
            down(Offset(startX, y))
            moveTo(Offset(startX - h * 0.25f, y - v * 0.25f))
            moveTo(Offset(startX - h * 0.6f, y - v * 0.6f))
            moveTo(Offset(startX - h, y - v))
            up()
        }
        compose.waitForIdle()
    }

    @Test
    fun horizontalSwipeChangesDayAndLeavesColumnAtTop() {
        val p = mount()
        compose.waitForIdle()
        p.swipe(hToV = 3f)   // clearly horizontal: 3 across for 1 up
        assertEquals("a horizontal swipe must change the day", DateFilter.Tomorrow, p.day())
        assertEquals(
            "a horizontal day-swipe must NOT scroll the column (it stayed at the top)",
            0, p.colIndex(),
        )
    }

    @Test
    fun verticalDragScrollsColumnAndKeepsTheDay() {
        val p = mount()
        compose.waitForIdle()
        p.swipe(hToV = 0.33f)   // clearly vertical: 1 across for 3 up
        assertEquals("a vertical drag must not change the day", DateFilter.Today, p.day())
        assertTrue(
            "a vertical drag must scroll the column (got idx ${p.colIndex()})",
            p.colIndex() > 3,
        )
    }
}
