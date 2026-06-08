package pl.kinowo.ui.list

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.material3.Text
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.performScrollToIndex
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.test.swipeLeft
import androidx.compose.ui.test.swipeRight
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.filter.DateFilter

/**
 * Off-device Robolectric checks of the day-swipe carousel: a horizontal swipe
 * steps the selected day among [DateFilter.presets] (wrapping at both ends), a
 * drag too short to cross the commit threshold leaves the day unchanged, a
 * committed swipe rolls the new day's grid back to the top, and a swipe made
 * while already at the top stays put (no jank).
 *
 * The carousel is generic over what each column renders, so these tests feed it
 * plain test grids and observe the committed [DateFilter] (and the centre
 * column's scroll) directly — the gesture logic is what's under test. The
 * harness mirrors ListScreen's scroll-reset wiring: ScrollToTopOnChange is keyed
 * on the committed day, so every day change eases the centre column to the top.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DayCarouselTest {

    @get:Rule
    val compose = createComposeRule()

    // The presets, in order: Dziś, Jutro, 7 dni, Wszystkie.
    private val today = DateFilter.Today
    private val tomorrow = DateFilter.Tomorrow
    private val anytime = DateFilter.Anytime

    // Renders the carousel starting on [start]; returns a getter for the
    // currently-committed day and the centre column's scroll state.
    private class Harness(
        val day: () -> DateFilter,
        val centerScroll: () -> LazyGridState,
    )

    private fun render(start: DateFilter): Harness {
        var current by mutableStateOf(start)
        lateinit var sharedScroll: LazyGridState
        compose.setContent {
            sharedScroll = rememberLazyGridState()
            DayCarousel(
                current = current,
                sharedScroll = sharedScroll,
                onCommitDay = { current = it },
                modifier = Modifier.testTag("dayCarousel"),
            ) { day, state, columnModifier ->
                // Mirror ListScreen: the centre column eases to the top on every
                // day change (keyed on the committed day). The centre grid carries
                // the tag below.
                val isCenter = day == current
                if (isCenter) ScrollToTopOnChange(state, current)
                LazyVerticalGrid(
                    columns = GridCells.Fixed(1),
                    state = state,
                    modifier = columnModifier
                        .fillMaxSize()
                        .then(if (isCenter) Modifier.testTag("centerGrid") else Modifier),
                ) {
                    items((0 until 100).toList()) { Text("row $it", Modifier.height(50.dp)) }
                }
            }
        }
        // The carousel only lays out its strip once it has measured a width; the
        // touch target is the whole carousel box.
        return Harness({ current }, { sharedScroll })
    }

    @Test
    fun swipeLeftAdvancesDay() {
        val h = render(today)
        compose.onNodeWithTag("dayCarousel").performTouchInput { swipeLeft() }
        compose.waitForIdle()
        compose.runOnIdle { assertEquals(tomorrow, h.day()) }
    }

    @Test
    fun swipeRightFromTodayWrapsToAnytime() {
        val h = render(today)
        compose.onNodeWithTag("dayCarousel").performTouchInput { swipeRight() }
        compose.waitForIdle()
        compose.runOnIdle { assertEquals(anytime, h.day()) }
    }

    @Test
    fun shortDragBelowThresholdAbortsToSameDay() {
        val h = render(today)
        // A tiny horizontal nudge — well under 25% of the width — must not commit.
        compose.onNodeWithTag("dayCarousel").performTouchInput {
            swipeLeft(startX = centerX, endX = centerX - 20f)
        }
        compose.waitForIdle()
        compose.runOnIdle { assertEquals(today, h.day()) }
    }

    @Test
    fun committedSwipeRollsNewDayToTop() {
        val h = render(today)
        // Scroll the centre column down, then swipe to a new day: the new day
        // rolls back to the top so the user starts it at its first row. The
        // centre column is the 2nd of three; its grid carries the tag below.
        compose.onNodeWithTag("centerGrid").performScrollToIndex(40)
        compose.runOnIdle { assertEquals(40, h.centerScroll().firstVisibleItemIndex) }

        compose.onNodeWithTag("dayCarousel").performTouchInput { swipeLeft() }
        compose.waitForIdle()
        compose.runOnIdle {
            assertEquals(tomorrow, h.day())
            assertEquals(0, h.centerScroll().firstVisibleItemIndex)
        }
    }

    @Test
    fun swipeWhileAtTopStaysAtTop() {
        val h = render(today)
        // Already at the top: a swipe changes the day but the roll-to-top is a
        // no-op, so the column never visibly janks up (the original complaint).
        compose.runOnIdle { assertEquals(0, h.centerScroll().firstVisibleItemIndex) }

        compose.onNodeWithTag("dayCarousel").performTouchInput { swipeLeft() }
        compose.waitForIdle()
        compose.runOnIdle {
            assertEquals(tomorrow, h.day())
            assertEquals(0, h.centerScroll().firstVisibleItemIndex)
            assertEquals(0, h.centerScroll().firstVisibleItemScrollOffset)
        }
    }
}
