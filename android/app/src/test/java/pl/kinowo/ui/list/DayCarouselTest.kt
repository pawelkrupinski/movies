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
 * committed swipe KEEPS the scroll position (the mirror carries it into the new
 * day), and a date-PILL tap drops the new day's grid back to the top.
 *
 * The carousel is generic over what each column renders, so these tests feed it
 * plain test grids and observe the committed [DateFilter] (and the centre
 * column's scroll) directly — the gesture logic is what's under test. The
 * harness mirrors ListScreen's scroll-reset wiring: ScrollToTopOnChange is keyed
 * on a pill-tap token, NOT on the committed day, so a swipe never resets it.
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
        val tapPill: (DateFilter) -> Unit,
    )

    private fun render(start: DateFilter): Harness {
        var current by mutableStateOf(start)
        // Mirrors ListScreen.pillResetToken: bumped only on a date-pill tap, never
        // on a swipe-commit, so the scroll-reset fires for taps but not swipes.
        var pillResetToken by mutableStateOf(0)
        lateinit var sharedScroll: LazyGridState
        compose.setContent {
            sharedScroll = rememberLazyGridState()
            DayCarousel(
                current = current,
                sharedScroll = sharedScroll,
                onCommitDay = { current = it },
                modifier = Modifier.testTag("dayCarousel"),
            ) { day, state, columnModifier ->
                // Mirror ListScreen: only the centre column resets to the top, and
                // only on a pill tap (pillResetToken) — a swipe keeps the scroll the
                // mirror carried over. The centre grid carries the tag below.
                val isCenter = day == current
                if (isCenter) ScrollToTopOnChange(state, pillResetToken)
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
        return Harness({ current }, { sharedScroll }, { current = it; pillResetToken++ })
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
    fun committedSwipePreservesScroll() {
        val h = render(today)
        // Scroll the centre column down, then swipe to a new day: the scroll the
        // mirror carried over must survive — the user lands at the same place in
        // the new day, NOT bounced back to the top. The centre column is the 2nd
        // of three; its grid carries the tag below.
        compose.onNodeWithTag("centerGrid").performScrollToIndex(40)
        compose.runOnIdle { assertEquals(40, h.centerScroll().firstVisibleItemIndex) }

        compose.onNodeWithTag("dayCarousel").performTouchInput { swipeLeft() }
        compose.waitForIdle()
        compose.runOnIdle {
            assertEquals(tomorrow, h.day())
            assertEquals(40, h.centerScroll().firstVisibleItemIndex)
        }
    }

    @Test
    fun pillTapScrollsNewDayToTop() {
        val h = render(today)
        // A date-PILL tap (unlike a swipe) drops the list back to the top, so the
        // user starts the freshly-picked day at its first row.
        compose.onNodeWithTag("centerGrid").performScrollToIndex(40)
        compose.runOnIdle { assertEquals(40, h.centerScroll().firstVisibleItemIndex) }

        compose.runOnIdle { h.tapPill(tomorrow) }
        compose.waitForIdle()
        compose.runOnIdle {
            assertEquals(tomorrow, h.day())
            assertEquals(0, h.centerScroll().firstVisibleItemIndex)
        }
    }
}
