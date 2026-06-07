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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.layout
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.unit.Constraints
import org.junit.Assert.assertNotEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.filter.DateFilter

/**
 * Reproduces the "cards only appear while swiping" regression. The generic
 * [DayCarouselTest] passes because it feeds the carousel a stand-in grid of
 * `GridCells.Fixed(1)` rows with a FIXED 50.dp height: those compose (and report
 * a height) even when measured at zero width. The real [FilmsGrid] holds 2:3
 * aspect-ratio poster cards in a multi-column grid whose item height is derived
 * from the cell WIDTH, so a column measured at width 0 yields zero-size cards
 * that never display.
 *
 * The carousel's three-column strip Row inherited the parent Box's single-width
 * max constraint, so the first (prev) column ate the whole budget and the centre
 * + next columns were measured at width 0 — the centre column then showed nothing
 * at rest, and cards only flashed mid-drag as a neighbour slid in.
 *
 * This test mirrors the real path (a `GridCells.Fixed(2)` grid of aspect-ratio
 * cards) and asserts (a) the centre grid is measured with non-zero width and
 * (b) the centre card is actually DISPLAYED at rest — non-zero on-screen size,
 * no scroll.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DayCarouselRealGridTest {

    @get:Rule
    val compose = createComposeRule()

    private fun mount(onCenterConstraints: (Constraints) -> Unit = {}) {
        compose.setContent {
            var current by mutableStateOf(DateFilter.Today)
            val sharedScroll: LazyGridState = rememberLazyGridState()
            DayCarousel(
                current = current,
                sharedScroll = sharedScroll,
                onCommitDay = { current = it },
                modifier = Modifier.testTag("dayCarousel"),
            ) { day, state, columnModifier ->
                val isCenter = day == current
                // Probe the constraints the centre grid is measured against.
                val probe = if (isCenter) {
                    Modifier.layout { measurable, constraints ->
                        onCenterConstraints(constraints)
                        val p = measurable.measure(constraints)
                        layout(p.width, p.height) { p.place(0, 0) }
                    }
                } else Modifier
                LazyVerticalGrid(
                    columns = GridCells.Fixed(2),
                    state = state,
                    modifier = columnModifier
                        .fillMaxSize()
                        .then(probe)
                        .then(if (isCenter) Modifier.testTag("centerGrid") else Modifier),
                ) {
                    items((0 until 40).toList()) { i ->
                        // No fixed height: a 2:3 poster card like the real FilmCard.
                        Box(
                            Modifier
                                .fillMaxWidth()
                                .aspectRatio(2f / 3f)
                                .background(Color.Gray)
                                .testTag(if (isCenter) "centerCard$i" else "card$i"),
                        )
                    }
                }
            }
        }
    }

    @Test
    fun centerGridIsMeasuredWithNonZeroWidth() {
        var captured: Constraints? = null
        mount(onCenterConstraints = { captured = it })
        compose.waitForIdle()
        val c = captured
        requireNotNull(c) { "centre grid was never measured" }
        // The proven root cause: the three-column Row inherited the parent Box's
        // single-width max constraint, so the first column consumed all of it and
        // the centre column was measured at maxWidth = 0 → its aspect-ratio cards
        // get zero size and never display at rest.
        assertNotEquals(
            "centre grid was measured at width 0 (Row not 3× wide) → centre cards never show at rest",
            0,
            c.maxWidth,
        )
    }

    @Test
    fun centerCardIsDisplayedAtRest() {
        mount()
        compose.waitForIdle()
        // The first centre card must be on-screen with a non-zero size at rest,
        // with no scroll. assertIsDisplayed catches both "not composed" and
        // "composed but zero-size".
        compose.onNodeWithTag("centerCard0").assertIsDisplayed()
    }
}
