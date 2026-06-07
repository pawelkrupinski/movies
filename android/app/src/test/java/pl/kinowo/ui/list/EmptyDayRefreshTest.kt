package pl.kinowo.ui.list

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.test.swipeDown
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * A "Brak repertuaru" day still needs pull-to-refresh: when a day's grid is
 * empty, the user's only recovery is to pull down and re-fetch. The empty
 * state must therefore be over-draggable so the enclosing [PullToRefreshBox]
 * sees the gesture. A bare Column dispatches no nested-scroll deltas, so this
 * fails until the empty state is made vertically scrollable.
 */
@OptIn(ExperimentalMaterial3Api::class)
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class EmptyDayRefreshTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun pullToRefreshFiresOnEmptyDay() {
        var refreshed = false
        compose.setContent {
            PullToRefreshBox(
                isRefreshing = false,
                onRefresh = { refreshed = true },
                modifier = Modifier.fillMaxSize().testTag("refresh"),
            ) {
                // The exact production path a "Brak repertuaru" day takes: an
                // empty film list renders the centred empty state.
                FilmsGrid(
                    films = emptyList(),
                    state = rememberLazyGridState(),
                    bottomInset = 0.dp,
                    showCinemaHeaders = false,
                    onOpen = {},
                    onHide = {},
                )
            }
        }

        compose.onNodeWithTag("refresh").performTouchInput { swipeDown() }
        compose.waitForIdle()

        compose.runOnIdle {
            assertTrue("pull-to-refresh must fire on an empty 'Brak repertuaru' day", refreshed)
        }
    }
}
