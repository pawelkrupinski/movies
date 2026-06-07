package pl.kinowo.ui.list

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
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * Picking a different day must drop the user back to the top of the list
 * (the shared mechanism behind both the Filmy and Kina grids). Off-device
 * Robolectric check on [ScrollToTopOnChange]: scroll a grid down, change the
 * key, and the grid snaps back to item 0 — while leaving the position alone
 * when the key is unchanged.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class ScrollToTopOnChangeTest {

    @get:Rule
    val compose = createComposeRule()

    private fun renderGrid(initialKey: String): Pair<LazyGridState, (String) -> Unit> {
        var key by mutableStateOf(initialKey)
        lateinit var state: LazyGridState
        compose.setContent {
            state = rememberLazyGridState()
            ScrollToTopOnChange(state, key)
            LazyVerticalGrid(
                columns = GridCells.Fixed(1),
                state = state,
                modifier = Modifier.height(200.dp).testTag("grid"),
            ) {
                items((0 until 100).toList()) { Text("row $it", Modifier.height(50.dp)) }
            }
        }
        return state to { key = it }
    }

    @Test
    fun changingTheKeyScrollsBackToTop() {
        val (state, setKey) = renderGrid("today")
        compose.onNodeWithTag("grid").performScrollToIndex(40)
        compose.runOnIdle { assertEquals(40, state.firstVisibleItemIndex) }

        setKey("tomorrow")
        compose.waitForIdle()
        compose.runOnIdle { assertEquals(0, state.firstVisibleItemIndex) }
    }

    @Test
    fun unchangedKeyLeavesScrollAlone() {
        val (state, setKey) = renderGrid("today")
        compose.onNodeWithTag("grid").performScrollToIndex(40)
        compose.runOnIdle { assertEquals(40, state.firstVisibleItemIndex) }

        // Re-setting the same value is not a change — the list must stay put.
        setKey("today")
        compose.waitForIdle()
        compose.runOnIdle { assertEquals(40, state.firstVisibleItemIndex) }
    }
}
