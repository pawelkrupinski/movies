package pl.kinowo.ui.list

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.foundation.lazy.grid.LazyVerticalGrid
import androidx.compose.foundation.lazy.grid.items
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.material3.Text
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode

/**
 * Off-device (Robolectric) test for the at-top day switch.
 *
 * The centre column shares one [LazyGridState] across days and the grid restores
 * position BY KEY (films are keyed by title). If the film you're at the top of
 * also appears LOWER in the next day, a plain day swap restores the new day to
 * that film's index — the new day lands scrolled down, and the follow-up
 * [ScrollToTopOnChange] then animates back up: a "synthetic scroll" the user
 * never asked for when they were already at the top.
 *
 * [selectDayKeepingTopFlat] pins an at-top switch to index 0 in the same layout
 * pass, so the new day lays out flat and there is nothing for the roll-to-top to
 * animate. (The remap is real and reproduces here — see [rawSwitchRestoresByKey],
 * which is exactly the bug this guards against.)
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class DaySwitchScrollTest {

    @get:Rule
    val compose = createComposeRule()

    // Day A's top item ("shared") also lives in day B, but at index 2 — the exact
    // shape that makes a key-based restore land the new day off the top.
    private val dayA = listOf("shared", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11")
    private val dayB = listOf("b0", "b1", "shared", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11")

    /** Render the shared grid for the current day; [switch] flips to day B. */
    private fun setUpGrid(useHelper: Boolean): Pair<() -> LazyGridState, () -> Unit> {
        lateinit var state: LazyGridState
        lateinit var switch: () -> Unit
        compose.setContent {
            state = rememberLazyGridState()
            var onA by remember { mutableStateOf(true) }
            switch = if (useHelper) {
                { selectDayKeepingTopFlat(state) { onA = false } }
            } else {
                { onA = false }
            }
            LazyVerticalGrid(
                columns = GridCells.Fixed(2),
                state = state,
                modifier = Modifier.height(360.dp).fillMaxWidth(),
            ) {
                items(if (onA) dayA else dayB, key = { it }) { item ->
                    Box(Modifier.height(120.dp).fillMaxWidth()) { Text(item) }
                }
            }
        }
        return ({ state } to switch)
    }

    /**
     * Pins the bug: a raw day swap restores by key, landing day B at index 2 (the
     * shared film's position there) instead of the top. This is what produces the
     * unwanted scroll; [atTopDaySwitchLandsFlat] proves the helper suppresses it.
     */
    @Test
    fun rawSwitchRestoresByKey() {
        val (state, switch) = setUpGrid(useHelper = false)
        compose.waitForIdle()
        assertEquals(0, state().firstVisibleItemIndex)

        compose.runOnIdle { switch() }
        compose.waitForIdle()

        assertEquals("raw switch should restore the shared film by key", 2, state().firstVisibleItemIndex)
    }

    /**
     * The fix: switching days while at the top lands the new day flat at the top —
     * no jump to the shared film's index, so nothing for the roll-to-top to scroll.
     */
    @Test
    fun atTopDaySwitchLandsFlat() {
        val (state, switch) = setUpGrid(useHelper = true)
        compose.waitForIdle()
        assertEquals(0, state().firstVisibleItemIndex)
        assertEquals(0, state().firstVisibleItemScrollOffset)

        compose.runOnIdle { switch() }
        compose.waitForIdle()

        assertEquals("an at-top day switch must land the new day at the top row", 0, state().firstVisibleItemIndex)
        assertEquals("an at-top day switch must not nudge the scroll offset", 0, state().firstVisibleItemScrollOffset)
    }
}
