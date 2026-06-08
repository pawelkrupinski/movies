package pl.kinowo.ui.list

import androidx.activity.ComponentActivity
import androidx.activity.compose.BackHandler
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.Text
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode

/**
 * The Filtry sheet must close on a system-back press (the user's "wyjść"
 * gesture), not just on an outside-tap or downward swipe. ListScreen layers an
 * explicit [BackHandler] over the sheet's `showFilters` state because the bare
 * [ModalBottomSheet] does not intercept back in this app's window config —
 * [backWithoutHandlerLeavesSheetOpen] documents that gap, and
 * [backWithHandlerClosesSheet] proves the handler closes it.
 *
 * Both assert on the hoisted `showFilters` boolean rather than nodes inside the
 * sheet's separate window, which Robolectric doesn't reliably traverse (the
 * same reason FiltersSheetContent is extracted for FiltersSheetOrderTest).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class FilterSheetBackDismissTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    @OptIn(ExperimentalMaterial3Api::class)
    @Composable
    private fun SheetHarness(withBackHandler: Boolean, show: () -> Boolean, onShow: (Boolean) -> Unit) {
        if (withBackHandler) {
            BackHandler(enabled = show()) { onShow(false) }
        }
        if (show()) {
            val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)
            ModalBottomSheet(onDismissRequest = { onShow(false) }, sheetState = sheetState) {
                Text("filtry-body")
            }
        }
    }

    private fun pressBack() {
        compose.runOnUiThread {
            compose.activity.onBackPressedDispatcher.onBackPressed()
        }
        compose.waitForIdle()
    }

    @Test
    fun backWithoutHandlerLeavesSheetOpen() {
        var showFilters by mutableStateOf(true)
        compose.setContent {
            SheetHarness(withBackHandler = false, show = { showFilters }, onShow = { showFilters = it })
        }
        compose.waitForIdle()

        pressBack()
        // The bare ModalBottomSheet doesn't flip our state on back — that's the
        // bug the user hit ("wyjść" did nothing).
        assertTrue("bare ModalBottomSheet unexpectedly closed on back", showFilters)
    }

    @Test
    fun backWithHandlerClosesSheet() {
        var showFilters by mutableStateOf(true)
        compose.setContent {
            SheetHarness(withBackHandler = true, show = { showFilters }, onShow = { showFilters = it })
        }
        compose.waitForIdle()

        pressBack()
        assertFalse("BackHandler must close the Filtry sheet on a back press", showFilters)
    }
}
