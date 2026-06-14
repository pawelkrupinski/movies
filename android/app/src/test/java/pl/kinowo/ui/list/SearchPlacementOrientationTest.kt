package pl.kinowo.ui.list

import android.content.res.Configuration
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.ui.TopBarLayout
import pl.kinowo.ui.common.viewportWidthDp

/**
 * Off-device (Robolectric) check that SEARCH placement follows the LIVE viewport
 * width: a phone rotated to landscape hosts search inline on the top bar, the
 * same way iOS does, while in portrait it stays the floating bottom pill.
 *
 * This keys off [viewportWidthDp] (the live `screenWidthDp`), NOT the portrait
 * width [layoutWidthDp][pl.kinowo.ui.common.layoutWidthDp]. Before that change
 * the decision read the rotation-invariant `smallestScreenWidthDp` (here 360),
 * which never crosses the 600 threshold on a phone — so landscape kept the
 * floating pill and the landscape assertion below fails in that world.
 *
 * Note: the chip/rating metrics intentionally stay locked to the portrait width
 * (proven by `ShowtimeChipOrientationTest`); only search placement and the
 * poster-grid columns follow the live width.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class SearchPlacementOrientationTest {

    @get:Rule
    val compose = createComposeRule()

    /** A phone whose portrait width is 360 dp, rendered in the given orientation.
     *  Landscape widens the live `screenWidthDp` to 800 (the long edge) but keeps
     *  `smallestScreenWidthDp` at 360 — exactly what a real device reports. */
    @Composable
    private fun AsPhone(landscape: Boolean, content: @Composable () -> Unit) {
        val config = Configuration(LocalConfiguration.current).apply {
            smallestScreenWidthDp = 360
            if (landscape) {
                orientation = Configuration.ORIENTATION_LANDSCAPE
                screenWidthDp = 800
                screenHeightDp = 360
            } else {
                orientation = Configuration.ORIENTATION_PORTRAIT
                screenWidthDp = 360
                screenHeightDp = 800
            }
        }
        CompositionLocalProvider(LocalConfiguration provides config, content = content)
    }

    /** Renders the production placement decision as a tag so the test can read it. */
    @Composable
    private fun PlacementTag() {
        val inline = TopBarLayout.searchInline(viewportWidthDp())
        Text(if (inline) "INLINE" else "FLOATING")
    }

    @Test
    fun landscapePhoneHostsSearchInline() {
        compose.setContent { AsPhone(landscape = true) { PlacementTag() } }
        compose.onNodeWithText("INLINE").assertExists()
    }

    @Test
    fun portraitPhoneKeepsSearchFloating() {
        compose.setContent { AsPhone(landscape = false) { PlacementTag() } }
        compose.onNodeWithText("FLOATING").assertExists()
    }
}
