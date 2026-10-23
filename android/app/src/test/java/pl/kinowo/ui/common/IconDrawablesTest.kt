package pl.kinowo.ui.common

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.size
import androidx.compose.material3.Icon
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toPixelMap
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.test.captureToImage
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.R

/**
 * The app's icons are vector drawables (`res/drawable/ic_*.xml`, the Material
 * Icons glyphs), not `material-icons-extended`: that artifact (and the
 * `material-icons-core` only it pulled in — material3 1.4 no longer does) was
 * a 36 MB AAR (37 MB of classes) for 13 icons, dexed into every debug build. When the
 * drawables replaced it they rendered pixel-identical to the old ImageVectors
 * (96 dp, LTR and RTL).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class IconDrawablesTest {

    @get:Rule
    val compose = createComposeRule()

    @Test
    fun theMaterialIconsLibraryIsNotOnTheClasspath() {
        val found = runCatching { Class.forName("androidx.compose.material.icons.Icons") }.isSuccess
        assertFalse("icons are res/drawable/ic_*.xml; material-icons-* must stay out of the build", found)
    }

    @Test
    fun everyIconDrawableRendersItsGlyph() {
        val icons = listOf(
            R.drawable.ic_arrow_back, R.drawable.ic_close, R.drawable.ic_expand_less, R.drawable.ic_expand_more,
            R.drawable.ic_filter_list, R.drawable.ic_keyboard_arrow_right, R.drawable.ic_link, R.drawable.ic_movie,
            R.drawable.ic_my_location, R.drawable.ic_open_in_new, R.drawable.ic_search, R.drawable.ic_share,
            R.drawable.ic_swipe,
        )
        var shown by mutableIntStateOf(icons.first())
        compose.setContent {
            Box(Modifier.size(48.dp).background(Color.White).testTag("icon")) {
                Icon(painterResource(shown), contentDescription = null, modifier = Modifier.size(48.dp), tint = Color.Black)
            }
        }
        val blank = icons.filter { icon ->
            shown = icon
            compose.waitForIdle()
            val pixels = compose.onNodeWithTag("icon").captureToImage().toPixelMap()
            val inked = (0 until pixels.width).sumOf { x -> (0 until pixels.height).count { y -> pixels[x, y].red < 0.5f } }
            inked < pixels.width * pixels.height / 50
        }
        assertEquals("icons drawing (almost) nothing", emptyList<Int>(), blank)
    }
}
