package pl.kinowo.ui.list

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.material3.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.filter.DateFilter

/**
 * The date pills must show their labels in full on the narrowest phone we
 * target. A Galaxy S24 is 1080 px at density 3.0 → 360 dp wide. The three dated
 * pills used to share the leftover row width via `weight`, but the weighted
 * share landed under "Jutro" / "7 dni"'s intrinsic width at 14 sp, so the labels
 * clipped beside the 🎬 mark, "Wszystkie", and the Filtry button.
 *
 * Renders the real [DateBar] at 360 dp and asserts (a) every date label gets at
 * least its intrinsic text width — i.e. is not horizontally clipped — and (b)
 * the bar fits, with the Filtry button still fully on-screen. NATIVE graphics
 * gives real text metrics, so the widths are real, not phantom zero-width stubs;
 * [clipsWhenFarTooNarrow] is the control proving the measurement bites.
 *
 * `hasVisualOverflow` is deliberately NOT the signal: it also trips on vertical
 * overflow (the pill's line box vs its vertical padding), so it reads `true`
 * even for labels that fit horizontally. Comparing the laid-out width against
 * the text's intrinsic width isolates horizontal truncation.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class DayPillFitTest {

    @get:Rule
    val compose = createComposeRule()

    @Composable
    private fun BarAtWidth(widthDp: Int) {
        MaterialTheme {
            Box(Modifier.width(widthDp.dp).testTag("bar")) {
                DateBar(
                    wide = false,
                    highlighted = DateFilter.Today,
                    filtersActive = false,
                    search = "",
                    onSearch = {},
                    onSelect = {},
                    onOpenFilters = {},
                )
            }
        }
    }

    private fun layoutOf(label: String): TextLayoutResult {
        val node = compose.onNodeWithText(label).fetchSemanticsNode()
        val results = mutableListOf<TextLayoutResult>()
        node.config[SemanticsActions.GetTextLayoutResult].action?.invoke(results)
        return results.first()
    }

    /** A label is horizontally clipped when its laid-out width is narrower than
     *  the width the text needs on one line (its max intrinsic width). */
    private fun isClipped(label: String): Boolean {
        val layout = layoutOf(label)
        return layout.size.width + 0.5f < layout.multiParagraph.maxIntrinsicWidth
    }

    @Test
    fun dayLabelsFitOnNarrowestPhone() {
        compose.setContent { BarAtWidth(360) }
        for (preset in DateFilter.presets) {
            assertTrue(
                "Date pill '${preset.label}' is horizontally clipped at 360dp " +
                    "(laid-out ${layoutOf(preset.label).size.width}px < " +
                    "intrinsic ${layoutOf(preset.label).multiParagraph.maxIntrinsicWidth}px)",
                !isClipped(preset.label),
            )
            assertEquals(
                "Date pill '${preset.label}' wrapped onto 2 lines at 360dp",
                1, layoutOf(preset.label).lineCount,
            )
        }
        // The whole bar must fit too: the Filtry button can't be shoved off the
        // right edge by intrinsic-width pills that overran the row.
        val barRight = compose.onNodeWithTag("bar").fetchSemanticsNode().boundsInRoot.right
        val filtry = compose.onNodeWithContentDescription("Filtry").fetchSemanticsNode().boundsInRoot
        assertTrue(
            "Filtry button overflows the bar (right edge ${filtry.right}px > bar ${barRight}px)",
            filtry.right <= barRight + 0.5f,
        )
    }

    @Test
    fun clipsWhenFarTooNarrow_provesMeasurementIsReal() {
        // 120 dp can't hold the 🎬 mark, four pills, and the Filtry button. If
        // Robolectric were handing back stub zero-width text the labels would
        // never clip and this would fail — so it guards that the positive test
        // isn't passing vacuously on phantom measurements.
        compose.setContent { BarAtWidth(120) }
        assertTrue(
            "expected at least one date label to clip in a 120dp bar",
            DateFilter.presets.any { isClipped(it.label) },
        )
    }
}
