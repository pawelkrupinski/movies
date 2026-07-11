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
 * The date pills on a narrow (portrait) top bar must do two things on the
 * narrowest phone we target — a Galaxy S24, 360 dp wide:
 *
 *  1. [dayPillsFillRemainingWidth] — the three dated pills (Dziś / Jutro /
 *     7 dni) take `weight` and so spread to fill ALL the width left between the
 *     🎬 mark and the Filtry button: no slack pools between the last pill and
 *     Filtry, and Filtry sits flush against the right edge. This fails the
 *     moment the pills stop filling (e.g. reverting them to intrinsic width).
 *
 *  2. [dayLabelsFitOnNarrowestPhone] — even after filling, the share each pill
 *     gets is wide enough to render its label at 14 sp without horizontal
 *     clipping. Comparing the laid-out width against the text's intrinsic width
 *     isolates horizontal truncation; `hasVisualOverflow` is NOT used because it
 *     also trips on the pill's vertical padding.
 *
 * Robolectric's default host is only 320 dp wide and clamps `Box(width(360.dp))`
 * down to it, so the bar is rendered inside a 1280 dp host (the `qualifiers`)
 * where the 360 dp box is honoured exactly — otherwise these would silently test
 * a 320 dp bar. NATIVE graphics gives real text metrics (no phantom zero-width
 * stubs); [clipsWhenFarTooNarrow] is the control proving the measurement bites.
 *
 * mdpi (density 1) keeps 1 px == 1 dp, so the pixel tolerances below read as dp.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "w1280dp-h800dp")
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
                    selectedCountryCode = "PL",
                    onSelectCountry = {},
                    onSearch = {},
                    onSelect = {},
                    onOpenFilters = {},
                )
            }
        }
    }

    private fun rightOf(label: String) = compose.onNodeWithText(label).fetchSemanticsNode().boundsInRoot.right
    private fun widthOf(label: String) = compose.onNodeWithText(label).fetchSemanticsNode().boundsInRoot.width

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
    fun dayPillsFillRemainingWidth() {
        compose.setContent { BarAtWidth(360) }

        val barRight = compose.onNodeWithTag("bar").fetchSemanticsNode().boundsInRoot.right
        val filtry = compose.onNodeWithContentDescription("Filtry").fetchSemanticsNode().boundsInRoot

        // "Wszystkie" is the last pill before Filtry. If the dated pills fill the
        // row, the only thing between it and Filtry is the 4dp arrangement gap —
        // not a pool of leftover width. A generous 12px ceiling still catches a
        // regression to intrinsic-width pills, which would dump ~40px of slack
        // here (or, with a trailing spacer, push Filtry off the right edge).
        val gapBeforeFiltry = filtry.left - rightOf("Wszystkie")
        assertTrue(
            "Date pills don't fill the row — ${gapBeforeFiltry}px of slack sits " +
                "between 'Wszystkie' and Filtry (expected ~4dp)",
            gapBeforeFiltry <= 12f,
        )

        // …and Filtry is flush to the right edge (just the 4dp end padding), so
        // the filled pills haven't instead overflowed and shoved it off-screen.
        val trailingSlack = barRight - filtry.right
        assertTrue(
            "Filtry isn't flush right — ${trailingSlack}px of slack after it " +
                "(expected ~4dp end padding)",
            trailingSlack in 0f..12f,
        )

        // The three dated pills share the leftover equally (the segmented look),
        // each wider than the snug intrinsic pill it would be if it stopped
        // filling — so the fill is real, not a coincidence of the geometry above.
        val widths = listOf("Dziś", "Jutro", "7 dni").map { widthOf(it) }
        widths.zipWithNext().forEach { (a, b) ->
            assertEquals("dated pills should be equal width (segmented control)", a, b, 1.5f)
        }
        val snugWidest = layoutOf("Jutro").multiParagraph.maxIntrinsicWidth + 2 * 6f // 6dp pad/side
        assertTrue(
            "dated pills (${widths[0]}px) aren't stretched past their snug " +
                "content width (${snugWidest}px) — they're not filling",
            widths[0] > snugWidest + 3f,
        )
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
    }

    @Test
    fun clipsWhenFarTooNarrow_provesMeasurementIsReal() {
        // 120 dp can't hold the 🎬 mark, four pills, and the Filtry button. If
        // Robolectric were handing back stub zero-width text the labels would
        // never clip and this would fail — so it guards that the positive tests
        // aren't passing vacuously on phantom measurements.
        compose.setContent { BarAtWidth(120) }
        assertTrue(
            "expected at least one date label to clip in a 120dp bar",
            DateFilter.presets.any { isClipped(it.label) },
        )
    }
}
