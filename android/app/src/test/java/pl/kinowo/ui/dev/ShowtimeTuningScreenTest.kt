package pl.kinowo.ui.dev

import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollTo
import androidx.compose.ui.test.performSemanticsAction
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Off-device (Robolectric) test that drives the real [ShowtimeTuningScreen] —
 * the in-CI Android counterpart of iOS `CardSpacingTuningUITests`. Where the
 * iOS XCUITest launches the app on a simulator and drags the sliders, this
 * renders the composable, taps the pager's page tabs, and drives the sliders
 * via their `SetProgress` semantics, asserting the REAL preview behind the
 * sheet responds — so a slider wired to the wrong style field, or a pager that
 * won't switch pages, fails here. The per-style wiring itself is pinned at a
 * finer grain by [pl.kinowo.ui.common.TuningStylesTest] /
 * [pl.kinowo.ui.common.CardSpacingStyleTest]; this guards the tuning SCREEN.
 *
 * NATIVE graphics for real text metrics; runs on the JVM via
 * `./gradlew app:testDebugUnitTest` — no emulator.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class ShowtimeTuningScreenTest {

    @get:Rule
    val compose = createComposeRule()

    /** Tallest rendered node carrying [text] (the detail title shows in both the
     *  app bar at a fixed size and the body at the tunable size — we want the
     *  body one, which is the taller). */
    private fun maxTextHeight(text: String): Float {
        val nodes = compose.onAllNodesWithText(text, useUnmergedTree = true)
        var max = 0f
        for (i in 0 until nodes.fetchSemanticsNodes().size) {
            val b = nodes[i].getUnclippedBoundsInRoot()
            max = maxOf(max, (b.bottom - b.top).value)
        }
        return max
    }

    private fun switchTo(page: TuningPage) {
        compose.onNodeWithTag(TuningTags.pageTab(page)).performClick()
        compose.waitForIdle()
    }

    private fun setSlider(tag: String, value: Float) {
        compose.onNodeWithTag(tag).performSemanticsAction(SemanticsActions.SetProgress) { it(value) }
        compose.waitForIdle()
    }

    private fun waitForText(text: String) =
        compose.waitUntil(5_000) { compose.onAllNodesWithText(text).fetchSemanticsNodes().isNotEmpty() }

    @Test
    fun pagerExposesAllThreePages() {
        compose.setContent { KinowoTheme { ShowtimeTuningScreen() } }
        // fetchSemanticsNode() throws if the tab node is absent or non-unique.
        for (p in TuningPage.entries) {
            compose.onNodeWithTag(TuningTags.pageTab(p)).fetchSemanticsNode()
        }
    }

    @Test
    fun kinaPageFontSliderGrowsCinemaHeader() {
        compose.setContent { KinowoTheme { ShowtimeTuningScreen() } }
        switchTo(TuningPage.Kina)
        waitForText("Kinepolis") // first cinema section header (pillName)

        val before = maxTextHeight("Kinepolis")
        setSlider(TuningTags.CinemaHeaderFontSlider, 24f) // 15 → 24sp
        val after = maxTextHeight("Kinepolis")

        assertTrue("cinema header measured no height — metrics stubbed", before > 0f)
        assertTrue(
            "maxing the Kina-page font slider must grow the cinema header: before=$before after=$after",
            after - before > 3f,
        )
    }

    @Test
    fun filmPageFontSliderGrowsDetailTitle() {
        compose.setContent { KinowoTheme { ShowtimeTuningScreen() } }
        switchTo(TuningPage.Film)
        waitForText("Wszystkie przypadki") // the detail title

        val before = maxTextHeight("Wszystkie przypadki")
        setSlider(TuningTags.DetailTitleFontSlider, 34f) // 20 → 34sp
        val after = maxTextHeight("Wszystkie przypadki")

        assertTrue("detail title measured no height — metrics stubbed", before > 0f)
        assertTrue(
            "maxing the Film-page title slider must grow the detail title: before=$before after=$after",
            after - before > 4f,
        )
    }

    /** The viewport/resolution readout lives at the TOP of the scrolling
     *  controls, not in the pinned header — so scrolling the controls down
     *  carries it off the top instead of letting it permanently eat header
     *  space. Scroll to the last card-page control and assert the readout
     *  moved up; if it were still pinned, its top wouldn't budge. */
    @Test
    fun resolutionReadoutScrollsAwayWithTheControls() {
        compose.setContent { KinowoTheme { ShowtimeTuningScreen() } }

        val topBefore = compose.onNodeWithTag(TuningTags.ResolutionReadout)
            .getUnclippedBoundsInRoot().top.value
        // "Zaokrąglenie" is the last group on the card page, well below the fold.
        compose.onNodeWithText("Zaokrąglenie").performScrollTo()
        compose.waitForIdle()
        val topAfter = compose.onNodeWithTag(TuningTags.ResolutionReadout)
            .getUnclippedBoundsInRoot().top.value

        assertTrue(
            "scrolling the controls must carry the resolution readout up and out " +
                "(it's pinned, not scrolling): top $topBefore → $topAfter",
            topBefore - topAfter > 40f,
        )
    }
}
