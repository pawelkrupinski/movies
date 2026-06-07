package pl.kinowo.ui.list

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.grid.rememberLazyGridState
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.day
import pl.kinowo.TestData.film
import pl.kinowo.TestData.slot
import pl.kinowo.filter.DateFilter

/**
 * Regression for the empty-state ("Brak repertuaru.") being jammed against the
 * RIGHT screen edge and clipped on a day whose carousel neighbour has content.
 *
 * Mirrors the carousel's layout: two fixed-width columns in a Row that's WIDER
 * than the viewport, with the on-screen (left) column showing the EMPTY
 * [FilmsGrid] path and the off-screen (right) neighbour full of cards. Before
 * the fix, the empty `Column(fillMaxSize())` ignored its column-width box and
 * filled the whole 2×-wide Row, so its centred text landed at the Row's centre —
 * i.e. the on-screen column's RIGHT edge — clipped. After the fix the empty
 * state fills only its own column and centres within it.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class EmptyStateCarouselTest {

    @get:Rule
    val compose = createComposeRule()

    private val neighbour = listOf(
        film("Neighbour Film", listOf(day("2026-06-08", listOf(cinema("Muza", listOf(slot("18:00"))))))),
    )

    private fun mount() {
        compose.setContent {
            // One column == the full viewport width, like the carousel's per-day column.
            val columnWidth = LocalConfiguration.current.screenWidthDp.dp
            Row(Modifier.fillMaxSize()) {
                // On-screen (centre) column: EMPTY repertoire → empty state.
                FilmsGrid(
                    films = emptyList(),
                    state = rememberLazyGridState(),
                    bottomInset = 0.dp,
                    showCinemaHeaders = false,
                    onOpen = {},
                    onHide = {},
                    modifier = Modifier.width(columnWidth).fillMaxHeight().testTag("centerColumn"),
                )
                // Off-screen neighbour column: full of cards (pushes the bug).
                FilmsGrid(
                    films = neighbour,
                    state = rememberLazyGridState(),
                    bottomInset = 0.dp,
                    showCinemaHeaders = false,
                    onOpen = {},
                    onHide = {},
                    modifier = Modifier.width(columnWidth).fillMaxHeight().background(Color.Gray),
                )
            }
        }
    }

    @Test
    fun emptyStateCentresWithinItsOwnColumnNotAtTheScreenEdge() {
        mount()
        compose.waitForIdle()

        val message = compose.onNodeWithText("Brak repertuaru.")
        message.assertIsDisplayed()

        val column = compose.onNodeWithTag("centerColumn").getUnclippedBoundsInRoot()
        val msg = message.getUnclippedBoundsInRoot()
        val columnCentre = (column.left + column.right) / 2
        val msgCentre = (msg.left + msg.right) / 2
        val columnWidth = column.right - column.left

        // The message centre must sit near the on-screen column's centre — NOT
        // pushed toward / past its right edge (where the bug placed it: the
        // centre of the whole 2×-wide Row). Tolerance: a tenth of a column.
        val tolerance = columnWidth * 0.1f
        assertTrue(
            "empty state centre ${msgCentre} is off the column centre ${columnCentre} " +
                "(column [${column.left}, ${column.right}]) — clipped/edge-jammed",
            (msgCentre - columnCentre).value.let { kotlin.math.abs(it) } <= tolerance.value,
        )
        // And it must not bleed past the column's right edge (clipping).
        assertTrue(
            "empty state right ${msg.right} exceeds column right ${column.right} — clipped",
            msg.right.value <= column.right.value + 1f,
        )
    }
}
