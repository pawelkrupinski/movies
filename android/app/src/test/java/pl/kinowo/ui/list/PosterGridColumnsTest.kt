package pl.kinowo.ui.list

import androidx.compose.foundation.lazy.grid.GridCells
import androidx.compose.ui.unit.dp
import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.ui.common.PosterGridMetrics

/**
 * Pure tests for the poster grid's column decision. Portrait is always two fixed
 * columns; landscape fits as many adaptive columns as the width allows, each no
 * narrower than a portrait card — that minimum is what keeps two chips on a row
 * in landscape (the rule is proven at the portrait card width by
 * `ShowtimeChipFitTest`).
 */
class PosterGridColumnsTest {

    @Test
    fun portraitIsAlwaysTwoFixedColumns() {
        assertEquals(GridCells.Fixed(2), posterGridColumns(landscape = false, layoutWidthDp = 360))
        assertEquals(GridCells.Fixed(2), posterGridColumns(landscape = false, layoutWidthDp = 800))
    }

    @Test
    fun landscapeColumnMinimumIsOnePortraitCardWide() {
        // (360 - 36) / 2 = 162 dp — the width of one portrait card column.
        assertEquals(GridCells.Adaptive(162.dp), posterGridColumns(landscape = true, layoutWidthDp = 360))
        // (411 - 36) / 2 = 187 dp.
        assertEquals(GridCells.Adaptive(187.dp), posterGridColumns(landscape = true, layoutWidthDp = 411))
    }

    @Test
    fun cardColumnAndContentGeometry() {
        assertEquals(162, PosterGridMetrics.cardColumnDp(360))
        assertEquals(138, PosterGridMetrics.cardContentDp(360))
    }
}
