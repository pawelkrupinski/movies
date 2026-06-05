package pl.kinowo.ui.common

/**
 * Sizes the showtime chips relative to the viewport width, so a chip's fonts /
 * padding / gaps grow on wider screens — the sibling of [RatingBadgeMetrics] for
 * the blue time chips.
 *
 * The baseline [ShowtimeChipStyle] is tuned at the **360 dp floor** — the
 * narrowest portrait width we support and the binding case for the
 * two-chips-per-row rule — so 360 dp is the reference (scale 1.0) and the factor
 * only GROWS for wider screens. That direction is what keeps two-per-row safe:
 * the 2-column portrait card column is `w/2 - 42` dp wide, which grows at
 * 0.5 dp per dp of screen, while chips scaled by `w/360` grow at
 * `twoChipWidth(360)/360 <= 138/360 ≈ 0.38` dp per dp. The column always pulls
 * ahead, so two-per-row that holds at 360 dp holds at every width >= 360
 * (pinned across widths by `ShowtimeChipFitTest`). Landscape uses
 * `GridCells.Adaptive(170.dp)` — columns wider than the 138 dp portrait one — so
 * it's safe there too. Capped so tablets / foldables don't balloon the chips.
 */
object ShowtimeChipMetrics {
    /** The width (dp) the baseline chip values are tuned at — the 360 dp floor. */
    const val ReferenceWidthDp = 360f

    /** Never shrink below the baseline (360 dp is the narrowest we support); the
     *  cap keeps very wide screens from over-growing the chips. */
    const val MinScale = 1.0f
    const val MaxScale = 1.4f

    fun scale(screenWidthDp: Int): Float =
        (screenWidthDp / ReferenceWidthDp).coerceIn(MinScale, MaxScale)
}
