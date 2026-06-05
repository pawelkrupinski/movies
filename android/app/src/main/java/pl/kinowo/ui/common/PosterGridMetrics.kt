package pl.kinowo.ui.common

/**
 * Geometry of the poster grid, shared by production (`ListScreen`) and the
 * layout tests so the "card column width" formula lives in exactly one place.
 *
 * The portrait grid is `GridCells.Fixed(2)` with 12 dp content padding each
 * side and a 12 dp gap between the columns (36 dp total), and each [FilmCard]
 * takes another 24 dp off via its inner `Column` padding. The card column width
 * that falls out is also the binding case for the two-chips-per-row rule
 * (pinned by `ShowtimeChipFitTest`): it's the narrowest a column is allowed to
 * be, so landscape's adaptive grid uses [cardColumnDp] as its minimum and never
 * produces a column too tight to fit two chips. Mirrors iOS `ShowtimePillMetrics`.
 */
object PosterGridMetrics {
    /** Grid content padding (start + end) + the inter-column gap. */
    const val PortraitGuttersDp = 36

    /** [FilmCard]'s inner `Column` padding, both sides combined. */
    const val CardInnerPaddingDp = 24

    /** Width (dp) of one card column on the portrait two-column grid at the
     *  given layout width — and the minimum width a landscape column may take. */
    fun cardColumnDp(layoutWidthDp: Int): Int = (layoutWidthDp - PortraitGuttersDp) / 2

    /** The showings/chip content width inside one card column. */
    fun cardContentDp(layoutWidthDp: Int): Int = cardColumnDp(layoutWidthDp) - CardInnerPaddingDp
}
