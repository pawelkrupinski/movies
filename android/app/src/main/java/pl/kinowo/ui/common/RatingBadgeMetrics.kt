package pl.kinowo.ui.common

/**
 * Sizes the rating pills relative to the viewport width.
 *
 * The base dimensions (11sp font, 6dp horizontal padding, no vertical padding,
 * 5dp corner) were tuned on the
 * Pixel 9a's ~411dp portrait width, so that width is the reference point: at it
 * the scale is exactly 1.0 and the pills render unchanged. Narrower phones
 * shrink the pills proportionally; wider screens grow them. The factor is
 * clamped so tiny configs stay legible and tablets/foldables don't balloon the
 * row.
 */
object RatingBadgeMetrics {
    /** The width (dp) the base pill sizes were tuned at — Pixel 9a portrait. */
    const val ReferenceWidthDp = 411f

    const val MinScale = 0.85f
    const val MaxScale = 1.4f

    /** Base font (sp) at the reference width. The actual rendered size is this
     *  times [scale]. */
    const val BaseFontSp = 11f

    fun scale(screenWidthDp: Int): Float =
        (screenWidthDp / ReferenceWidthDp).coerceIn(MinScale, MaxScale)
}
