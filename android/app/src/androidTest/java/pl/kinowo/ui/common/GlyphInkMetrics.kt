package pl.kinowo.ui.common

import android.graphics.Bitmap
import kotlin.math.abs

/**
 * Pixel-measures the vertical whitespace around glyph ink in a captured pill
 * bitmap, for the on-emulator [RatingPillVisualPaddingTest]. The pill is a single
 * fill colour under higher-contrast text; this finds the rows that carry ink and
 * reports the empty band of fill above and below it — the gap the eye reads as
 * padding.
 */
object GlyphInkMetrics {

    data class Band(val topGap: Int, val bottomGap: Int, val inkHeight: Int) {
        /** Total empty fill (above + below) relative to the glyph height. */
        val gapRatio: Float get() = (topGap + bottomGap).toFloat() / inkHeight
    }

    /** Sum of per-channel deltas — cheap and good enough to separate ink from
     *  fill when the two are well apart. */
    fun distance(a: Int, b: Int): Int {
        val dr = abs(((a shr 16) and 0xFF) - ((b shr 16) and 0xFF))
        val dg = abs(((a shr 8) and 0xFF) - ((b shr 8) and 0xFF))
        val db = abs((a and 0xFF) - (b and 0xFF))
        return dr + dg + db
    }

    /**
     * Scan [xRange] (kept clear of rounded corners) for the rows whose pixels
     * differ from the [bg] fill beyond [threshold] — the glyph ink — and return
     * the empty band above and below it.
     */
    fun measure(bmp: Bitmap, bg: Int, xRange: IntRange, threshold: Int): Band {
        var first = -1
        var last = -1
        for (y in 0 until bmp.height) {
            var ink = false
            for (x in xRange) {
                if (distance(bmp.getPixel(x, y), bg) > threshold) {
                    ink = true
                    break
                }
            }
            if (ink) {
                if (first < 0) first = y
                last = y
            }
        }
        require(first in 0 until bmp.height && last >= first) {
            "No glyph ink detected in ${bmp.width}x${bmp.height} over x=$xRange — sampling/threshold is off."
        }
        return Band(topGap = first, bottomGap = bmp.height - 1 - last, inkHeight = last - first + 1)
    }
}
