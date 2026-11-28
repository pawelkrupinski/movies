package pl.kinowo.ui.dev

import android.graphics.Paint
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.model.Film
import pl.kinowo.ui.common.CardSpacingStyle
import pl.kinowo.ui.common.CinemaHeaderStyle
import pl.kinowo.ui.common.FilmDetailStyle
import pl.kinowo.ui.common.RatingBadgeMetrics
import pl.kinowo.ui.common.RatingPillStyle
import pl.kinowo.ui.common.ShowtimeChipStyle

// ── copy format + fit estimate ──────────────────────────────────────────────

internal fun oneDecimal(v: Float): String = String.format(java.util.Locale.US, "%.1f", v)
internal fun twoDecimal(v: Float): String = String.format(java.util.Locale.US, "%.2f", v)

/** Serialises the current page's values to the `key=value`-per-line paste
 *  format shared with iOS. */
internal fun copyTextFor(
    page: TuningPage,
    s: ShowtimeChipStyle,
    r: RatingPillStyle,
    c: CardSpacingStyle,
    h: CinemaHeaderStyle,
    f: FilmDetailStyle,
    screenWidthDp: Int,
): String = when (page) {
    TuningPage.Card -> copyText(s, r, c, screenWidthDp)
    TuningPage.Kina -> buildString {
        append("cinema-header: size=${oneDecimal(h.fontSize.value)} weight=${WeightOption.of(h.fontWeight).key}\n")
        append("cinema-gaps: top=${oneDecimal(h.headerTopGap.value)} headerToGrid=${oneDecimal(h.headerToGrid.value)} section=${oneDecimal(h.sectionSpacing.value)}")
    }
    TuningPage.Film -> buildString {
        append("film-layout: outer=${oneDecimal(f.outerSpacing.value)} header=${oneDecimal(f.headerSpacing.value)}\n")
        append("film-title: size=${oneDecimal(f.titleFontSize.value)} weight=${WeightOption.of(f.titleWeight).key} original=${oneDecimal(f.originalTitleFontSize.value)}\n")
        append("film-meta: label=${oneDecimal(f.sectionLabelFontSize.value)} body=${oneDecimal(f.sectionBodyFontSize.value)}\n")
        append("film-showings: header=${oneDecimal(f.showingsHeaderFontSize.value)}")
    }
}

/** The card page's serialisation: showtime + rating + card-gap lines. The
 *  rating lines carry the device-scale readout (Android-specific, since the
 *  pills size relative to viewport width). */
internal fun copyText(s: ShowtimeChipStyle, r: RatingPillStyle, c: CardSpacingStyle, screenWidthDp: Int): String {
    val scale = RatingBadgeMetrics.scale(screenWidthDp)
    val rendered = r.baseFontSize.value * scale
    return buildString {
        append("time: size=${oneDecimal(s.timeFontSize.value)} weight=${WeightOption.of(s.timeWeight).key}\n")
        append("format: size=${oneDecimal(s.formatFontSize.value)} weight=${WeightOption.of(s.formatWeight).key}\n")
        append("padding: h=${oneDecimal(s.horizontalInset.value)} v=${oneDecimal(s.verticalInset.value)}\n")
        append("gaps: internal=${oneDecimal(s.internalGap.value)} interPill=${oneDecimal(s.interPillGap.value)}\n")
        append("card-gaps: titleToMeta=${oneDecimal(c.titleToMeta.value)} metaToRatings=${oneDecimal(c.metaToRatings.value)} ratingsToShowings=${oneDecimal(c.ratingsToShowings.value)} dayToCinema=${oneDecimal(c.dayToCinema.value)} showingsBlock=${oneDecimal(c.showingsBlock.value)}\n")
        append("rating-base: size=${oneDecimal(r.baseFontSize.value)} labelW=${WeightOption.of(r.labelWeight).key} valueW=${WeightOption.of(r.valueWeight).key} solidW=${WeightOption.of(r.solidWeight).key}\n")
        append("rating-pad: h=${oneDecimal(r.hPad.value)} v=${oneDecimal(r.vPad.value)}\n")
        append("rating-shape: corner=${oneDecimal(r.corner.value)} interPill=${oneDecimal(r.interPillGap.value)}\n")
        append("rating-scale: x${twoDecimal(scale)} rendered=${oneDecimal(rendered)}")
    }
}

/**
 * Rough live estimate of whether two canonical chips still share one row in the
 * narrowest card column. APPROXIMATE — it measures text with an Android [Paint]
 * at the chosen sizes/weights; the rendered cards (and `ShowtimeChipFitTest`)
 * are the source of truth. Mirrors iOS `ShowtimeTuningScreen`'s `fitReadout`.
 */
internal object ChipFit {
    /** Narrowest Android card showings column: 360dp screen, `(360-36)/2 - 24`. */
    private const val CardWidthDp = (360 - 36) / 2 - 24 // = 138

    data class Result(val ok: Boolean, val readout: String)

    fun evaluate(s: ShowtimeChipStyle, density: androidx.compose.ui.unit.Density): Result {
        val a = chipWidthDp(s, "12:55", "2D DUB", density)
        val b = chipWidthDp(s, "22:55", "3D NAP", density)
        val two = a + s.interPillGap.value + b
        val margin = CardWidthDp - two
        return if (margin >= 0f) {
            Result(true, "✓ dwie pigułki w rzędzie (zapas ${oneDecimal(margin)} dp)")
        } else {
            Result(false, "✗ druga pigułka się zawija (brakuje ${oneDecimal(-margin)} dp)")
        }
    }

    private fun chipWidthDp(s: ShowtimeChipStyle, time: String, format: String, density: androidx.compose.ui.unit.Density): Float {
        val tw = textWidthDp(time, s.timeFontSize.value, s.timeWeight, density)
        val fw = if (format.isEmpty()) 0f
        else s.internalGap.value + textWidthDp(format, s.formatFontSize.value, s.formatWeight, density)
        return 2 * s.horizontalInset.value + tw + fw
    }

    private fun textWidthDp(text: String, sizeSp: Float, weight: FontWeight, density: androidx.compose.ui.unit.Density): Float {
        val paint = Paint().apply {
            textSize = with(density) { sizeSp.sp.toPx() }
            isFakeBoldText = weight.weight >= FontWeight.Bold.weight
        }
        val px = paint.measureText(text)
        return with(density) { px.toDp().value }
    }
}
