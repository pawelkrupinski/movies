package pl.kinowo.ui.common

import androidx.compose.runtime.compositionLocalOf
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp

/**
 * Live-tunable vertical gaps in a [pl.kinowo.ui.list.FilmCard], read from
 * [LocalCardSpacingStyle] by the card and its showings block.
 *
 * The defaults are the SHIPPING gaps, dialled in on the tuning screen — so
 * production (which never provides its own value) renders exactly as before.
 * The non-prod `ShowtimeTuningScreen` injects an edited copy via
 * `CompositionLocalProvider` to preview spacing changes against the real card.
 * Each field is a DISTINCT spacing property already present in the card layout.
 * Mirrors the role of [ShowtimeChipStyle] for chip styling.
 */
data class CardSpacingStyle(
    /** Gap below the title, above the runtime/year meta pills. */
    val titleToMeta: Dp = 1.dp,
    /** Gap below the meta pills, above the rating badges. */
    val metaToRatings: Dp = 6.dp,
    /** Gap below the ratings, above the showings block. */
    val ratingsToShowings: Dp = 5.dp,
    /** Gap below a day label, above its first cinema name (or showtimes when the
     *  cinema header is hidden). Independent of [showingsBlock] so the day↔cinema
     *  distance can be dialled separately. */
    val dayToCinema: Dp = 4.5.dp,
    /** Spacing for the rest of the showings block (cinema↔pills, pills↔next
     *  cinema, day-block↔day-block). The day↔cinema gap is [dayToCinema]. */
    val showingsBlock: Dp = 4.5.dp,
)

/** Spacing driving every film card. The default equals today's shipping gaps, so
 *  production renders exactly as before; the tuning screen overrides it through
 *  `CompositionLocalProvider`. */
val LocalCardSpacingStyle = compositionLocalOf { CardSpacingStyle() }
