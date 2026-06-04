package pl.kinowo.ui.common

import androidx.compose.runtime.compositionLocalOf
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp

/**
 * Live-tunable vertical gaps in a [pl.kinowo.ui.list.FilmCard], read from
 * [LocalCardSpacingStyle] by the card and its showings block.
 *
 * The defaults are the SHIPPING gaps — every section is separated by 8 dp today
 * — so production (which never provides its own value) renders exactly as before.
 * The non-prod `ShowtimeTuningScreen` injects an edited copy via
 * `CompositionLocalProvider` to preview spacing changes against the real card.
 * Each field is a DISTINCT spacing property already present in the card layout.
 * Mirrors the role of [ShowtimeChipStyle] for chip styling.
 */
data class CardSpacingStyle(
    /** Gap below the title, above the runtime/year meta pills. */
    val titleToMeta: Dp = 8.dp,
    /** Gap below the meta pills, above the rating badges. */
    val metaToRatings: Dp = 8.dp,
    /** Gap below the ratings, above the showings block. */
    val ratingsToShowings: Dp = 8.dp,
    /** Spacing inside the showings block (day↔cinema, cinema↔pills, pills↔day). */
    val showingsBlock: Dp = 8.dp,
)

/** Spacing driving every film card. The default equals today's shipping gaps, so
 *  production renders exactly as before; the tuning screen overrides it through
 *  `CompositionLocalProvider`. */
val LocalCardSpacingStyle = compositionLocalOf { CardSpacingStyle() }
