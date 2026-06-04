package pl.kinowo.ui.common

import androidx.compose.runtime.compositionLocalOf
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

/**
 * Live-tunable look of the Kina-tab cinema section header (the blue cinema name
 * above each section's 2-column poster grid), read from [LocalCinemaHeaderStyle]
 * by `pl.kinowo.ui.list.CinemaGrid`.
 *
 * The defaults are the SHIPPING values, so production (which never provides its
 * own value) renders exactly as before. The non-prod tuning pager injects an
 * edited copy via `CompositionLocalProvider` to preview header changes against
 * the real sectioned list. Mirrors the role of [CardSpacingStyle] for the card.
 *
 * The live header is a plain blue `Text` with no underline, so there is no
 * underline lever — only font, weight, and the gaps around it.
 */
data class CinemaHeaderStyle(
    /** Cinema-name font size. */
    val fontSize: TextUnit = 15.sp,
    /** Cinema-name font weight. */
    val fontWeight: FontWeight = FontWeight.SemiBold,
    /** Gap above the header (between the previous section's last row and it). */
    val headerTopGap: Dp = 8.dp,
    /** Gap below the header, above its section's first poster row. */
    val headerToGrid: Dp = 2.dp,
    /** Vertical gap between stacked grid rows (the section-to-section rhythm). */
    val sectionSpacing: Dp = 12.dp,
)

/** Style driving every Kina-tab cinema header. The default equals today's
 *  shipping values, so production renders exactly as before; the tuning pager
 *  overrides it through `CompositionLocalProvider`. */
val LocalCinemaHeaderStyle = compositionLocalOf { CinemaHeaderStyle() }
