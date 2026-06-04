package pl.kinowo.ui.common

import androidx.compose.runtime.compositionLocalOf
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.TextUnit
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

/**
 * Live-tunable fonts and vertical gaps of the film-detail screen
 * (`pl.kinowo.ui.detail.DetailScreen`), read from [LocalFilmDetailStyle].
 *
 * The defaults are the SHIPPING values, so production (which never provides its
 * own value) renders exactly as before. The non-prod tuning pager injects an
 * edited copy via `CompositionLocalProvider` to preview the detail screen's
 * typography and spacing against a real film. Mirrors the role of
 * [CardSpacingStyle] for the listing card.
 */
data class FilmDetailStyle(
    /** Polish title at the top of the header block. */
    val titleFontSize: TextUnit = 20.sp,
    /** Title font weight. */
    val titleWeight: FontWeight = FontWeight.Bold,
    /** Original (production-language) italic title under the Polish one. */
    val originalTitleFontSize: TextUnit = 14.sp,
    /** Uppercase section labels (OPIS / REŻYSERIA / OBSADA / …). */
    val sectionLabelFontSize: TextUnit = 11.sp,
    /** Body text of the meta blocks (synopsis, director, cast, countries). */
    val sectionBodyFontSize: TextUnit = 14.sp,
    /** "Seanse" header above the full showings block. */
    val showingsHeaderFontSize: TextUnit = 14.sp,
    /** Vertical gap between the top-level header / link / meta / showings
     *  blocks (the outer scroll Column's arrangement). */
    val outerSpacing: Dp = 16.dp,
    /** Vertical gap between the poster header's title, original title, meta
     *  pills, and ratings. */
    val headerSpacing: Dp = 8.dp,
)

/** Style driving the detail screen. The default equals today's shipping values,
 *  so production renders exactly as before; the tuning pager overrides it
 *  through `CompositionLocalProvider`. */
val LocalFilmDetailStyle = compositionLocalOf { FilmDetailStyle() }
