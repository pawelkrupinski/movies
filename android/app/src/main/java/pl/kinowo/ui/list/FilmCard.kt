package pl.kinowo.ui.list

import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.aspectRatio
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Link
import androidx.compose.material.icons.filled.Share
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.Icon
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.model.Film
import pl.kinowo.ui.common.LocalCardSpacingStyle
import pl.kinowo.ui.common.LocalCitySlug
import pl.kinowo.ui.common.MetaPills
import pl.kinowo.ui.common.PosterImage
import pl.kinowo.ui.common.RatingBadges
import pl.kinowo.ui.common.Showings
import pl.kinowo.ui.common.ShowtimeChipMetrics
import pl.kinowo.ui.common.copyFilmLink
import pl.kinowo.ui.common.layoutWidthDp
import pl.kinowo.ui.common.shareFilm
import pl.kinowo.ui.theme.CardSurface

/** Test tag on the whole card, so the tap-to-open test can click it. */
internal const val FilmCardTestTag = "film-card"

/** Test tag on the poster, which owns the long-press share menu. */
internal const val FilmCardPosterTestTag = "film-card-poster"

/**
 * One film card in the grid: 2:3 poster (with a hide button), then title +
 * runtime, ratings, and a truncated showtimes block. The whole card opens the
 * detail screen; the hide button and inner links capture their own taps.
 * Mirrors iOS `FilmCardView`.
 */
@OptIn(ExperimentalFoundationApi::class)
@Composable
fun FilmCard(
    film: Film,
    showCinemaHeaders: Boolean,
    onOpen: () -> Unit,
    onHide: () -> Unit,
    modifier: Modifier = Modifier,
) {
    val context = LocalContext.current
    // The share link is city-scoped; the slug comes from the city-gate root.
    val citySlug = LocalCitySlug.current
    val spacing = LocalCardSpacingStyle.current
    // Card gaps scale with the device's portrait width off the 360 dp baseline,
    // in lockstep with the chips (layoutWidthDp, so landscape matches portrait —
    // see ShowtimeChipMetrics). scale == 1f at the 360 dp floor.
    val scale = ShowtimeChipMetrics.scale(layoutWidthDp())
    var menuExpanded by remember { mutableStateOf(false) }
    Surface(
        color = CardSurface,
        shape = RoundedCornerShape((12 * scale).dp),
        // A tap anywhere opens the detail screen. Long-press is deliberately a
        // no-op at the card level: the share menu lives on the POSTER and the
        // room tooltip on each showtime CHIP (both below). A card-wide
        // onLongClick would (a) swallow the chip's room tooltip and (b) — were
        // it a plain `clickable` instead — a long-press-then-release would
        // register as a tap and open the detail screen out from under the
        // chip's hold. combinedClickable separates the two so neither happens.
        modifier = modifier.fillMaxWidth().testTag(FilmCardTestTag).combinedClickable(
            onClick = onOpen,
            onLongClick = {},
        ),
    ) {
        Column {
            ShareMenu(
                expanded = menuExpanded,
                onDismiss = { menuExpanded = false },
                onShare = { shareFilm(context, citySlug, film.title) },
                onCopy = { copyFilmLink(context, citySlug, film.title) },
            )
            // The poster owns the long-press → share menu (Udostępnij / Skopiuj
            // link), mirroring iOS `FilmCardView`, where the `.contextMenu` now
            // sits on `PosterView`. A tap still opens the detail screen.
            Box(
                modifier = Modifier
                    .testTag(FilmCardPosterTestTag)
                    .combinedClickable(
                        onClick = onOpen,
                        onLongClick = { menuExpanded = true },
                    ),
            ) {
                PosterImage(
                    chain = film.posterChain,
                    contentDescription = film.title,
                    modifier = Modifier
                        .fillMaxWidth()
                        .aspectRatio(2f / 3f),
                )
                // Hide (⊗) button, top-right.
                Box(
                    modifier = Modifier
                        .align(Alignment.TopEnd)
                        .padding((6 * scale).dp)
                        .size((28 * scale).dp)
                        .clip(RoundedCornerShape((14 * scale).dp))
                        .background(Color.Black.copy(alpha = 0.45f))
                        .clickable(onClick = onHide),
                    contentAlignment = Alignment.Center,
                ) {
                    Icon(
                        Icons.Filled.Close,
                        contentDescription = "Ukryj film",
                        tint = Color.White,
                        modifier = Modifier.size((16 * scale).dp),
                    )
                }
            }
            // Card content padding stays a fixed 12dp: the shared grid geometry
            // (PosterGridMetrics.cardContentDp) subtracts exactly this to derive
            // the showings-column width the two-per-row guarantee is proven
            // against, so scaling it would silently shrink that column on wide
            // phones. Everything INSIDE scales with the viewport instead.
            Column(Modifier.padding(12.dp)) {
                Text(
                    text = film.title,
                    color = Color.White,
                    fontSize = (14 * scale).sp,
                    fontWeight = FontWeight.SemiBold,
                    modifier = Modifier.fillMaxWidth(),
                )
                // Runtime + year + the first three genres, mirroring the web
                // `_cardTitle` (`movie.genres.take(3)`); the detail screen
                // shows them all.
                MetaPills(
                    runtimeMinutes = film.runtimeMinutes,
                    releaseYear = film.releaseYear,
                    genres = film.genres.take(3),
                    scale = scale,
                    modifier = Modifier.padding(top = spacing.titleToMeta * scale),
                )
                if (!film.ratings.isEmpty) {
                    RatingBadges(film.ratings, Modifier.padding(top = spacing.metaToRatings * scale))
                }
                Showings(
                    film = film,
                    showCinemaHeaders = showCinemaHeaders,
                    maxChips = 14,
                    modifier = Modifier.padding(top = spacing.ratingsToShowings * scale),
                )
            }
        }
    }
}

/**
 * The card's long-press menu: share the film's public link via the system
 * sheet, or copy it to the clipboard. Mirrors the two actions iOS offers in
 * `FilmCardView`'s `.contextMenu`. Each action dismisses the menu first so the
 * launched chooser / clipboard toast isn't fighting the popup.
 */
@Composable
private fun ShareMenu(
    expanded: Boolean,
    onDismiss: () -> Unit,
    onShare: () -> Unit,
    onCopy: () -> Unit,
) {
    DropdownMenu(expanded = expanded, onDismissRequest = onDismiss) {
        DropdownMenuItem(
            text = { Text("Udostępnij") },
            leadingIcon = { Icon(Icons.Filled.Share, contentDescription = null) },
            onClick = { onDismiss(); onShare() },
        )
        DropdownMenuItem(
            text = { Text("Skopiuj link") },
            leadingIcon = { Icon(Icons.Filled.Link, contentDescription = null) },
            onClick = { onDismiss(); onCopy() },
        )
    }
}
