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
import androidx.compose.material3.Icon
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import pl.kinowo.model.Film
import pl.kinowo.ui.common.LocalCardSpacingStyle
import pl.kinowo.ui.common.MetaPills
import pl.kinowo.ui.common.PosterImage
import pl.kinowo.ui.common.RatingBadges
import pl.kinowo.ui.common.Showings
import pl.kinowo.ui.common.copyFilmLink
import pl.kinowo.ui.theme.CardSurface

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
    val spacing = LocalCardSpacingStyle.current
    Surface(
        color = CardSurface,
        shape = RoundedCornerShape(12.dp),
        // Tap opens the detail screen; long-press copies the canonical
        // `/film?title=…` link (there's no address bar to copy from on mobile).
        modifier = modifier.fillMaxWidth().combinedClickable(
            onClick = onOpen,
            onLongClick = { copyFilmLink(context, film.title) },
        ),
    ) {
        Column {
            Box {
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
                        .padding(6.dp)
                        .size(28.dp)
                        .clip(RoundedCornerShape(14.dp))
                        .background(Color.Black.copy(alpha = 0.45f))
                        .clickable(onClick = onHide),
                    contentAlignment = Alignment.Center,
                ) {
                    Icon(
                        Icons.Filled.Close,
                        contentDescription = "Ukryj film",
                        tint = Color.White,
                        modifier = Modifier.size(16.dp),
                    )
                }
            }
            Column(Modifier.padding(12.dp)) {
                Text(
                    text = film.title,
                    color = Color.White,
                    fontSize = 14.sp,
                    fontWeight = FontWeight.SemiBold,
                    modifier = Modifier.fillMaxWidth(),
                )
                // Runtime + year only — the listing card drops genres
                // (they stay on the detail screen).
                MetaPills(
                    runtimeMinutes = film.runtimeMinutes,
                    releaseYear = film.releaseYear,
                    modifier = Modifier.padding(top = spacing.titleToMeta),
                )
                if (!film.ratings.isEmpty) {
                    RatingBadges(film.ratings, Modifier.padding(top = spacing.metaToRatings))
                }
                Showings(
                    film = film,
                    showCinemaHeaders = showCinemaHeaders,
                    maxChips = 14,
                    modifier = Modifier.padding(top = spacing.ratingsToShowings),
                )
            }
        }
    }
}
