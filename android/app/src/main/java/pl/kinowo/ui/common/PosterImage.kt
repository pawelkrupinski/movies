package pl.kinowo.ui.common

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Movie
import androidx.compose.material3.Icon
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import coil.compose.SubcomposeAsyncImage
import coil.compose.SubcomposeAsyncImageContent
import coil.request.ImageRequest
import pl.kinowo.ui.theme.CardSurface
import pl.kinowo.ui.theme.TextSecondary

/**
 * Poster that walks a fallback chain: when one URL fails to load, it advances
 * to the next (cinema CDNs intermittently 403/404 on the bytes). Mirrors iOS
 * `FilmCardView.PosterImage`. Shows a film-strip placeholder while loading
 * and after the whole chain is exhausted.
 */
@Composable
fun PosterImage(
    chain: List<String>,
    contentDescription: String?,
    modifier: Modifier = Modifier,
    // Listing/detail thumbnails crop to fill their 2:3 box; the full-screen
    // viewer passes Fit so the whole poster shows, letterboxed.
    contentScale: ContentScale = ContentScale.Crop,
    // Thumbnails sit on the card surface; the full-screen viewer passes
    // Transparent so its black backdrop shows through the Fit letterbox bars.
    background: Color = CardSurface,
) {
    Box(modifier = modifier.background(background), contentAlignment = Alignment.Center) {
        if (chain.isEmpty()) {
            PlaceholderGlyph()
            return@Box
        }
        var index by remember(chain) { mutableIntStateOf(0) }
        SubcomposeAsyncImage(
            model = ImageRequest.Builder(LocalContext.current)
                .data(chain[index])
                // No crossfade: PosterPrefetch warms the disk cache ahead of
                // scroll, so posters should snap in rather than fade.
                .crossfade(false)
                .build(),
            contentDescription = contentDescription,
            contentScale = contentScale,
            modifier = Modifier.fillMaxSize(),
            loading = { PlaceholderGlyph() },
            error = {
                // Advance to the next candidate; if exhausted, leave the glyph.
                if (index < chain.lastIndex) index++ else PlaceholderGlyph()
            },
            success = { SubcomposeAsyncImageContent() },
        )
    }
}

@Composable
private fun PlaceholderGlyph() {
    Icon(
        imageVector = Icons.Outlined.Movie,
        contentDescription = null,
        tint = TextSecondary.copy(alpha = 0.35f),
        modifier = Modifier.fillMaxSize(0.35f),
    )
}

/** Open an external URL in the browser / relevant app. No-op on bad URLs. */
fun openUrl(context: Context, url: String?) {
    if (url.isNullOrBlank()) return
    try {
        context.startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(url)))
    } catch (_: ActivityNotFoundException) {
    } catch (_: Exception) {
    }
}

/** The IMDb app's `imdb://title/tt…` deep link, derived from the public IMDb
 *  web URL, or null when that URL carries no `tt…` title id. */
fun imdbAppUri(webUrl: String?): String? {
    val titleId = webUrl?.let { Regex("tt\\d+").find(it)?.value } ?: return null
    return "imdb://title/$titleId"
}

/** Open an IMDb title in the IMDb app, falling back to its web page when the
 *  app isn't installed. The other rating sources just call [openUrl]: RT and
 *  Filmweb already open their apps from the https link via Android App Links,
 *  and Metacritic has no app. */
fun openImdb(context: Context, webUrl: String?) {
    val appUri = imdbAppUri(webUrl)
    if (appUri != null) {
        try {
            context.startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(appUri)))
            return
        } catch (_: ActivityNotFoundException) {
        } catch (_: Exception) {
        }
    }
    openUrl(context, webUrl)
}
