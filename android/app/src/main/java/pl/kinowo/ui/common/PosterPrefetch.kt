package pl.kinowo.ui.common

import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.remember
import androidx.compose.runtime.snapshotFlow
import androidx.compose.ui.platform.LocalContext
import coil.imageLoader
import coil.request.CachePolicy
import coil.request.ImageRequest
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlin.math.abs

/**
 * Warms Coil's disk cache for every poster in [posterUrls] so the grid
 * doesn't fetch over the network as cards scroll into view. Posters nearest
 * the current scroll anchor are enqueued first; the rest follow outward. Each
 * URL is enqueued at most once.
 *
 * [posterUrls] must mirror the grid's item order 1:1 — pass a blank string for
 * any non-poster grid item (e.g. a section header) so a list index lines up
 * with [LazyGridState.firstVisibleItemIndex] and the nearest-first ordering is
 * exact.
 *
 * Disk-only (memory cache disabled): prefetch lands the bytes on disk and the
 * on-screen `SubcomposeAsyncImage` decodes from there on first paint — no
 * network wait — without pinning hundreds of decoded bitmaps in memory. Only
 * the primary poster (chain head) is warmed; the rare fallback walk still hits
 * the network on the card that needs it.
 */
@Composable
fun PosterPrefetch(posterUrls: List<String>, gridState: LazyGridState) {
    val context = LocalContext.current
    val enqueued = remember(posterUrls) { mutableSetOf<String>() }
    LaunchedEffect(posterUrls, gridState) {
        val loader = context.imageLoader
        snapshotFlow { gridState.firstVisibleItemIndex }
            .distinctUntilChanged()
            .collect { anchor ->
                prefetchOrder(posterUrls, anchor)
                    .filter { posterUrls[it] !in enqueued }
                    .forEach { i ->
                        val url = posterUrls[i]
                        enqueued += url
                        loader.enqueue(
                            ImageRequest.Builder(context)
                                .data(url)
                                .memoryCachePolicy(CachePolicy.DISABLED)
                                .build()
                        )
                    }
            }
    }
}

/**
 * Indices of [posterUrls] worth prefetching — non-blank entries only —
 * ordered nearest first relative to [anchor] (the first visible item index).
 * Blank slots (section headers) are dropped. Pure, so the nearest-first rule
 * is unit-testable without Compose or Coil. The sort is stable, so at equal
 * distance the lower (higher up) index comes first.
 */
internal fun prefetchOrder(posterUrls: List<String>, anchor: Int): List<Int> =
    posterUrls.indices
        .filter { posterUrls[it].isNotBlank() }
        .sortedBy { abs(it - anchor) }
