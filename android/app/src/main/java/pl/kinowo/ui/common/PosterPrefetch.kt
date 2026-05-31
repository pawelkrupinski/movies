package pl.kinowo.ui.common

import androidx.compose.foundation.lazy.grid.LazyGridState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.ui.platform.LocalContext
import coil.imageLoader
import coil.request.CachePolicy
import coil.request.ImageRequest
import kotlin.math.abs

/**
 * Number of posters warmed per wave. Kept small so Coil's in-flight queue
 * stays short: each wave waits for the previous to finish, then re-reads the
 * scroll position, so a fast fling re-targets the next wave at wherever the
 * user landed instead of grinding through a frozen top-to-bottom order. A
 * wave a little larger than a screenful keeps the pipe full without flooding.
 */
private const val WAVE = 12

/**
 * Warms Coil's disk cache for every poster in [posterUrls] so the grid doesn't
 * fetch over the network as cards scroll into view. Posters nearest the current
 * scroll anchor are warmed first, in [WAVE]-sized waves; between waves the
 * scroll anchor is re-read, so flinging re-prioritises the still-unwarmed
 * posters around the new position. Every poster is warmed eventually, even
 * while the list sits idle.
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
    LaunchedEffect(posterUrls, gridState) {
        val loader = context.imageLoader
        val warmed = mutableSetOf<Int>()
        val total = posterUrls.count { it.isNotBlank() }
        while (warmed.size < total) {
            // Re-read each wave so the order follows the latest scroll.
            val wave = prefetchOrder(posterUrls, gridState.firstVisibleItemIndex)
                .filter { it !in warmed }
                .take(WAVE)
            if (wave.isEmpty()) break
            val jobs = wave.map { i ->
                warmed += i
                loader.enqueue(
                    ImageRequest.Builder(context)
                        .data(posterUrls[i])
                        .memoryCachePolicy(CachePolicy.DISABLED)
                        .build()
                ).job
            }
            // Wait for the wave to drain before picking the next, keeping
            // Coil's queue short so the re-prioritisation above stays live.
            jobs.forEach { it.join() }
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
