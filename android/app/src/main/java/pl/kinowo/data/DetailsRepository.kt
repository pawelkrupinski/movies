package pl.kinowo.data

import pl.kinowo.runCatchingCancellable
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.DetailsApi
import java.time.Instant

/**
 * Owns the detail-only payload (synopsis + trailers) fetched from
 * `/api/details`, exposed as a `title -> FilmDetails` map for the detail
 * screen to look up. Fetched in parallel with the listing; a missing key just
 * means "no extra detail for this film". Mirrors [RepertoireRepository]'s
 * cache + conditional-GET shape.
 */
class DetailsRepository(
    private val api: DetailsApi,
    private val cache: JsonListCache<FilmDetails>,
) {
    private val _byTitle = MutableStateFlow<Map<String, FilmDetails>>(emptyMap())
    val byTitle: StateFlow<Map<String, FilmDetails>> = _byTitle.asStateFlow()

    private val freshness = ReloadFreshness()

    fun loadCachedData() {
        if (_byTitle.value.isEmpty()) {
            cache.load()?.let { _byTitle.value = it.associateBy(FilmDetails::title) }
        }
    }

    suspend fun reload(citySlug: String, now: Instant = Instant.now()) {
        // Details are non-critical; on failure the detail screen falls back to
        // the listing-derived fields (poster, ratings, showings, cinema links).
        runCatchingCancellable {
            val result = api.fetchDetails(citySlug, cache.lastModifiedFor(citySlug))
            if (result.notModified) {
                freshness.markReloaded(now)
                return
            }
            val details = result.items ?: return
            _byTitle.value = details.associateBy(FilmDetails::title)
            freshness.markReloaded(now)
            cache.save(citySlug, details, result.lastModified)
        }
    }

    suspend fun reloadIfStale(citySlug: String, now: Instant = Instant.now()) {
        if (freshness.isFresh(now)) return
        reload(citySlug, now)
    }
}
