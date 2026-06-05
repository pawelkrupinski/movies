package pl.kinowo.data

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.DetailsApi
import java.time.Duration
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

    private var lastReloadedAt: Instant? = null
    private val staleAfter: Duration = Duration.ofSeconds(60)

    fun loadCachedData() {
        if (_byTitle.value.isEmpty()) {
            cache.load()?.let { _byTitle.value = it.associateBy(FilmDetails::title) }
        }
    }

    suspend fun reload(citySlug: String, now: Instant = Instant.now()) {
        try {
            val result = api.fetchDetails(citySlug, cache.lastModifiedFor(citySlug))
            if (result.notModified) {
                lastReloadedAt = now
                return
            }
            val details = result.items ?: return
            _byTitle.value = details.associateBy(FilmDetails::title)
            lastReloadedAt = now
            cache.save(citySlug, details, result.lastModified)
        } catch (_: Exception) {
            // Details are non-critical; the detail screen falls back to the
            // listing-derived fields (poster, ratings, showings, cinema links).
        }
    }

    suspend fun reloadIfStale(citySlug: String, now: Instant = Instant.now()) {
        val last = lastReloadedAt
        if (last != null && Duration.between(last, now) < staleAfter) return
        reload(citySlug, now)
    }
}
