package pl.kinowo.data

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.filter.prunedPastShowings
import pl.kinowo.model.Film
import pl.kinowo.net.RepertoireApi
import java.time.Duration
import java.time.Instant

/**
 * Owns the repertoire payload + its freshness bookkeeping — the Android
 * counterpart of iOS `RepertoireStore`. Network and cache live here; the
 * ViewModel layers the per-screen filter state on top of [films].
 */
class RepertoireRepository(
    private val api: RepertoireApi,
    private val cache: JsonListCache<Film>,
) {
    private val _films = MutableStateFlow<List<Film>>(emptyList())
    val films: StateFlow<List<Film>> = _films.asStateFlow()

    private val _isLoading = MutableStateFlow(false)
    val isLoading: StateFlow<Boolean> = _isLoading.asStateFlow()

    private val _error = MutableStateFlow<String?>(null)
    val error: StateFlow<String?> = _error.asStateFlow()

    // The city whose repertoire `films` currently holds, or null before the
    // first successful load. `films` is NOT cleared on a city switch (to avoid
    // flashing an empty grid), so it briefly holds the PREVIOUS city's list
    // mid-switch — anything that must read the right city's films (e.g. a deep
    // link's film lookup) waits for this to equal the target slug rather than
    // for `films` to merely be non-empty.
    private val _loadedCity = MutableStateFlow<String?>(null)
    val loadedCity: StateFlow<String?> = _loadedCity.asStateFlow()

    private var lastReloadedAt: Instant? = null
    private val staleAfter: Duration = Duration.ofSeconds(60)

    /** Paint cached data instantly on cold start (before the network call). */
    fun loadCachedData() {
        if (_films.value.isEmpty()) {
            cache.load()?.let { _films.value = it }
        }
    }

    suspend fun reload(citySlug: String, now: Instant = Instant.now()) {
        _isLoading.value = true
        _error.value = null
        try {
            val result = api.fetchRepertoire(citySlug, cache.lastModifiedFor(citySlug))
            if (result.notModified) {
                // 304: `films` already holds this city's data (the conditional
                // header is city-bound, so a 304 only comes back for the same
                // city). Mark it loaded so a deep link can proceed.
                _loadedCity.value = citySlug
                lastReloadedAt = now
                return
            }
            val films = result.items ?: return
            _films.value = films
            _loadedCity.value = citySlug
            lastReloadedAt = now
            cache.save(citySlug, films, result.lastModified)
        } catch (e: Exception) {
            _error.value = e.message ?: e.toString()
        } finally {
            _isLoading.value = false
        }
    }

    suspend fun reloadIfStale(citySlug: String, now: Instant = Instant.now()) {
        val last = lastReloadedAt
        if (last != null && Duration.between(last, now) < staleAfter) return
        reload(citySlug, now)
    }

    /** Local-only: drop screenings now in the past and re-sort cinemas by
     *  earliest remaining slot. Cheap to run on resume between fetches. */
    fun pruneStaleShowings(now: Instant = Instant.now()) {
        val pruned = _films.value.prunedPastShowings(now)
        if (pruned != _films.value) _films.value = pruned
    }
}
