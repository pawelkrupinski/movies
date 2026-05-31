package pl.kinowo.data

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.filter.prunedPastShowings
import pl.kinowo.model.Film
import pl.kinowo.net.KinowoApi
import java.time.Duration
import java.time.Instant

/**
 * Owns the repertoire payload + its freshness bookkeeping — the Android
 * counterpart of iOS `RepertoireStore`. Network and cache live here; the
 * ViewModel layers the per-screen filter state on top of [films].
 */
class RepertoireRepository(
    private val api: KinowoApi,
    private val cache: JsonListCache<Film>,
) {
    private val _films = MutableStateFlow<List<Film>>(emptyList())
    val films: StateFlow<List<Film>> = _films.asStateFlow()

    private val _isLoading = MutableStateFlow(false)
    val isLoading: StateFlow<Boolean> = _isLoading.asStateFlow()

    private val _error = MutableStateFlow<String?>(null)
    val error: StateFlow<String?> = _error.asStateFlow()

    private var lastReloadedAt: Instant? = null
    private val staleAfter: Duration = Duration.ofSeconds(60)

    /** Paint cached data instantly on cold start (before the network call). */
    fun loadCachedData() {
        if (_films.value.isEmpty()) {
            cache.load()?.let { _films.value = it }
        }
    }

    suspend fun reload(now: Instant = Instant.now()) {
        _isLoading.value = true
        _error.value = null
        try {
            val result = api.fetchRepertoire(cache.loadLastModified())
            if (result.notModified) {
                lastReloadedAt = now
                return
            }
            val films = result.items ?: return
            _films.value = films
            lastReloadedAt = now
            result.lastModified?.let { cache.saveLastModified(it) }
            cache.save(films)
        } catch (e: Exception) {
            _error.value = e.message ?: e.toString()
        } finally {
            _isLoading.value = false
        }
    }

    suspend fun reloadIfStale(now: Instant = Instant.now()) {
        val last = lastReloadedAt
        if (last != null && Duration.between(last, now) < staleAfter) return
        reload(now)
    }

    /** Local-only: drop screenings now in the past and re-sort cinemas by
     *  earliest remaining slot. Cheap to run on resume between fetches. */
    fun pruneStaleShowings(now: Instant = Instant.now()) {
        val pruned = _films.value.prunedPastShowings(now)
        if (pruned != _films.value) _films.value = pruned
    }
}
