package pl.kinowo.data

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import pl.kinowo.model.Catalog
import pl.kinowo.net.CatalogApi

/**
 * The country/city catalog the app runs off. Seeded from the bundled snapshot
 * ([seedJson], shipped in the distro), refreshed on each open with a conditional
 * GET (`If-None-Match`), and persisted so a relaunch starts from the last fetch.
 * Publishes [catalog] for the UI to collect.
 *
 * Preference order at construction: the last persisted fetch, else the bundled
 * seed (which also supplies the ETag for the FIRST conditional GET — a 304 when
 * the build is current), else the compile-time fallback.
 */
class CatalogRepository(
    private val api: CatalogApi,
    private val cache: CatalogCache,
    seedJson: String?,
) {
    private val _catalog: MutableStateFlow<Catalog>
    val catalog: StateFlow<Catalog> get() = _catalog.asStateFlow()
    private var etag: String?

    init {
        val cached = cache.load()
        val seed = seedJson?.let { Catalog.parseSeed(it) }
        when {
            cached != null -> {
                _catalog = MutableStateFlow(Catalog.parseBody(cached.body) ?: Catalog.fallback)
                etag = cached.etag
            }
            seed != null -> {
                _catalog = MutableStateFlow(seed.second)
                etag = seed.first
            }
            else -> {
                _catalog = MutableStateFlow(Catalog.fallback)
                etag = null
            }
        }
    }

    /** Revalidate against the server. A 304 keeps the current catalog (no body);
     *  a 200 replaces + persists it. Any failure (offline/transient) is swallowed
     *  so whatever is loaded (persisted, seed, or fallback) stands. */
    suspend fun reload() {
        runCatching {
            val fetched = api.fetchCatalog(etag)
            if (fetched.notModified) return
            val body = fetched.body ?: return
            val parsed = Catalog.parseBody(body) ?: return
            _catalog.value = parsed
            etag = fetched.etag
            cache.save(body, fetched.etag)
        }
    }
}
