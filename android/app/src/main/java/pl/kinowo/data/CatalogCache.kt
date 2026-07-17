package pl.kinowo.data

import java.io.File

/**
 * On-disk persistence of the last fetched catalog: the raw `{countries,cities}`
 * body plus its ETag. So a relaunch starts from the last fetch (not the bundled
 * seed), and the first fetch revalidates with the persisted ETag.
 */
class CatalogCache(cacheDir: File) {
    data class Cached(val body: String, val etag: String)

    private val bodyFile = File(cacheDir, "catalog-body.json")
    private val etagFile = File(cacheDir, "catalog-etag.txt")

    fun load(): Cached? = runCatching {
        if (bodyFile.exists() && etagFile.exists()) Cached(bodyFile.readText(), etagFile.readText()) else null
    }.getOrNull()

    fun save(body: String, etag: String?) {
        runCatching {
            bodyFile.writeText(body)
            if (etag != null) etagFile.writeText(etag)
        }
    }
}
