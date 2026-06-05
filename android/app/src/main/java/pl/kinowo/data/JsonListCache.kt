package pl.kinowo.data

import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import java.io.File

/**
 * On-disk cache of a JSON list payload plus the city + `Last-Modified` it was
 * fetched for, so a cold start paints instantly off disk and a warm reload of
 * the *same* city issues a conditional GET. One instance per endpoint
 * (repertoire, details). Mirrors iOS `RepertoireCache`, generalised since there
 * are now two payloads to cache.
 *
 * The cached `Last-Modified` is bound to the city it was fetched for: the
 * server stamps a single *global* timestamp regardless of city, so replaying
 * one city's value as `If-Modified-Since` for another city would draw a 304 and
 * strand the grid on the old city's films. [lastModifiedFor] therefore only
 * hands the timestamp back for the city that produced it — a switch sends no
 * conditional header and always gets a fresh 200.
 */
class JsonListCache<T>(
    cacheDir: File,
    name: String,
    private val elementSerializer: KSerializer<T>,
) {
    private val json = Json { ignoreUnknownKeys = true }
    private val listSerializer = ListSerializer(elementSerializer)
    private val dataFile = File(cacheDir, "$name.json")
    // line 1: city slug the body was fetched for; line 2: its Last-Modified.
    private val metaFile = File(cacheDir, "$name-meta.txt")

    fun load(): List<T>? = try {
        if (dataFile.exists()) json.decodeFromString(listSerializer, dataFile.readText()) else null
    } catch (_: Exception) {
        null
    }

    /**
     * Persist the freshly-fetched [list] for [city] together with its
     * [lastModified] header, so a later same-city reload can revalidate.
     */
    fun save(city: String, list: List<T>, lastModified: String?) {
        try {
            dataFile.writeText(json.encodeToString(listSerializer, list))
            metaFile.writeText(city + "\n" + (lastModified ?: ""))
        } catch (_: Exception) {
            // Best-effort; a failed write just means a slower next start.
        }
    }

    /**
     * The `Last-Modified` to replay as `If-Modified-Since`, but only when the
     * cached body belongs to [city]; null for any other city (forcing a full
     * fetch on a city switch — see the class note on the global timestamp).
     */
    fun lastModifiedFor(city: String): String? {
        val (cachedCity, lastModified) = readMeta() ?: return null
        return if (cachedCity == city) lastModified else null
    }

    private fun readMeta(): Pair<String, String?>? = try {
        if (!metaFile.exists()) null
        else metaFile.readText().split("\n", limit = 2).let { lines ->
            val city = lines.getOrNull(0)?.trim().orEmpty()
            if (city.isEmpty()) null
            else city to lines.getOrNull(1)?.trim()?.ifEmpty { null }
        }
    } catch (_: Exception) {
        null
    }
}
