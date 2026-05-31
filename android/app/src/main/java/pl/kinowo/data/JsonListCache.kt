package pl.kinowo.data

import kotlinx.serialization.KSerializer
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import java.io.File

/**
 * On-disk cache of a JSON list payload plus its `Last-Modified` header, so a
 * cold start paints instantly off disk and warm reloads issue a conditional
 * GET. One instance per endpoint (repertoire, details). Mirrors iOS
 * `RepertoireCache`, generalised since there are now two payloads to cache.
 */
class JsonListCache<T>(
    cacheDir: File,
    name: String,
    private val elementSerializer: KSerializer<T>,
) {
    private val json = Json { ignoreUnknownKeys = true }
    private val listSerializer = ListSerializer(elementSerializer)
    private val dataFile = File(cacheDir, "$name.json")
    private val lastModifiedFile = File(cacheDir, "$name-lastmodified.txt")

    fun load(): List<T>? = try {
        if (dataFile.exists()) json.decodeFromString(listSerializer, dataFile.readText()) else null
    } catch (_: Exception) {
        null
    }

    fun save(list: List<T>) {
        try {
            dataFile.writeText(json.encodeToString(listSerializer, list))
        } catch (_: Exception) {
            // Best-effort; a failed write just means a slower next start.
        }
    }

    fun loadLastModified(): String? = try {
        if (lastModifiedFile.exists()) lastModifiedFile.readText().trim().ifEmpty { null } else null
    } catch (_: Exception) {
        null
    }

    fun saveLastModified(value: String) {
        try {
            lastModifiedFile.writeText(value)
        } catch (_: Exception) {
        }
    }
}
