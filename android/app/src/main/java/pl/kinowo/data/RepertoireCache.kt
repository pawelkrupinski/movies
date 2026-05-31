package pl.kinowo.data

import kotlinx.serialization.json.Json
import pl.kinowo.model.Film
import java.io.File

/**
 * On-disk cache of the last good repertoire payload + its `Last-Modified`
 * header, so a cold start paints instantly off disk and warm reloads can
 * issue a conditional GET. Mirrors iOS `RepertoireCache` (two files under
 * the Caches directory).
 */
class RepertoireCache(cacheDir: File) {
    private val json = Json { ignoreUnknownKeys = true }
    private val filmsFile = File(cacheDir, "repertoire.json")
    private val lastModifiedFile = File(cacheDir, "repertoire-lastmodified.txt")

    fun load(): List<Film>? = try {
        if (filmsFile.exists()) json.decodeFromString<List<Film>>(filmsFile.readText()) else null
    } catch (_: Exception) {
        null
    }

    fun save(films: List<Film>) {
        try {
            filmsFile.writeText(json.encodeToString(films))
        } catch (_: Exception) {
            // Cache is best-effort; a failed write just means a slower next start.
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
