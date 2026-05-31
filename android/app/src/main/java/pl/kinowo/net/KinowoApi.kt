package pl.kinowo.net

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.Json
import okhttp3.CacheControl
import okhttp3.OkHttpClient
import okhttp3.Request
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import java.io.IOException
import java.util.concurrent.TimeUnit

/**
 * Talks to the kinowo backend. The whole app reads one endpoint —
 * `GET /api/repertoire` — which carries every field both the grid and the
 * detail screen need (incl. synopsis + trailer embed URLs). Mirrors iOS
 * `RepertoireStore` transport: a `KinowoAndroid/1.0` User-Agent, a
 * conditional GET via `If-Modified-Since`, and no on-disk URLCache.
 */
class KinowoApi(
    private val baseUrl: String = "https://kinowo.fly.dev",
    private val client: OkHttpClient = defaultClient,
) {
    private val json = Json { ignoreUnknownKeys = true }

    /** Result of a conditional GET: [items] is null on a 304 (use the cache). */
    data class Fetched<T>(
        val items: List<T>?,
        val lastModified: String?,
        val notModified: Boolean,
    )

    suspend fun fetchRepertoire(ifModifiedSince: String?): Fetched<Film> =
        fetchList("$baseUrl/api/repertoire", ifModifiedSince)

    suspend fun fetchDetails(ifModifiedSince: String?): Fetched<FilmDetails> =
        fetchList("$baseUrl/api/details", ifModifiedSince)

    private suspend inline fun <reified T> fetchList(
        url: String,
        ifModifiedSince: String?,
    ): Fetched<T> = withContext(Dispatchers.IO) {
        val builder = Request.Builder()
            .url(url)
            .header("User-Agent", UA)
            .cacheControl(CacheControl.FORCE_NETWORK)
        if (ifModifiedSince != null) builder.header("If-Modified-Since", ifModifiedSince)
        client.newCall(builder.build()).execute().use { resp ->
            if (resp.code == 304) {
                return@withContext Fetched<T>(null, ifModifiedSince, notModified = true)
            }
            if (!resp.isSuccessful) throw IOException("HTTP ${resp.code}")
            val body = resp.body?.string() ?: throw IOException("empty body")
            Fetched(json.decodeFromString<List<T>>(body), resp.header("Last-Modified"), notModified = false)
        }
    }

    companion object {
        private const val UA = "KinowoAndroid/1.0"

        val defaultClient: OkHttpClient by lazy {
            OkHttpClient.Builder()
                .connectTimeout(15, TimeUnit.SECONDS)
                .readTimeout(20, TimeUnit.SECONDS)
                .build()
        }
    }
}
