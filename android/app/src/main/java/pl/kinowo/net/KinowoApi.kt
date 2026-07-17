package pl.kinowo.net

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.Json
import okhttp3.CacheControl
import okhttp3.OkHttpClient
import okhttp3.Request
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import java.io.IOException
import java.util.concurrent.TimeUnit

/** The repertoire-listing half of the backend — all [RepertoireRepository] needs. */
interface RepertoireApi {
    suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?): KinowoApi.Fetched<Film>
}

/** The detail-payload half of the backend — all [pl.kinowo.data.DetailsRepository] needs. */
interface DetailsApi {
    suspend fun fetchDetails(citySlug: String, ifModifiedSince: String?): KinowoApi.Fetched<FilmDetails>
}

/** The cinema catalog (universe + area grouping) for a city. `fun interface` so
 *  tests can supply a one-liner stub (e.g. `CinemaCatalogApi { CinemaCatalog.EMPTY }`). */
fun interface CinemaCatalogApi {
    suspend fun fetchCinemas(citySlug: String): CinemaCatalog
}

/** The global country+city catalog (`GET /api/catalog`), fetched with a
 *  conditional GET (`If-None-Match`) so an unchanged catalog answers 304. */
fun interface CatalogApi {
    suspend fun fetchCatalog(ifNoneMatch: String?): KinowoApi.FetchedCatalog
}

/**
 * Talks to the kinowo backend via two endpoints: `GET /{city}/api/repertoire`
 * (grid listing) and `GET /{city}/api/details` (synopsis, trailers, cast).
 * The city slug prefixes the path. Mirrors iOS `RepertoireStore` transport:
 * a `KinowoAndroid/1.0` User-Agent, conditional GET via `If-Modified-Since`,
 * and no on-disk URLCache.
 */
class KinowoApi(
    private val baseUrl: String = "https://kinowo.fly.dev",
    private val client: OkHttpClient = defaultClient,
) : RepertoireApi, DetailsApi, CinemaCatalogApi, CatalogApi {
    private val json = Json { ignoreUnknownKeys = true }

    /** Result of a conditional GET: [items] is null on a 304 (use the cache). */
    data class Fetched<T>(
        val items: List<T>?,
        val lastModified: String?,
        val notModified: Boolean,
    )

    /** Result of the catalog conditional GET: [body] is null on a 304, else the
     *  raw `{countries,cities}` JSON; [etag] is the validator to persist + resend. */
    data class FetchedCatalog(
        val body: String?,
        val etag: String?,
        val notModified: Boolean,
    )

    /** Fetch `/api/catalog` (country-agnostic — any deployment serves the same
     *  bytes) with `If-None-Match`. A 304 returns [FetchedCatalog.notModified]. */
    override suspend fun fetchCatalog(ifNoneMatch: String?): FetchedCatalog = withContext(Dispatchers.IO) {
        val builder = Request.Builder()
            .url("$baseUrl/api/catalog")
            .header("User-Agent", UA)
            .cacheControl(CacheControl.FORCE_NETWORK)
        if (ifNoneMatch != null) builder.header("If-None-Match", ifNoneMatch)
        client.newCall(builder.build()).execute().use { response ->
            if (response.code == 304) return@withContext FetchedCatalog(null, ifNoneMatch, notModified = true)
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            val body = response.body?.string() ?: throw IOException("empty body")
            FetchedCatalog(body, response.header("ETag"), notModified = false)
        }
    }

    override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?): Fetched<Film> =
        fetchList("$baseUrl/$citySlug/api/repertoire", ifModifiedSince)

    override suspend fun fetchDetails(citySlug: String, ifModifiedSince: String?): Fetched<FilmDetails> =
        fetchList("$baseUrl/$citySlug/api/details", ifModifiedSince)

    /** Fetch the static cinema catalog for a city. Unconditional GET (the data
     *  is compile-time on the server); the caller fetches it once per city. */
    override suspend fun fetchCinemas(citySlug: String): CinemaCatalog = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/$citySlug/api/cinemas")
            .header("User-Agent", UA)
            .cacheControl(CacheControl.FORCE_NETWORK)
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            val body = response.body?.string() ?: throw IOException("empty body")
            json.decodeFromString<CinemaCatalog>(body)
        }
    }

    private suspend inline fun <reified T> fetchList(
        url: String,
        ifModifiedSince: String?,
    ): Fetched<T> = withContext(Dispatchers.IO) {
        val builder = Request.Builder()
            .url(url)
            .header("User-Agent", UA)
            .cacheControl(CacheControl.FORCE_NETWORK)
        if (ifModifiedSince != null) builder.header("If-Modified-Since", ifModifiedSince)
        client.newCall(builder.build()).execute().use { response ->
            if (response.code == 304) {
                return@withContext Fetched<T>(null, ifModifiedSince, notModified = true)
            }
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            val body = response.body?.string() ?: throw IOException("empty body")
            Fetched(json.decodeFromString<List<T>>(body), response.header("Last-Modified"), notModified = false)
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
