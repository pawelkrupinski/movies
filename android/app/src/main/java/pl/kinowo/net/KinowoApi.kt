package pl.kinowo.net

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.decodeFromStream
import okhttp3.CacheControl
import okhttp3.OkHttpClient
import okhttp3.Request
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import java.io.IOException

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

/** Which app versions the server still serves (`GET /api/client-support`) —
 *  the raw 200 body; throws on a transport failure or non-2xx. `fun interface`
 *  so tests can stub it in one line. */
fun interface ClientSupportApi {
    suspend fun fetchClientSupport(): String
}

/**
 * Talks to the kinowo backend: the per-city `GET /{city}/api/repertoire` (grid
 * listing), `/{city}/api/details` (synopsis, trailers, cast) and
 * `/{city}/api/cinemas`, plus the country-agnostic `/api/catalog` and
 * `/api/client-support`. Mirrors iOS `RepertoireStore` transport:
 * a `KinowoAndroid/1.0` User-Agent, conditional GET via `If-Modified-Since`,
 * and no on-disk URLCache.
 *
 * [client] is the composition root's (`KinowoApplication`): the one
 * OkHttp client every caller (posters included) shares, so its cookie jar, pools and timeouts
 * are configured once rather than a process-wide default built here.
 */
@OptIn(ExperimentalSerializationApi::class) // Json.decodeFromStream
class KinowoApi(
    private val baseUrl: String = "https://kinowo.net",
    private val client: OkHttpClient,
) : RepertoireApi, DetailsApi, CinemaCatalogApi, CatalogApi, ClientSupportApi {

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
            .header("User-Agent", USER_AGENT)
            .cacheControl(CacheControl.FORCE_NETWORK)
        if (ifNoneMatch != null) builder.header("If-None-Match", ifNoneMatch)
        client.newCall(builder.build()).execute().use { response ->
            if (response.code == 304) return@withContext FetchedCatalog(null, ifNoneMatch, notModified = true)
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            FetchedCatalog(response.body.string(), response.header("ETag"), notModified = false)
        }
    }

    /** Fetch `/api/client-support` (country-agnostic, like the catalog).
     *  Unconditional: it is asked once per launch and the body is ~200 bytes. */
    override suspend fun fetchClientSupport(): String = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/client-support")
            .header("User-Agent", USER_AGENT)
            .cacheControl(CacheControl.FORCE_NETWORK)
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            response.body.string()
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
            .header("User-Agent", USER_AGENT)
            .cacheControl(CacheControl.FORCE_NETWORK)
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            LenientJson.decodeFromStream<CinemaCatalog>(response.body.byteStream())
        }
    }

    private suspend inline fun <reified T> fetchList(
        url: String,
        ifModifiedSince: String?,
    ): Fetched<T> = withContext(Dispatchers.IO) {
        val builder = Request.Builder()
            .url(url)
            .header("User-Agent", USER_AGENT)
            .cacheControl(CacheControl.FORCE_NETWORK)
        if (ifModifiedSince != null) builder.header("If-Modified-Since", ifModifiedSince)
        client.newCall(builder.build()).execute().use { response ->
            if (response.code == 304) {
                return@withContext Fetched<T>(null, ifModifiedSince, notModified = true)
            }
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            // Decoded as the bytes arrive: a big city's repertoire is megabytes,
            // never held whole as a String beside the models it becomes.
            val items = LenientJson.decodeFromStream<List<T>>(response.body.byteStream())
            Fetched(items, response.header("Last-Modified"), notModified = false)
        }
    }
}
