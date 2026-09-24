package pl.kinowo.auth

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import okhttp3.Response
import java.io.IOException

/** One country's hiddenFilms set plus the validators the NEXT conditional
 *  fetch should send back — `etag`/`lastModified` are null only when the
 *  server didn't return one (shouldn't happen in practice, but the type
 *  doesn't assume it). */
data class HiddenFilmsState(
    val hiddenFilms: Set<String>,
    val etag: String?,
    val lastModified: String?,
)

sealed interface HiddenFilmsFetchResult {
    data class Changed(val state: HiddenFilmsState) : HiddenFilmsFetchResult
    data object NotModified : HiddenFilmsFetchResult
}

/**
 * Per-country hiddenFilms sync — the successor to `UserStateClient`/
 * `/api/me/state` (deleted; this app no longer calls that bulk endpoint at
 * all). Mirrors iOS `HiddenFilmsClient`.
 *
 * Each operation is scoped to ONE country and hits
 * `/api/me/{country}/hidden-films(/{title})`: `fetch` is conditional
 * (`If-None-Match`/`If-Modified-Since`, honouring the stored validators) and
 * may answer [HiddenFilmsFetchResult.NotModified] with no body; `hide`/
 * `unhide`/`clear` are idempotent writes that each echo the RESULTING state
 * plus fresh validators, so a caller that just wrote never needs a
 * follow-up fetch to learn its new ETag.
 */
interface HiddenFilmsClient {
    suspend fun fetch(country: String, etag: String?, lastModified: String?): HiddenFilmsFetchResult
    suspend fun hide(country: String, title: String): HiddenFilmsState
    suspend fun unhide(country: String, title: String): HiddenFilmsState
    suspend fun clear(country: String): HiddenFilmsState
}

/** The server refused a hidden-films write for good — only
 *  `UserStateController`'s own refusals: a 400 (a title over its length bound,
 *  or a country it does not know) or a 413 (the country's bucket is full). No
 *  amount of resending changes either, so the caller stops owing the write.
 *  Every other failure is retried: a 403 in particular is as likely a
 *  Cloudflare challenge in front of the app as anything the app said. Same
 *  rule as iOS `HiddenFilmsWriteRefused` and the web's
 *  `_hiddenFilmsWriteRefused`. */
class HiddenFilmsWriteRefused(val statusCode: Int) : IOException("HTTP $statusCode") {
    companion object {
        fun isPermanent(statusCode: Int): Boolean = statusCode == 400 || statusCode == 413
    }
}

class HttpHiddenFilmsClient(
    internal val baseUrl: String,
    private val client: OkHttpClient,
) : HiddenFilmsClient {

    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun fetch(country: String, etag: String?, lastModified: String?): HiddenFilmsFetchResult =
        withContext(Dispatchers.IO) {
            val builder = Request.Builder().url(urlFor(country)).header("User-Agent", UA)
            // If-None-Match, when present, is authoritative over If-Modified-Since
            // (RFC 7232 §3.3) — only the ETag reflects hiddenFilms specifically, so
            // only send the date-based validator when there's no ETag to send instead.
            when {
                etag != null -> builder.header("If-None-Match", etag)
                lastModified != null -> builder.header("If-Modified-Since", lastModified)
            }
            client.newCall(builder.build()).execute().use { response ->
                if (response.code == 304) HiddenFilmsFetchResult.NotModified
                else {
                    if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
                    HiddenFilmsFetchResult.Changed(parse(response))
                }
            }
        }

    override suspend fun hide(country: String, title: String): HiddenFilmsState =
        write(Request.Builder().url(urlFor(country, title)).put("".toRequestBody(null)))

    override suspend fun unhide(country: String, title: String): HiddenFilmsState =
        write(Request.Builder().url(urlFor(country, title)).delete())

    override suspend fun clear(country: String): HiddenFilmsState =
        write(Request.Builder().url(urlFor(country)).delete())

    private suspend fun write(builder: Request.Builder): HiddenFilmsState = withContext(Dispatchers.IO) {
        val request = builder.header("User-Agent", UA).build()
        client.newCall(request).execute().use { response ->
            if (HiddenFilmsWriteRefused.isPermanent(response.code)) throw HiddenFilmsWriteRefused(response.code)
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            parse(response)
        }
    }

    private fun parse(response: Response): HiddenFilmsState {
        val body = response.body.string()
        val wire = json.decodeFromString<WireHiddenFilms>(body)
        return HiddenFilmsState(wire.hiddenFilms, response.header("ETag"), response.header("Last-Modified"))
    }

    /** `URLEncoder` fixed up for a URL PATH segment: it already escapes `/`
     *  (unlike leaving it as a separator), the one thing that would otherwise
     *  split the URL wrong; the only fixup needed is `+` → `%20` for spaces,
     *  since `URLEncoder` is query-string encoding underneath. Same fixup
     *  `ui/common/Share.kt` already uses for a share-link title. Deliberately
     *  NOT `android.net.Uri.encode` — that one can't be unit-tested without
     *  Robolectric (it returns null under a plain JVM test), and this app's
     *  test suite for this class runs as plain JUnit against a MockWebServer. */
    private fun urlFor(country: String, title: String? = null): String {
        val base = "$baseUrl/api/me/${encode(country)}/hidden-films"
        return if (title == null) base else "$base/${encode(title)}"
    }

    private fun encode(segment: String): String =
        java.net.URLEncoder.encode(segment, "UTF-8").replace("+", "%20")

    @Serializable
    private data class WireHiddenFilms(val hiddenFilms: Set<String> = emptySet())

    private companion object {
        const val UA = "KinowoAndroid/1.0"
    }
}
