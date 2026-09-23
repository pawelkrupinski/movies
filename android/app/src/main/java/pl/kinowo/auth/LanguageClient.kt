package pl.kinowo.auth

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.Serializable
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import java.io.IOException

/**
 * The account's language pick — the one thing still riding the legacy
 * `/api/me/state` document now that hiddenFilms moved to [HiddenFilmsClient]
 * and disabledCinemas is device-local: there's no granular endpoint for a
 * single scalar pick, only for the two sets that used to share this document
 * with it. Mirrors iOS's
 * equivalent split.
 */
interface LanguageClient {
    /** The account's current pick, or null if it hasn't made one. */
    suspend fun fetch(): String?

    /** Push this device's explicit pick as the account's. */
    suspend fun push(language: String)
}

class HttpLanguageClient(
    private val baseUrl: String = "https://kinowo.net",
    private val client: OkHttpClient,
) : LanguageClient {

    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun fetch(): String? = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            json.decodeFromString<WireLanguage>(response.body.string()).language
        }
    }

    // PUT /api/me/state is a partial update — a field this body omits keeps
    // its stored value, so sending `language` alone can never wipe the
    // hiddenFilms/disabledCinemas an older client (or this app, before this
    // split) left there. `language`'s default (null) plus this `Json`'s
    // default `encodeDefaults = false` OMITS the key entirely rather than
    // sending an explicit `null` when there's nothing to push — this method
    // is never actually called with one, but the wire shape stays correct
    // either way: an absent key means "leave the stored pick alone", an
    // explicit `null` means "clear it", and only the server-side caller of
    // THIS endpoint ever wants the latter.
    override suspend fun push(language: String) = withContext(Dispatchers.IO) {
        val payload = json.encodeToString(WireLanguage(language))
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .put(payload.toRequestBody(JSON_MEDIA))
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
        }
    }

    @Serializable
    private data class WireLanguage(val language: String? = null)

    private companion object {
        const val UA = "KinowoAndroid/1.0"
        val JSON_MEDIA = "application/json".toMediaType()
    }
}
