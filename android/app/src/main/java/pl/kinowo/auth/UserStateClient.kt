package pl.kinowo.auth

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import java.io.IOException

/** The personalization state that round-trips to `/api/me/state`.
 *
 *  [language] is a single explicit pick, not a set like the other two
 *  fields, and defaults to null so existing 2-arg call sites keep
 *  compiling. Unlike them, it should never carry the RESOLVED default a
 *  visitor never chose — see [StateSyncService]'s merge for why. */
data class UserSyncState(
    val hiddenFilms: Set<String>,
    val disabledCinemas: Set<String>,
    val language: String? = null,
)

/**
 * Reads/writes the signed-in user's [UserSyncState]. The interface is what
 * [StateSyncService] depends on so tests can swap an in-memory fake; the
 * HTTP implementation is the only production binding. Mirrors iOS
 * `UserStateClient`.
 */
interface UserStateClient {
    suspend fun fetchState(): UserSyncState
    suspend fun putState(state: UserSyncState)
}

class HttpUserStateClient(
    private val baseUrl: String = "https://kinowo.net",
    private val client: OkHttpClient,
) : UserStateClient {

    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun fetchState(): UserSyncState = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
            val body = response.body?.string() ?: throw IOException("empty body")
            val wire = json.decodeFromString<WireState>(body)
            UserSyncState(wire.hiddenFilms, wire.disabledCinemas, wire.language)
        }
    }

    override suspend fun putState(state: UserSyncState) = withContext(Dispatchers.IO) {
        val payload = json.encodeToString(WireState(state.hiddenFilms, state.disabledCinemas, state.language))
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .put(payload.toRequestBody(JSON_MEDIA))
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
        }
    }

    // We send/read only the sets the mobile UI models, and PUT /api/me/state is a
    // partial update — omitted fields keep their stored value — so a field this
    // client does not model is preserved rather than wiped. That mattered when the
    // web carried two the apps did not (retired with the plan page) and still does
    // for the next one. Mirrors iOS's client.
    //
    // `language`'s default (null) plus this `Json`'s default `encodeDefaults =
    // false` means a null value is OMITTED from the encoded body, never sent as
    // an explicit JSON `null` — which matters, because the server treats an
    // absent key as "leave the stored pick alone" and an explicit `null` as
    // "clear it". Decoding treats a missing key and a JSON `null` alike, both
    // landing as `null` — exactly the server's "no pick yet" case.
    @Serializable
    private data class WireState(
        val hiddenFilms: Set<String> = emptySet(),
        val disabledCinemas: Set<String> = emptySet(),
        val language: String? = null,
    )

    private companion object {
        const val UA = "KinowoAndroid/1.0"
        val JSON_MEDIA = "application/json".toMediaType()
    }
}
