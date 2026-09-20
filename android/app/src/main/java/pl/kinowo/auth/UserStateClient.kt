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

/**
 * The personalization state that round-trips to `/api/me/state`. hiddenFilms
 * ONLY — disabledCinemas stopped being a server-synced field (it's
 * device-local now, see [pl.kinowo.data.UserPreferences]). `/api/me/state`
 * still accepts/returns disabledCinemas server-side, for whatever older app
 * build still sends it; this client just no longer models that half of the
 * payload.
 */
data class UserSyncState(
    val hiddenFilms: Set<String>,
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
            UserSyncState(wire.hiddenFilms)
        }
    }

    override suspend fun putState(state: UserSyncState) = withContext(Dispatchers.IO) {
        val payload = json.encodeToString(WireState(state.hiddenFilms))
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .put(payload.toRequestBody(JSON_MEDIA))
            .build()
        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) throw IOException("HTTP ${response.code}")
        }
    }

    // PUT /api/me/state is a partial update server-side — a field this class
    // doesn't model (disabledCinemas, now — selectedMovies/favouriteRooms
    // before it) is simply never sent, so its stored value is preserved
    // rather than wiped by this client's writes. `ignoreUnknownKeys` above
    // handles the read side symmetrically: a response still carrying
    // disabledCinemas (for older clients) decodes fine and is dropped.
    // Mirrors iOS's client.
    @Serializable
    private data class WireState(
        val hiddenFilms: Set<String> = emptySet(),
    )

    private companion object {
        const val UA = "KinowoAndroid/1.0"
        val JSON_MEDIA = "application/json".toMediaType()
    }
}
