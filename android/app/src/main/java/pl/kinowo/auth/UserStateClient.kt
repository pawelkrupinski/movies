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

/** The personalization state that round-trips to `/api/me/state`. */
data class UserSyncState(
    val hiddenFilms: Set<String>,
    val disabledCinemas: Set<String>,
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
    private val baseUrl: String = "https://kinowo.fly.dev",
    private val client: OkHttpClient,
) : UserStateClient {

    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun fetchState(): UserSyncState = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .build()
        client.newCall(request).execute().use { resp ->
            if (!resp.isSuccessful) throw IOException("HTTP ${resp.code}")
            val body = resp.body?.string() ?: throw IOException("empty body")
            val wire = json.decodeFromString<WireState>(body)
            UserSyncState(wire.hiddenFilms, wire.disabledCinemas)
        }
    }

    override suspend fun putState(state: UserSyncState) = withContext(Dispatchers.IO) {
        val payload = json.encodeToString(WireState(state.hiddenFilms, state.disabledCinemas))
        val request = Request.Builder()
            .url("$baseUrl/api/me/state")
            .header("User-Agent", UA)
            .put(payload.toRequestBody(JSON_MEDIA))
            .build()
        client.newCall(request).execute().use { resp ->
            if (!resp.isSuccessful) throw IOException("HTTP ${resp.code}")
        }
    }

    // We send/read only the two sets the mobile UI has. The server's wire
    // shape also carries selectedMovies/favouriteRooms (the web /plan picks);
    // we mirror iOS's client exactly and omit them. (Note: the server's PUT
    // defaults any missing array to empty, so this matches iOS in also not
    // preserving those /plan-only fields — mobile never sets them anyway.)
    @Serializable
    private data class WireState(
        val hiddenFilms: Set<String> = emptySet(),
        val disabledCinemas: Set<String> = emptySet(),
    )

    private companion object {
        const val UA = "KinowoAndroid/1.0"
        val JSON_MEDIA = "application/json".toMediaType()
    }
}
