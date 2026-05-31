package pl.kinowo.auth

import android.content.Context
import android.net.Uri
import androidx.browser.customtabs.CustomTabsIntent
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.withContext
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import pl.kinowo.net.PersistentCookieJar

/**
 * Owns the signed-in session — the Android counterpart of iOS `AuthService`.
 *
 * Sign-in mirrors iOS exactly: open a web OAuth flow (Google / Facebook) in a
 * Custom Tab, let the server bounce back to the `kinowo://auth-done?code=…`
 * deep link, then redeem that one-shot code at `/auth/exchange` for a session
 * cookie. The cookie lands in the shared [PersistentCookieJar] (same
 * [OkHttpClient] used for every call), so the session is carried on
 * `/api/me`, `/api/me/state`, etc., and survives app restarts — the parity
 * for iOS's `HTTPCookieStorage`.
 */
class AuthRepository(
    private val client: OkHttpClient,
    private val cookieJar: PersistentCookieJar,
    private val baseUrl: String = "https://kinowo.fly.dev",
) {
    private val json = Json { ignoreUnknownKeys = true }

    private val _user = MutableStateFlow<UserProfile?>(null)
    val user: StateFlow<UserProfile?> = _user.asStateFlow()

    /** Open the provider's web consent flow. The result returns via the
     *  `kinowo://auth-done` deep link → [exchangeCode]. */
    fun startWebSignIn(context: Context, provider: String) {
        val url = "$baseUrl/auth/$provider/start?platform=android"
        CustomTabsIntent.Builder().build().launchUrl(context, Uri.parse(url))
    }

    /** Redeem the one-shot code from the deep link for a session. */
    suspend fun exchangeCode(code: String) = withContext(Dispatchers.IO) {
        val payload = json.encodeToString(CodeRequest(code))
        val request = post("auth/exchange", payload)
        client.newCall(request).execute().use { resp ->
            val body = resp.body?.string()
            if (resp.isSuccessful && body != null) {
                _user.value = json.decodeFromString<UserProfile>(body)
            }
        }
    }

    /** Re-hydrate the user from a cookie persisted across launches. */
    suspend fun checkSession() = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/me")
            .header("User-Agent", UA)
            .build()
        runCatching {
            client.newCall(request).execute().use { resp ->
                val body = resp.body?.string()
                if (resp.isSuccessful && body != null) {
                    _user.value = json.decodeFromString<UserProfile>(body)
                }
            }
        }
        Unit
    }

    suspend fun signOut() = withContext(Dispatchers.IO) {
        runCatching { client.newCall(post("auth/logout", "")).execute().close() }
        clearSession()
    }

    suspend fun deleteAccount() = withContext(Dispatchers.IO) {
        val request = Request.Builder()
            .url("$baseUrl/api/me")
            .header("User-Agent", UA)
            .delete()
            .build()
        runCatching { client.newCall(request).execute().close() }
        clearSession()
    }

    private fun clearSession() {
        _user.value = null
        cookieJar.clear()
    }

    private fun post(path: String, jsonBody: String): Request =
        Request.Builder()
            .url("$baseUrl/$path")
            .header("User-Agent", UA)
            .post(jsonBody.toRequestBody(JSON_MEDIA))
            .build()

    @Serializable
    private data class CodeRequest(val code: String)

    private companion object {
        const val UA = "KinowoAndroid/1.0"
        val JSON_MEDIA = "application/json".toMediaType()
    }
}
