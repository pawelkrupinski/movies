package pl.kinowo.auth

import android.content.Context
import java.security.MessageDigest
import java.security.SecureRandom
import java.util.Base64

/**
 * The PKCE-style secret behind a web sign-in (RFC 7636, S256 only).
 *
 * [startWebSignIn][AuthRepository.startWebSignIn] makes a fresh verifier, keeps
 * it, and sends only its [challenge] to `/auth/:provider/start`; the server
 * stamps that challenge on the one-shot code the `kinowo://auth-done` deep
 * link carries, and `/auth/exchange` spends the code only for this verifier.
 * A code minted by anybody else's flow — a link an attacker sends, carrying a
 * code for THEIR account — is worthless to this app. Same scheme as iOS
 * `PkceVerifier`.
 */
object PkceVerifier {
    private val encoder = Base64.getUrlEncoder().withoutPadding()

    /** 32 bytes from a [SecureRandom], base64url: 43 characters of `[A-Za-z0-9_-]`. */
    fun newVerifier(random: SecureRandom = SecureRandom()): String =
        encoder.encodeToString(ByteArray(32).also(random::nextBytes))

    /** base64url(SHA-256(verifier)), unpadded — what the server compares. */
    fun challenge(verifier: String): String =
        encoder.encodeToString(MessageDigest.getInstance("SHA-256").digest(verifier.toByteArray(Charsets.US_ASCII)))
}

/** Where the verifier waits between launching the Custom Tab and the deep link
 *  coming back — which may be a different PROCESS, since Android can kill the
 *  app while the browser is in front. */
interface PendingVerifierStore {
    fun put(verifier: String)
    /** The waiting verifier, removed: one sign-in, one exchange. */
    fun take(): String?
}

class SharedPrefsPendingVerifierStore(context: Context) : PendingVerifierStore {
    private val prefs = context.getSharedPreferences("kinowo_auth", Context.MODE_PRIVATE)

    override fun put(verifier: String) {
        prefs.edit().putString(KEY, verifier).apply()
    }

    override fun take(): String? = prefs.getString(KEY, null).also { prefs.edit().remove(KEY).apply() }

    private companion object {
        const val KEY = "pendingVerifier"
    }
}
