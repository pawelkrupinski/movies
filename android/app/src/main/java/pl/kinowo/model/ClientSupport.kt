package pl.kinowo.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

/**
 * The server's `GET /api/client-support` answer: per platform, the oldest app
 * version it still serves and where to send anyone older. Only [android] is read
 * here; [ios] is decoded so the payload's shape is pinned, nothing more.
 *
 * Mirrors Scala `models.ClientSupport` — the reference the Kotlin and Swift gates
 * are written against.
 */
@Serializable
data class ClientSupport(
    val android: Platform? = null,
    val ios: Platform? = null,
) {
    /** One platform's rule. GATED only when both halves are present: a minimum
     *  with nowhere to send people would strand them, and a store URL with no
     *  minimum gates nobody. */
    @Serializable
    data class Platform(
        val minimumVersion: String? = null,
        val storeUrl: String? = null,
    ) {
        val isGated: Boolean get() = minimumVersion != null && storeUrl != null
    }

    companion object {
        private val json = Json { ignoreUnknownKeys = true }

        /** Parse a `/api/client-support` 200 body, or null when it doesn't decode. */
        fun parse(body: String): ClientSupport? =
            runCatching { json.decodeFromString<ClientSupport>(body) }.getOrNull()
    }
}

/**
 * Dotted numeric version comparison and the one rule that decides whether this
 * client is still allowed in — a line-for-line mirror of Scala
 * `models.ClientVersion`, so "is 1.10 newer than 1.9" has one answer everywhere.
 */
object ClientVersion {

    /** `"1.2.3"` → `[1, 2, 3]`; null for anything non-numeric. `"1.0"` and
     *  `"1.0.0"` both parse and compare equal. */
    fun parse(version: String): List<Int>? {
        val parts = version.trim().split('.')
        if (parts.isEmpty() || !parts.all { p -> p.isNotEmpty() && p.all { it in '0'..'9' } }) return null
        return parts.map { it.toIntOrNull() ?: return null }
    }

    /** Whether [version] is at least [minimum], comparing component-wise with
     *  missing components read as 0 (1.10 > 1.9, 1.0 == 1.0.0); null when either
     *  doesn't parse. */
    fun isAtLeast(version: String, minimum: String): Boolean? {
        val v = parse(version) ?: return null
        val m = parse(minimum) ?: return null
        val width = maxOf(v.size, m.size)
        for (i in 0 until width) {
            val a = v.getOrElse(i) { 0 }
            val b = m.getOrElse(i) { 0 }
            if (a != b) return a > b
        }
        return true
    }

    /**
     * Unsupported ONLY when [platform] is gated AND [version] parses AND it is
     * below the minimum. Anything unknown — no rule, an unparseable version — is
     * SUPPORTED: locking someone out of a working app is the worse failure.
     */
    fun isSupported(version: String, platform: ClientSupport.Platform?): Boolean {
        val min = platform?.minimumVersion
        if (platform == null || min == null || !platform.isGated) return true
        return isAtLeast(version, min) ?: true
    }
}
