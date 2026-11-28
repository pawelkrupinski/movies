package pl.kinowo.data

import java.time.Duration
import java.time.Instant

/** When a repository last reloaded, and whether that is recent enough to skip a reload. */
internal class ReloadFreshness(private val staleAfter: Duration = Duration.ofSeconds(60)) {
    private var reloadedAt: Instant? = null

    fun markReloaded(now: Instant) {
        reloadedAt = now
    }

    fun isFresh(now: Instant): Boolean {
        val last = reloadedAt ?: return false
        return Duration.between(last, now) < staleAfter
    }
}
