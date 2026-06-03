package pl.kinowo.data

import pl.kinowo.model.Film

/**
 * Decides which cached posters to evict on the daily purge. Pure — no Coil,
 * no Android — so the keep/evict rule is unit-testable on the JVM. The
 * ViewModel pairs it with DataStore (the once-a-day gate + the persisted
 * "seen" set) and Coil's disk/memory cache (the actual eviction).
 *
 * Mirrors iOS `PosterStore.reconcile`: keep the posters of films still in the
 * repertoire — which is pruned to films with a future screening — and drop the
 * rest. Coil's `DiskCache` can't enumerate its keys, so rather than walk the
 * cache we remember the set of poster URLs we've seen and evict the ones that
 * fall out of the repertoire next time.
 */
object PosterCachePurge {
    /** Every poster URL (primary + declared fallbacks) across [films]. */
    fun keepUrls(films: List<Film>): Set<String> =
        films.flatMapTo(LinkedHashSet()) { it.posterChain }

    /**
     * URLs seen before but no longer worth keeping: the posters of films that
     * have left the repertoire (no future screening), plus any URL a
     * still-present film has rotated away from.
     */
    fun toEvict(previouslySeen: Set<String>, keep: Set<String>): Set<String> =
        previouslySeen - keep
}
