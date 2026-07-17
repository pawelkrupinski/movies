package services.movies

import com.github.benmanes.caffeine.cache.Caffeine

/** Interns synopsis strings so a film shown at N cinemas — each cinema slot carrying the
 *  same editorial blurb (a chain-wide press synopsis) as a DISTINCT `String` — holds ONE
 *  shared instance instead of N byte-identical copies. The resident MovieCache duplicated
 *  these N× per film across `SourceData.synopsis` slots (+ the `retainedSynopses` copy,
 *  which references the interned slot instance and so dedups for free). Interning at the
 *  single write boundary (`MovieCache.buildCinemaSlot`) collapses them all.
 *
 *  Bounded (a plain `ConcurrentHashMap` would hold every synopsis a film ever had, forever)
 *  so stale blurbs from films that left the listings are evicted. Not `String.intern()` —
 *  that pins long text in native memory with no eviction. */
object SynopsisPool {

  private val pool = Caffeine.newBuilder()
    .maximumSize(20000L)   // ≫ the ~6-7k-film distinct-synopsis working set; each ≲1KB → ≲20MB cap
    .build[String, String]()

  /** The canonical instance for a synopsis: the first equal string interned wins, so all
   *  byte-identical blurbs across a film's cinema slots share one object. */
  def canonical(s: String): String = pool.get(s, (k: String) => k)
}
