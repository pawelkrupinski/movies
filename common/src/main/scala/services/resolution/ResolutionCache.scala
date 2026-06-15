package services.resolution

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

/**
 * Caches the result of an expensive identity resolution — `hintKey` → a TMDB
 * id, an IMDb id, or a Filmweb/RT/Metacritic url — so that two cinema rows (or
 * two scrape cycles) reporting the SAME hints resolve once and reuse the
 * answer, instead of hitting the upstream live each time.
 *
 * The key is a deterministic, order-independent string built by
 * [[ResolutionKeys]] from exactly the hints a source's resolver consumes. Two
 * rows whose hints differ (a different director, year, or title) get different
 * keys and resolve independently — the dedup is PER hint-combination, which is
 * why staging no longer merges hints before resolving (the merge happens later,
 * at fold/settle).
 *
 * HITS ONLY: a resolver that returns None is not cached, so a film that isn't
 * yet resolvable is retried next cycle rather than remembered as a miss.
 */
trait ResolutionCache {
  /** The cached value for `hintKey`, or run `resolve` and cache+return its
   *  result. A None result is NOT cached. Concurrent calls with the same key
   *  collapse to a single `resolve`. */
  def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String]
}

object ResolutionCache {
  /** A cache that never stores — every call resolves live. The default for
   *  tests/scripts that don't wire a real cache, so behaviour is identical to
   *  the pre-cache code path. */
  val passthrough: ResolutionCache = new ResolutionCache {
    def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = resolve
  }
}

/**
 * In-memory (Caffeine, 24h `expireAfterWrite`) write-through to a
 * [[ResolutionStore]]. The Caffeine layer absorbs the hot path; the store gives
 * persistence across restarts and the cross-process source of truth.
 *
 * `getOrResolve` leans on Caffeine's `get(key, loader)`: it runs the loader at
 * most once per key even under concurrent misses (collapsing the thundering
 * herd), and a `null` loader return is NOT cached — which is exactly the
 * hits-only contract. The loader checks the durable store first (warming a cold
 * Caffeine after a restart), then resolves live and writes through on a hit.
 */
class WriteThroughResolutionCache(store: ResolutionStore) extends ResolutionCache {
  private val cache: Cache[String, String] =
    Caffeine.newBuilder().expireAfterWrite(ResolutionStore.Ttl.toMillis, TimeUnit.MILLISECONDS).build()

  override def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] =
    Option(cache.get(hintKey, _ => loadOrResolve(hintKey, resolve).orNull))

  private def loadOrResolve(hintKey: String, resolve: => Option[String]): Option[String] =
    store.get(hintKey).orElse {
      resolve match {
        case hit @ Some(value) => store.put(hintKey, value); hit
        case None              => None
      }
    }
}
