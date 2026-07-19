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

  /** Forget every memoised resolution for the film titled `cleanTitle`, so the
   *  next `getOrResolve` genuinely re-resolves.
   *
   *  Lives on the CACHE, not the store: the write-through implementation fronts
   *  the store with its own Caffeine layer, so clearing only the durable side
   *  would leave the stale value in memory and change nothing. */
  def forget(cleanTitle: String): Unit = ()

  /** Forget EVERY memoised resolution for this source. The operator's
   *  corpus-wide refresh button calls this first, so the walk genuinely
   *  re-derives instead of replaying the answers it is meant to re-check. */
  def forgetAll(): Unit = ()
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
 *
 * Every call reports a [[ResolutionOutcome]] to `recorder`, which is how the
 * `kinowo_worker_resolution_total` panel shows what the cache is actually worth
 * per source — chains avoided vs chains run, and how many of the latter came
 * back empty and will therefore run again next cycle.
 */
class WriteThroughResolutionCache(
  store: ResolutionStore,
  recorder: ResolutionOutcomeRecorder = ResolutionOutcomeRecorder.noop) extends ResolutionCache {

  private val cache: Cache[String, String] =
    Caffeine.newBuilder().expireAfterWrite(ResolutionStore.Ttl.toMillis, TimeUnit.MILLISECONDS).build()

  /** The loader runs only on a Caffeine miss, so "loader never ran" IS the
   *  in-memory hit — including for a caller whose concurrent duplicate lost the
   *  race and blocked on the winner's load. Counting that as `hit_memory` is
   *  right for what the counter measures: it did not run a probe chain. */
  override def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = {
    var loaderRan = false
    val value = Option(cache.get(hintKey, _ => {
      loaderRan = true
      loadOrResolve(hintKey, resolve).orNull
    }))
    if (!loaderRan) recorder.record(ResolutionOutcome.HitMemory)
    value
  }

  /** Both layers, in that order: Caffeine first so a concurrent read can't
   *  re-warm it from the row we are about to delete. */
  override def forget(cleanTitle: String): Unit = {
    import scala.jdk.CollectionConverters._
    cache.invalidateAll(cache.asMap().keySet().asScala.filter(ResolutionKeys.belongsTo(_, cleanTitle)).toSeq.asJava)
    store.removeForFilm(cleanTitle)
    ()
  }

  override def forgetAll(): Unit = {
    cache.invalidateAll()
    store.removeAll()
    ()
  }

  private def loadOrResolve(hintKey: String, resolve: => Option[String]): Option[String] =
    store.get(hintKey) match {
      case warm @ Some(_) => recorder.record(ResolutionOutcome.HitStore); warm
      case None =>
        resolve match {
          case hit @ Some(value) =>
            store.put(hintKey, value)
            recorder.record(ResolutionOutcome.MissResolved)
            hit
          case None =>
            recorder.record(ResolutionOutcome.MissUnresolved)
            None
        }
    }
}
