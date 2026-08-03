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
 * Whether an EMPTY resolution is cached too is the caller's choice, made at the
 * composition root via [[UnresolvedPolicy]] — see there for which sources take
 * which and why.
 */
trait ResolutionCache {
  /** The cached value for `hintKey`, or run `resolve` and cache+return its
   *  result. Concurrent calls with the same key collapse to a single `resolve`.
   *  Whether a None result is remembered depends on the cache's
   *  [[UnresolvedPolicy]]. */
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
 * What a [[ResolutionCache]] does with a probe chain that came back EMPTY.
 * Chosen per source at the composition root, because the two answers are right
 * for genuinely different reasons.
 */
sealed trait UnresolvedPolicy

object UnresolvedPolicy {
  /** Run the chain again next cycle. Right where an empty answer is expected to
   *  turn into a real one without anything about the film changing: TMDB indexes
   *  the film a few hours after we first look for it, and `UnresolvedTmdbReaper`
   *  exists to catch exactly that. Remembering those misses would put a day
   *  between a film appearing upstream and us seeing it. */
  case object Retry extends UnresolvedPolicy

  /** Remember the miss for the store's TTL, so the chain runs once a day rather
   *  than once per refresh.
   *
   *  Right for the rating-link probes (Metacritic, Rotten Tomatoes, Filmweb),
   *  where all three of these hold: the chain is long (up to ~20 sequential GETs
   *  for MC/RT, ~55 for Filmweb), an empty answer is the COMMON answer (97% of
   *  Metacritic resolutions in production), and "this site has no page for this
   *  film" is stable — a day-late rating badge costs nothing.
   *
   *  Untouched by this: an operator's forced re-enrich, which goes through
   *  `forget`/`forgetAll` and drops remembered misses along with the hits, so
   *  the escape hatch still genuinely re-probes. */
  case object Remember extends UnresolvedPolicy
}

/**
 * In-memory (Caffeine, 24h `expireAfterWrite`) write-through to a
 * [[ResolutionStore]]. The Caffeine layer absorbs the hot path; the store gives
 * persistence across restarts and the cross-process source of truth.
 *
 * `getOrResolve` leans on Caffeine's `get(key, loader)`: it runs the loader at
 * most once per key even under concurrent misses (collapsing the thundering
 * herd). The loader checks the durable store first (warming a cold Caffeine
 * after a restart), then resolves live and writes through.
 *
 * `unresolved` decides what happens when the chain comes back empty — see
 * [[UnresolvedPolicy]]. Under `Remember`, the miss is stored as an EMPTY value,
 * which both layers hold like any other entry (so it expires on the same TTL and
 * `forget`/`forgetAll` drop it) and which `getOrResolve` maps back to None on
 * the way out. A resolver that hands back `Some("")` is treated as unresolved:
 * an empty id or url is never a usable answer, and letting one through would be
 * indistinguishable from the marker.
 *
 * Every call reports a [[ResolutionOutcome]] to `recorder`, which is how the
 * `kinowo_worker_resolution_total` panel shows what the cache is actually worth
 * per source — chains avoided vs chains run, and how many of the latter came
 * back empty. A remembered miss served from either layer counts as a `hit_*`:
 * what the counter measures is whether a probe chain ran, and it did not.
 */
class WriteThroughResolutionCache(
  store: ResolutionStore,
  recorder: ResolutionOutcomeRecorder = ResolutionOutcomeRecorder.noop,
  unresolved: UnresolvedPolicy = UnresolvedPolicy.Retry) extends ResolutionCache {

  // Take the rules from the store rather than accepting a second copy: the
  // in-memory cache and the durable store hold the SAME hint keys, so folding
  // them differently would make an invalidation miss the row it meant to clear.
  private given services.movies.TitleNormalizer = store.normalizer

  private val cache: Cache[String, String] =
    Caffeine.newBuilder().expireAfterWrite(ResolutionStore.Ttl.toMillis, TimeUnit.MILLISECONDS).build()

  /** The stored stand-in for "this chain ran and found nothing". Empty because
   *  no real resolution can be empty, so it can never be mistaken for one. */
  private val Unresolved = ""

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
    value.filter(_.nonEmpty)
  }

  /** Both layers, in that order: Caffeine first so a concurrent read can't
   *  re-warm it from the row we are about to delete. */
  override def forget(cleanTitle: String): Unit = {
    import scala.jdk.CollectionConverters._
    cache.invalidateAll(cache.asMap().keySet().asScala.filter(ResolutionKeys.belongsTo(_, cleanTitle, store.normalizer)).toSeq.asJava)
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
        resolve.filter(_.nonEmpty) match {
          case hit @ Some(value) =>
            store.put(hintKey, value)
            recorder.record(ResolutionOutcome.MissResolved)
            hit
          case None =>
            recorder.record(ResolutionOutcome.MissUnresolved)
            unresolved match {
              // Cached as the empty marker in BOTH layers — Caffeine holds what
              // the loader returns, so returning it here is what stops the next
              // call in this process from re-probing.
              case UnresolvedPolicy.Remember => store.put(hintKey, Unresolved); Some(Unresolved)
              case UnresolvedPolicy.Retry    => None
            }
        }
    }
}
