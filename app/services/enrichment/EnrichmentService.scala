package services.enrichment

import clients.TmdbClient
import play.api.Logging
import services.TitleOverrides
import services.events.{DomainEvent, MovieAdded}

import java.text.Normalizer
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeUnit}
import models.Enrichment
import scala.util.{Failure, Success, Try}

/**
 * Resolves `(title, year)` → `Enrichment` (IMDB id, IMDB rating, original
 * title, Filmweb data, Metacritic/RT URLs). Each lookup runs Polish-title →
 * TMDB → IMDB id, then cross-references IMDb + Filmweb + URL validators
 * for the rest.
 *
 * The service exposes a single trigger entry point — `enqueue(title, year)` —
 * which dispatches a background lookup if the key isn't already cached, in
 * flight, or in the negative cache. Page renders are pure reads against
 * `EnrichmentCache`; they never schedule work. Event wiring (subscribing to
 * `MovieAdded` and calling `enqueue`) lives in `modules.EventWiring` so this
 * class is unaware of who triggers it.
 *
 * Single bounded worker pool drains the lookup queue so callers aren't blocked
 * on network round-trips.
 */
class EnrichmentService(
  cache:      EnrichmentCache,
  tmdb:       TmdbClient,
  filmweb:    FilmwebClient,
  imdb:       ImdbClient,
  metacritic: MetacriticClient,
  rt:         RottenTomatoesClient
) extends Logging {

  // Active or queued lookups, so we don't dispatch the same key twice.
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()

  // Bounded pool so a burst of `MovieAdded` events for a fresh catalogue takes
  // seconds, not minutes — each lookup is several serial HTTP round-trips
  // (TMDB search, TMDB external_ids, IMDb, Filmweb, MC/RT URL validators) so
  // almost all the time is spent waiting on the network. 10 in-flight is well
  // under TMDB's published rate limit (~50 req/s).
  private val EnrichmentWorkers = 10
  private val workerCounter     = new java.util.concurrent.atomic.AtomicInteger(0)
  private val worker = Executors.newFixedThreadPool(EnrichmentWorkers, { r: Runnable =>
    val t = new Thread(r, s"enrichment-worker-${workerCounter.incrementAndGet()}")
    t.setDaemon(true)
    t
  })

  // Separate scheduler for delayed retries — it just hands a Runnable back to
  // the worker pool when the timer fires, so we don't tie up a worker thread
  // sleeping. Daemon so it doesn't keep the JVM alive.
  private val retryScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "enrichment-retry-scheduler"); t.setDaemon(true); t
  }

  // IMDb's GraphQL is fast and free; refresh ratings hourly so the live numbers
  // stay close to what users see on imdb.com. Runs sequentially on its own
  // thread — for a few hundred films one tick takes <1 minute.
  private val imdbRefreshScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "imdb-refresh"); t.setDaemon(true); t
  }
  // Small startup delay so the first refresh fires AFTER Mongo hydration
  // populates the positive cache (and the scheduler doesn't race app boot).
  private val StartupDelaySeconds = 10L
  private val ImdbRefreshHours    = 1L

  // How many times we've retried each key after a transient failure. Cleared
  // on success / non-transient miss. Caps a runaway loop for a key that
  // perpetually fails some non-cacheable way.
  private val retryAttempts = new ConcurrentHashMap[CacheKey, Integer]()
  private val MaxRetries    = 6

  /** Schedule the periodic IMDb refresh. Called from `AppLoader` so the class
   *  remains lifecycle-agnostic (tests can construct it without sprouting a
   *  background thread). */
  def start(): Unit = {
    val intervalSeconds = ImdbRefreshHours * 3600
    logger.info(s"IMDb refresh scheduled: first run in ${StartupDelaySeconds}s, then every ${ImdbRefreshHours}h.")
    imdbRefreshScheduler.scheduleAtFixedRate(
      () => Try(refreshImdbRatings()).recover {
        case ex => logger.warn(s"IMDb refresh tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, intervalSeconds, TimeUnit.SECONDS
    )
  }

  /** Drain the queue so in-flight upserts hit Mongo before `EnrichmentRepo`
   *  closes its client. The caller (`AppLoader`) must register this so that
   *  Play runs the repo's close hook strictly *after* this returns. */
  def stop(): Unit = {
    worker.shutdown()
    retryScheduler.shutdown()
    imdbRefreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }

  /** Ensure `(title, year)` gets enriched in the background. No-op if the key
   *  is already cached, in flight, or in the negative cache. */
  def enqueue(title: String, year: Option[Int]): Unit =
    scheduleLookup(cache.keyOf(title, year))

  /** Subscribe this on the `EventBus` to drive enrichment from `MovieAdded`
   *  events. Defined as a partial function so the bus's `applyOrElse` simply
   *  ignores events of other types. */
  val onMovieAdded: PartialFunction[DomainEvent, Unit] = {
    case MovieAdded(title, year) => enqueue(title, year)
  }

  /** Force a re-enrichment for `(title, year)`.
   *
   *  Always re-resolves via TMDB title search — the cinema's `releaseYear` is
   *  frequently the *scheduling* year (when a screening happens), not the
   *  film's production year. First-time enrichment can pick a year-coincidental
   *  candidate over the title-correct famous film; re-enrichment is the
   *  recovery path. For an ambiguous Polish title like "Powrót do przyszłości"
   *  (anniversary screening of Back to the Future), the year-less fallback
   *  inside `tmdb.search` picks the popular exact-title match — the film the
   *  cinema is actually screening.
   *
   *  Writes each stage through to cache + Mongo as soon as the data arrives so
   *  a debug-page reload mid-flight shows the partial state. Runs on the
   *  worker pool — returns immediately. Tests that want synchronous results
   *  call `reEnrichSync` instead.
   */
  def reEnrich(title: String, year: Option[Int]): Unit = {
    val key = cache.keyOf(title, year)
    worker.execute(() => reEnrichKey(key))
  }

  /** Same as `reEnrich` but blocks the calling thread and returns the final
   *  enrichment (or None if TMDB has no hit). Intended for scripts that need
   *  a synchronous answer — no scheduler, no events, no callbacks.
   */
  def reEnrichSync(title: String, year: Option[Int]): Option[Enrichment] =
    reEnrichKey(cache.keyOf(title, year))

  // Shared engine. Resolves TMDB *first* and only invalidates the cache once
  // we have a fresh hit — so a transient TMDB failure can never blank an
  // existing row.
  private def reEnrichKey(key: CacheKey): Option[Enrichment] = {
    resolveTmdb(key.cleanTitle, key.year).map { case (hit, imdbId) =>
      logger.info(s"Re-enriching ${key.cleanTitle} (${key.year.getOrElse("?")}) → $imdbId")
      cache.invalidate(key)

      // Stage 1: TMDB hit + IMDb id — write the skeleton so the row reappears
      // immediately, even before any rating/URL stages have run.
      var current = Enrichment(
        imdbId        = imdbId,
        imdbRating    = None,
        metascore     = None,
        originalTitle = hit.originalTitle,
        tmdbId        = Some(hit.id)
      )
      cache.put(key, current)

      val linkTitle = hit.originalTitle.getOrElse(key.cleanTitle)

      // Stage 2: IMDb rating
      Try(imdb.lookup(imdbId)).toOption.flatten.foreach { rating =>
        current = current.copy(imdbRating = Some(rating))
        cache.put(key, current)
      }

      // Stage 3: Filmweb (URL + rating arrive together)
      Try(filmweb.lookup(key.cleanTitle, key.year)).toOption.flatten.foreach { fw =>
        current = current.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating)
        cache.put(key, current)
      }

      // Stage 4: Metacritic canonical URL. Pass cleanTitle as a fallback so
      // films whose TMDB originalTitle slugs poorly (Japanese / wrong-language
      // match / TMDB mismatch) still resolve when the Polish title happens to
      // line up with MC's slug.
      val mcFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
      Try(metacritic.urlFor(linkTitle, mcFallback)).toOption.flatten.foreach { url =>
        current = current.copy(metacriticUrl = Some(url))
        cache.put(key, current)
      }

      // Stage 5: Rotten Tomatoes canonical URL
      Try(rt.urlFor(linkTitle)).toOption.flatten.foreach { url =>
        current = current.copy(rottenTomatoesUrl = Some(url))
        cache.put(key, current)
      }

      current
    }
  }

  /**
   * Read what we have right now. Pure cache lookup — never blocks, never
   * schedules. Misses return None; the next `MovieAdded` event re-triggers
   * a background fetch.
   */
  def get(title: String, year: Option[Int]): Option[Enrichment] =
    cache.get(cache.keyOf(title, year))

  /** Snapshot of every cached enrichment — for debug tooling. */
  def snapshot(): Seq[(String, Option[Int], Enrichment)] = cache.snapshot()

  private def scheduleLookup(key: CacheKey): Unit = {
    if (cache.get(key).isDefined) return               // already enriched
    if (cache.isNegative(key)) return                  // known miss, retry after TTL
    if (pending.add(key))
      worker.execute(() => try lookup(key) finally pending.remove(key))
  }

  private def lookup(key: CacheKey): Unit =
    fetchEnrichment(key.cleanTitle, key.year) match {
      case Success(Some(e)) =>
        cache.put(key, e)
        retryAttempts.remove(key)
        logger.debug(s"Enriched ${key.cleanTitle} (${key.year.getOrElse("?")}) → ${e.imdbId}")
      case Success(None)    =>
        cache.markMissing(key)
        retryAttempts.remove(key)
      case Failure(ex)      =>
        // Transient failure (rate limit / network blip). Don't poison the
        // negative cache — schedule a delayed retry so the title gets enriched
        // automatically once the upstream recovers, instead of waiting for
        // someone to revisit the page.
        scheduleRetry(key, ex)
    }

  private def scheduleRetry(key: CacheKey, cause: Throwable): Unit = {
    val attempt = retryAttempts.merge(key, 1: Integer, (a: Integer, b: Integer) => a + b)
    if (attempt > MaxRetries) {
      logger.warn(s"Giving up on ${key.cleanTitle} (${key.year.getOrElse("?")}) after $attempt attempts: ${cause.getMessage}")
      retryAttempts.remove(key)
      cache.markMissing(key)  // Stop hammering it for an hour.
      return
    }
    val delaySeconds = retryDelaySeconds(cause, attempt)
    logger.warn(s"Enrichment lookup failed for ${key.cleanTitle} (${key.year.getOrElse("?")}) on attempt $attempt: ${cause.getMessage}; retrying in ${delaySeconds}s")
    retryScheduler.schedule(new Runnable {
      def run(): Unit = scheduleLookup(key)
    }, delaySeconds, TimeUnit.SECONDS)
  }

  // Exponential backoff for transient upstream failures (TMDB / IMDb /
  // Filmweb network blips). 30s → 1m → 2m → 4m … capped at 30 min.
  private def retryDelaySeconds(cause: Throwable, attempt: Int): Long = {
    val _ = cause  // currently unused — retained for future per-cause tuning
    math.min(30L * 60, 30L * (1L << (attempt - 1)))
  }

  /** Walks every cached enrichment on each tick, asks IMDb's GraphQL CDN for
   *  its current rating, and writes back to cache + Mongo only when the value
   *  actually changed. Per-entry failures are swallowed so one bad imdbId
   *  doesn't poison the whole tick.
   *
   *  Scheduled by `start()`; the first run fires `StartupDelaySeconds` after,
   *  giving Mongo hydration time to populate the cache. */

  // A stored URL is always a canonical /movie/ or /m/ page — search URLs are
  // never persisted (see CLAUDE.md). Only rows with no URL yet get re-probed.
  private def needsMetacriticRetry(url: Option[String]): Boolean = url.isEmpty
  private def needsRtRetry(url: Option[String]): Boolean         = url.isEmpty

  private[services] def refreshImdbRatings(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    logger.info(s"IMDb refresh: starting tick over ${snapshot.size} cached enrichment(s).")
    var changed     = 0
    var failed      = 0
    var urlsFilled  = 0
    snapshot.foreach { case (key, enrichment) =>
      val freshImdb = Try(imdb.lookup(enrichment.imdbId))
      val withImdb = freshImdb match {
        case Success(fresh) if fresh != enrichment.imdbRating =>
          logger.debug(s"IMDb refresh: ${key.cleanTitle} ${enrichment.imdbId} ${enrichment.imdbRating.getOrElse("—")} → ${fresh.getOrElse("—")}")
          enrichment.copy(imdbRating = fresh)
        case Success(_) => enrichment
        case Failure(ex) =>
          failed += 1
          logger.debug(s"IMDb refresh: ${enrichment.imdbId} lookup failed: ${ex.getMessage}")
          enrichment
      }
      // Refresh Metacritic + RT URLs. We only persist canonical /movie/ and /m/
      // URLs now. If a stored URL is None or a legacy /search/ fallback we
      // retry — and if the retry also can't resolve a canonical page, we
      // *clear* the field (the view layer falls back to a synthesised search
      // link on the fly). This is the backfill path for records that were
      // written under the old "populate even with search URL" logic.
      val linkTitle = withImdb.originalTitle.getOrElse(key.cleanTitle)
      val mcFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None
      val withMc = if (needsMetacriticRetry(withImdb.metacriticUrl)) {
        val fresh = Try(metacritic.urlFor(linkTitle, mcFallback)).toOption.flatten
        if (fresh != withImdb.metacriticUrl) {
          fresh match {
            case Some(url) =>
              urlsFilled += 1
              logger.debug(s"IMDb refresh: ${withImdb.metacriticUrl.fold("backfilled")(_ => "upgraded")} Metacritic URL for ${key.cleanTitle} → $url")
            case None =>
              logger.debug(s"IMDb refresh: cleared non-canonical Metacritic URL for ${key.cleanTitle} (was ${withImdb.metacriticUrl.getOrElse("—")})")
          }
          withImdb.copy(metacriticUrl = fresh)
        } else withImdb
      } else withImdb
      val withRt = if (needsRtRetry(withMc.rottenTomatoesUrl)) {
        val fresh = Try(rt.urlFor(linkTitle)).toOption.flatten
        if (fresh != withMc.rottenTomatoesUrl) {
          fresh match {
            case Some(url) =>
              urlsFilled += 1
              logger.debug(s"IMDb refresh: ${withMc.rottenTomatoesUrl.fold("backfilled")(_ => "upgraded")} RT URL for ${key.cleanTitle} → $url")
            case None =>
              logger.debug(s"IMDb refresh: cleared non-canonical RT URL for ${key.cleanTitle} (was ${withMc.rottenTomatoesUrl.getOrElse("—")})")
          }
          withMc.copy(rottenTomatoesUrl = fresh)
        } else withMc
      } else withMc

      // Backfill originalTitle from TMDB (production-language) for rows that
      // were enriched before we started preferring TMDB over OMDb. Cheap: one
      // HTTP per row per tick, guarded on tmdbId so legacy rows without it
      // are skipped silently. Once the row matches TMDB we stop changing it.
      val withOrig = withRt.tmdbId.flatMap(id => Try(tmdb.originalTitle(id)).toOption.flatten) match {
        case Some(t) if !withRt.originalTitle.contains(t) =>
          logger.debug(s"IMDb refresh: refreshed originalTitle for ${key.cleanTitle} → $t (was ${withRt.originalTitle.getOrElse("—")})")
          withRt.copy(originalTitle = Some(t))
        case _ => withRt
      }

      if (withOrig != enrichment) {
        cache.put(key, withOrig)
        changed += 1
      }
    }
    val took = System.currentTimeMillis() - startedAt
    val urlNote = if (urlsFilled > 0) s" ($urlsFilled URL(s) backfilled)" else ""
    logger.info(s"IMDb refresh: tick done in ${took}ms — $changed changed, $failed failed, ${snapshot.size - changed - failed} unchanged$urlNote.")
  }

  // TMDB hit + IMDb id, with a manual-override path first. The override fetches
  // the canonical TMDB record via /find when the Polish search index has no
  // mapping from the cinema's exhibitor title to the real film (e.g.
  // "Wspinaczka" → tt36437006 "Girl Climber" — TMDB has no Polish translation).
  private def resolveTmdb(title: String, year: Option[Int]): Option[(TmdbClient.SearchResult, String)] =
    TitleOverrides.lookup(title, year)
      .flatMap(id => tmdb.findByImdbId(id).map(_ -> id))
      .orElse(tmdb.search(title, year).flatMap(hit => tmdb.imdbId(hit.id).map(hit -> _)))

  private def fetchEnrichment(title: String, year: Option[Int]): Try[Option[Enrichment]] = Try {
    resolveTmdb(title, year).map { case (hit, imdbId) =>
      // Filmweb + IMDb + canonical-URL validation are all best-effort —
      // transient failures shouldn't drop the rest of the record. None of the
      // other fields are required for the enrichment to be usable; the worst
      // case is a row with just an IMDb id (and link).
      val fw          = Try(filmweb.lookup(title, year)).toOption.flatten
      val imdbRating  = Try(imdb.lookup(imdbId)).toOption.flatten
      val originalTit = hit.originalTitle
      val linkTitle   = originalTit.getOrElse(title)
      // Only persist canonical /movie/ and /m/ URLs. If the upstream falls back
      // to an on-site search URL we leave the field None — the view layer's
      // `metacriticHref` / `rottenTomatoesHref` synthesises a search link on
      // demand from the original title, so we don't lose anything user-facing.
      val mcFallback  = if (linkTitle != title) Some(title) else None
      val metacrUrl   = Try(metacritic.urlFor(linkTitle, mcFallback)).toOption.flatten
      val rtomatoeUrl = Try(rt.urlFor(linkTitle)).toOption.flatten
      Enrichment(
        imdbId            = imdbId,
        imdbRating        = imdbRating,
        metascore         = None,
        originalTitle     = originalTit,
        filmwebUrl        = fw.map(_.url),
        filmwebRating     = fw.flatMap(_.rating),
        rottenTomatoes    = None,
        tmdbId            = Some(hit.id),
        metacriticUrl     = metacrUrl,
        rottenTomatoesUrl = rtomatoeUrl
      )
    }
  }
}

object EnrichmentService {
  // Lowercase + accent strip + whitespace collapse. NFD strips most diacritics
  // (ą → a, ę → e, …) but Polish `ł` is a base character, not a composed one,
  // so we hand-map it.
  def normalize(title: String): String = {
    val stripped = Normalizer.normalize(title, Normalizer.Form.NFD).replaceAll("\\p{M}", "")
    stripped.toLowerCase
      .replace('ł', 'l').replace('Ł', 'l')
      .replaceAll("\\s+", " ").trim
  }

  // Decoration-stripping lives in `controllers.TitleNormalizer` so the same
  // patterns are used for merging (so "Top Gun 40th Anniversary" and "Top Gun"
  // collapse into one card) AND for TMDB/Filmweb lookups (so the search hits
  // the base film). See `TitleNormalizer.searchTitle` for the full list and
  // why each anchor is shaped the way it is.
  def searchTitle(display: String): String = controllers.TitleNormalizer.searchTitle(display)
}
