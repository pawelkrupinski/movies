package services.enrichment

import clients.TmdbClient
import play.api.Logging
import services.TitleOverrides
import services.events.{DomainEvent, EventBus, MovieAdded, TmdbResolved}

import java.text.Normalizer
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeUnit}
import models.Enrichment
import scala.util.{Failure, Success, Try}

/**
 * Two-stage enrichment pipeline, event-driven.
 *
 *   - **TMDB stage** resolves `(title, year)` → tmdbId + imdbId + originalTitle,
 *     plus Filmweb + Metacritic + Rotten Tomatoes URLs (all of which key off
 *     TMDB's `originalTitle`). Triggered by `MovieAdded`, and re-run once a day
 *     for cached rows whose `tmdbId` is still empty. Publishes `TmdbResolved`
 *     on success so the IMDb stage can fetch the rating asynchronously without
 *     blocking the TMDB lookup.
 *   - **IMDb stage** fetches one row's IMDb rating. Triggered by
 *     `TmdbResolved` for first-time enrichment, and re-run hourly to keep
 *     ratings current.
 *
 * Listeners are exposed as `PartialFunction` so `EventBus.applyOrElse` filters
 * for us — handlers only see events they pattern-match. Wiring lives in
 * `AppLoader`; this class never self-subscribes (see CLAUDE.md).
 *
 * Single bounded worker pool drains both stages so callers and event
 * publishers are never blocked on network round-trips.
 */
class EnrichmentService(
  cache:       EnrichmentCache,
  bus:         EventBus,
  imdbRatings: ImdbRatings,
  tmdb:        TmdbClient,
  filmweb:     FilmwebClient,
  metacritic:  MetacriticClient,
  rt:          RottenTomatoesClient
) extends Logging {

  // Active or queued TMDB-stage lookups, so we don't dispatch the same key
  // twice. (The IMDb stage doesn't dedup — it's idempotent and cheap.)
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()

  // Bounded pool drains both stages — TMDB-search/external_ids/Filmweb/MC/RT
  // for the TMDB stage, plus IMDb GraphQL for the IMDb stage. Each lookup is
  // mostly network wait, so 10 in-flight is comfortable under TMDB's published
  // rate limit (~50 req/s).
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

  // Daily-tick scheduler for `retryUnresolvedTmdb`. The hourly IMDb refresh
  // lives in `ImdbRatings`.
  private val tmdbRetryScheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "tmdb-retry"); t.setDaemon(true); t
  }

  // First run fires shortly after startup so Mongo hydration has time to
  // populate the cache and we don't race app boot.
  private val StartupDelaySeconds = 10L
  private val TmdbRetryHours      = 24L

  // How many times we've retried each key after a transient failure. Cleared
  // on success / non-transient miss. Caps a runaway loop for a key that
  // perpetually fails some non-cacheable way.
  private val retryAttempts = new ConcurrentHashMap[CacheKey, Integer]()
  private val MaxRetries    = 6

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the daily TMDB-retry tick. The hourly IMDb refresh is scheduled
   *  by `ImdbRatings.start()`. Called from `AppLoader` — see CLAUDE.md. */
  def start(): Unit = {
    val tmdbInterval = TmdbRetryHours * 3600
    logger.info(s"TMDB retry scheduled every ${TmdbRetryHours}h (first run in ${StartupDelaySeconds}s).")
    tmdbRetryScheduler.scheduleAtFixedRate(
      () => Try(retryUnresolvedTmdb()).recover {
        case ex => logger.warn(s"TMDB retry tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, tmdbInterval, TimeUnit.SECONDS
    )
  }

  /** Drain the queue so in-flight upserts hit Mongo before `EnrichmentRepo`
   *  closes its client. The caller (`AppLoader`) must register this so that
   *  Play runs the repo's close hook strictly *after* this returns. */
  def stop(): Unit = {
    worker.shutdown()
    retryScheduler.shutdown()
    tmdbRetryScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }

  // ── Event listeners ───────────────────────────────────────────────────────

  /** Subscribe on the `EventBus` to schedule the TMDB stage when a new title
   *  shows up in the cinema schedule. No-op for rows already resolved or
   *  negative-cached. */
  val onMovieAdded: PartialFunction[DomainEvent, Unit] = {
    case MovieAdded(title, year) => scheduleTmdbStage(cache.keyOf(title, year))
  }

  // ── Public read + manual re-enrich ────────────────────────────────────────

  /** Pure cache lookup — never blocks, never schedules. Misses return None;
   *  the next `MovieAdded` event re-triggers a background fetch. */
  def get(title: String, year: Option[Int]): Option[Enrichment] =
    cache.get(cache.keyOf(title, year))

  /** Snapshot of every cached enrichment — for debug tooling. */
  def snapshot(): Seq[(String, Option[Int], Enrichment)] = cache.snapshot()

  /** Manual re-enrich (debug page). Runs the TMDB stage on the worker pool;
   *  the bus listener picks up `TmdbResolved` and chains the IMDb stage.
   *
   *  Returns immediately. Tests that want synchronous results call
   *  `reEnrichSync` instead.
   */
  def reEnrich(title: String, year: Option[Int]): Unit = {
    val key = cache.keyOf(title, year)
    worker.execute(() => runTmdbStage(key))
  }

  /** Same as `reEnrich` but blocks the calling thread and returns the final
   *  enrichment (or None if TMDB has no hit). Runs both stages synchronously
   *  — useful for scripts that need a single-shot answer. */
  def reEnrichSync(title: String, year: Option[Int]): Option[Enrichment] = {
    val key = cache.keyOf(title, year)
    runTmdbStageSync(key).map { _ =>
      imdbRatings.refreshOneSync(key)
      cache.get(key).getOrElse(throw new IllegalStateException(
        s"TMDB stage stored a row for ${key.cleanTitle} but it's gone — concurrent invalidate?"
      ))
    }
  }

  // ── TMDB stage ─────────────────────────────────────────────────────────────

  private def scheduleTmdbStage(key: CacheKey): Unit = {
    if (cache.get(key).exists(_.tmdbId.isDefined)) return  // already resolved
    if (cache.isNegative(key)) return                       // known miss, retry after TTL
    if (pending.add(key))
      worker.execute(() => try runTmdbStage(key) finally pending.remove(key))
  }

  // Async wrapper around runTmdbStageSync: handles retry policy + event publish.
  // The event is only published when an IMDb id is present — otherwise there's
  // nothing for the IMDb listener to look up. Rows resolved without an IMDb
  // id still get re-checked by the daily TMDB retry tick.
  private def runTmdbStage(key: CacheKey): Unit =
    Try(runTmdbStageSync(key)) match {
      case Success(Some(e)) =>
        retryAttempts.remove(key)
        e.imdbId match {
          case Some(id) =>
            bus.publish(TmdbResolved(key.cleanTitle, key.year, id))
            logger.debug(s"TMDB stage: ${key.cleanTitle} (${key.year.getOrElse("?")}) → $id")
          case None =>
            logger.debug(s"TMDB stage: ${key.cleanTitle} (${key.year.getOrElse("?")}) → tmdbId=${e.tmdbId.getOrElse("—")} (no IMDb cross-reference yet)")
        }
      case Success(None)    =>
        cache.markMissing(key)
        retryAttempts.remove(key)
      case Failure(ex)      =>
        // Transient failure (rate limit / network blip). Don't poison the
        // negative cache — schedule a delayed retry so the title gets enriched
        // automatically once the upstream recovers.
        scheduleTmdbRetry(key, ex)
    }

  // Synchronous core. Resolves TMDB; on a hit, also fetches Filmweb / MC URL /
  // RT URL (all derived from TMDB's `originalTitle`) and writes the new row
  // through to the cache. Returns the new Enrichment, or None when TMDB has
  // no match. Does NOT publish events — callers decide.
  private def runTmdbStageSync(key: CacheKey): Option[Enrichment] =
    resolveTmdb(key.cleanTitle, key.year).map { case (hit, imdbId) =>
      val linkTitle  = hit.originalTitle.getOrElse(key.cleanTitle)
      val mcFallback = if (linkTitle != key.cleanTitle) Some(key.cleanTitle) else None

      val fw    = Try(filmweb.lookup(key.cleanTitle, key.year)).toOption.flatten
      // Pass TMDB's release year (not the cinema's year, which is unreliable —
      // anniversary screenings report the *screening* year, not the film's).
      val mc    = Try(metacritic.urlFor(linkTitle, mcFallback, hit.releaseYear)).toOption.flatten
      val rtUrl = Try(rt.urlFor(linkTitle, mcFallback, hit.releaseYear)).toOption.flatten

      // Preserve any existing ratings — `ImdbRatings` and `RottenTomatoesRatings`
      // refresh them on `TmdbResolved` and on their own hourly cycles. The
      // TMDB stage owns URLs only.
      val existing = cache.get(key)
      val enr = Enrichment(
        imdbId            = imdbId,
        imdbRating        = existing.flatMap(_.imdbRating),
        metascore         = None,
        originalTitle     = hit.originalTitle,
        filmwebUrl        = fw.map(_.url),
        filmwebRating     = fw.flatMap(_.rating),
        rottenTomatoes    = existing.flatMap(_.rottenTomatoes),
        tmdbId            = Some(hit.id),
        metacriticUrl     = mc,
        rottenTomatoesUrl = rtUrl
      )
      cache.put(key, enr)
      enr
    }

  // The IMDb-side stage logic (schedule, sync refresh, periodic walk) lives in
  // `ImdbRatings`. `AppLoader` subscribes `imdbRatings.onTmdbResolved` on the
  // same bus that publishes `TmdbResolved` above; `reEnrichSync` calls
  // `imdbRatings.refreshOneSync` directly to avoid the worker pool.

  // ── Retry policy for TMDB transient failures ──────────────────────────────

  private def scheduleTmdbRetry(key: CacheKey, cause: Throwable): Unit = {
    val attempt = retryAttempts.merge(key, 1: Integer, (a: Integer, b: Integer) => a + b)
    if (attempt > MaxRetries) {
      logger.warn(s"Giving up on TMDB ${key.cleanTitle} (${key.year.getOrElse("?")}) after $attempt attempts: ${cause.getMessage}")
      retryAttempts.remove(key)
      cache.markMissing(key)  // Stop hammering it for an hour.
      return
    }
    val delaySeconds = math.min(30L * 60, 30L * (1L << (attempt - 1)))
    logger.warn(s"TMDB stage failed for ${key.cleanTitle} (${key.year.getOrElse("?")}) on attempt $attempt: ${cause.getMessage}; retrying in ${delaySeconds}s")
    retryScheduler.schedule(new Runnable {
      def run(): Unit = scheduleTmdbStage(key)
    }, delaySeconds, TimeUnit.SECONDS)
  }

  // ── Scheduled loop ────────────────────────────────────────────────────────
  // The hourly IMDb refresh lives in `ImdbRatings.refreshAll`.

  /** Walk every *incomplete* cached row — `tmdbId` missing, MC URL missing,
   *  or RT URL missing — and re-run the TMDB stage. Bypasses
   *  `scheduleTmdbStage`'s "skip when tmdbId.isDefined" short-circuit so rows
   *  that have TMDB resolved but didn't get MC/RT URLs at first lookup get
   *  another shot (the upstream slug strategy keeps improving — new search-
   *  scrape fallback in `MetacriticClient`, future RT improvements, etc.).
   *  Also clears the entire negative cache so previously-failed `(title,
   *  year)` lookups get one fresh shot via the next ShowtimeCache refresh.
   *  Fires once every `TmdbRetryHours`. */
  private[enrichment] def retryUnresolvedTmdb(): Unit = {
    cache.clearNegatives()
    val targets = cache.entries.collect {
      case (k, e) if e.tmdbId.isEmpty
                  || e.imdbId.isEmpty
                  || e.metacriticUrl.isEmpty
                  || e.rottenTomatoesUrl.isEmpty => k
    }
    val missingTmdb = targets.count(k => cache.get(k).exists(_.tmdbId.isEmpty))
    val missingImdb = targets.count(k => cache.get(k).exists(e => e.tmdbId.isDefined && e.imdbId.isEmpty))
    val missingUrls = targets.size - missingTmdb - missingImdb
    logger.info(
      s"TMDB retry: cleared negatives + re-scheduling ${targets.size} incomplete row(s) " +
      s"($missingTmdb missing tmdbId, $missingImdb missing imdbId, $missingUrls missing MC/RT URL)."
    )
    // Bypass scheduleTmdbStage — it would skip rows where tmdbId is already
    // set. We deliberately want to re-run the TMDB stage on those rows so the
    // MC/RT URL probes get another shot AND TMDB's external_ids may have
    // gained an imdbId since the last attempt.
    targets.foreach { k =>
      if (pending.add(k))
        worker.execute(() => try runTmdbStage(k) finally pending.remove(k))
    }
  }

  // ── TMDB resolution ────────────────────────────────────────────────────────

  // Manual-override path first — `TitleOverrides` pins (title, year) → imdbId
  // for films TMDB's Polish search can't find (e.g. "Wspinaczka" → tt36437006
  // "Girl Climber" — TMDB has no Polish translation). Falls back to the normal
  // title-search resolution.
  //
  // The IMDb id is OPTIONAL: TMDB doesn't always have a cross-reference yet
  // (very recent releases — e.g. "Za duży na bajki 3" tmdbid 1484486 has no
  // imdb_id at TMDB at the time of writing). When we have only a TMDB hit, we
  // still store the row (Filmweb / MC / RT all key off the title, not the
  // IMDb id) and the daily retry tick re-checks for the IMDb id later.
  private def resolveTmdb(title: String, year: Option[Int]): Option[(TmdbClient.SearchResult, Option[String])] =
    TitleOverrides.lookup(title, year)
      .flatMap(id => tmdb.findByImdbId(id).map(_ -> Some(id)))
      .orElse(tmdb.search(title, year).map(hit => hit -> tmdb.imdbId(hit.id)))
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
