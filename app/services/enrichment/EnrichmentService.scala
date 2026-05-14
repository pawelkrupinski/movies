package services.enrichment

import clients.TmdbClient
import play.api.Logging
import services.events.{DomainEvent, EventBus, ImdbIdMissing, MovieAdded, TmdbResolved}

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
  cache: EnrichmentCache,
  bus:   EventBus,
  tmdb:  TmdbClient
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
   *  negative-cached.
   *
   *  Captures the cinema-provided `originalTitle` (when present) as a hint
   *  the TMDB stage can use as a secondary search title — see `resolveTmdb`. */
  val onMovieAdded: PartialFunction[DomainEvent, Unit] = {
    case MovieAdded(title, year, originalTitle) =>
      scheduleTmdbStage(cache.keyOf(title, year), originalTitle)
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

  /** Same as `reEnrich` but blocks the calling thread and returns the row TMDB
   *  resolved (or None if TMDB has no hit). Runs the TMDB stage only — callers
   *  that also want fresh IMDb / Filmweb / Metacritic / Rotten Tomatoes data
   *  should chain the corresponding `*Ratings.refreshOneSync(title, year)`
   *  call themselves (see `scripts/EnrichmentBackfill` for the pattern). Does
   *  NOT publish bus events, so concurrent listeners don't double-fetch. */
  def reEnrichSync(title: String, year: Option[Int]): Option[Enrichment] =
    runTmdbStageSync(cache.keyOf(title, year))

  // ── TMDB stage ─────────────────────────────────────────────────────────────

  private def scheduleTmdbStage(key: CacheKey, originalTitle: Option[String] = None): Unit = {
    if (cache.get(key).exists(_.tmdbId.isDefined)) return  // already resolved
    if (cache.isNegative(key)) return                       // known miss, retry after TTL
    if (pending.add(key))
      worker.execute(() => try runTmdbStage(key, originalTitle) finally pending.remove(key))
  }

  // Async wrapper around runTmdbStageSync: handles retry policy + event publish.
  // Two events:
  //   - `TmdbResolved`    when TMDB had an IMDb id — drives the IMDb rating
  //                       and RT score listeners.
  //   - `ImdbIdMissing`   when TMDB resolved the film but had no IMDb cross-
  //                       reference — `ImdbRatings` falls back to IMDb's
  //                       suggestion endpoint to find the id (e.g. very recent
  //                       theatrical releases TMDB hasn't been told about yet).
  private def runTmdbStage(key: CacheKey, originalTitleHint: Option[String] = None): Unit =
    Try(runTmdbStageSync(key, originalTitleHint)) match {
      case Success(Some(e)) =>
        retryAttempts.remove(key)
        e.imdbId match {
          case Some(id) =>
            bus.publish(TmdbResolved(key.cleanTitle, key.year, id))
            logger.debug(s"TMDB stage: ${key.cleanTitle} (${key.year.getOrElse("?")}) → $id")
          case None =>
            val searchTitle = e.originalTitle.getOrElse(key.cleanTitle)
            logger.debug(s"TMDB stage: ${key.cleanTitle} (${key.year.getOrElse("?")}) → tmdbId=${e.tmdbId.getOrElse("—")} (no IMDb cross-reference yet); publishing ImdbIdMissing(search='$searchTitle')")
            bus.publish(ImdbIdMissing(key.cleanTitle, key.year, searchTitle))
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

  // Synchronous core. Resolves TMDB; on a hit, writes a row carrying ONLY the
  // TMDB-side fields (tmdbId, imdbId, originalTitle). All score/URL fields
  // (IMDb rating, Metacritic URL+score, RT URL+score, Filmweb URL+rating)
  // are owned by the dedicated *Ratings classes — each subscribes to
  // `TmdbResolved` on the bus and runs its own discovery/refresh. The TMDB
  // stage preserves any existing values for those fields so a re-resolve
  // doesn't blank them while the listeners catch up. Returns the new
  // Enrichment, or None when TMDB has no match. Does NOT publish events —
  // callers decide.
  private def runTmdbStageSync(key: CacheKey, originalTitleHint: Option[String] = None): Option[Enrichment] =
    resolveTmdb(key.cleanTitle, key.year, originalTitleHint).map { case (hit, imdbId) =>
      val existing = cache.get(key)
      // Preserve the previously-known `imdbId` when TMDB resolved the same
      // film (same `tmdbId`) but momentarily dropped the cross-reference —
      // happens for very recent releases and occasional TMDB data hiccups.
      // When TMDB resolves to a DIFFERENT `tmdbId` we accept the new film's
      // imdbId (even if None) so a stale id can't leak across a correction.
      val preserveImdbId = existing.exists(_.tmdbId.contains(hit.id))
      val resolvedImdbId = imdbId.orElse(if (preserveImdbId) existing.flatMap(_.imdbId) else None)
      val enr = Enrichment(
        imdbId            = resolvedImdbId,
        imdbRating        = existing.flatMap(_.imdbRating),
        metascore         = existing.flatMap(_.metascore),
        originalTitle     = hit.originalTitle,
        filmwebUrl        = existing.flatMap(_.filmwebUrl),
        filmwebRating     = existing.flatMap(_.filmwebRating),
        rottenTomatoes    = existing.flatMap(_.rottenTomatoes),
        tmdbId            = Some(hit.id),
        metacriticUrl     = existing.flatMap(_.metacriticUrl),
        rottenTomatoesUrl = existing.flatMap(_.rottenTomatoesUrl)
      )
      cache.put(key, enr)
      enr
    }

  // IMDb / Filmweb / Metacritic / Rotten Tomatoes refresh logic lives in the
  // dedicated *Ratings classes. `AppLoader` subscribes each one's
  // `onTmdbResolved` on the same bus that publishes `TmdbResolved` above, so
  // they react automatically to async re-enrichment. The sync path
  // (`reEnrichSync`) is TMDB-only on purpose — callers that need the score
  // fields chain `*Ratings.refreshOneSync(title, year)` themselves so the
  // worker pool stays uninvolved (see `scripts/EnrichmentBackfill`).

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

  /** Walk every cached row with no `tmdbId` yet and re-run the TMDB stage on
   *  it. Rows that DO have a `tmdbId` are intentionally left alone — once a
   *  row is TMDB-resolved (whether via title search, sister-row inheritance,
   *  or a manual override), we trust that resolution. Re-resolving could
   *  flip the row to a different film when TMDB's title search lands on a
   *  more popular same-title hit, undoing earlier corrections (override or
   *  sister-row donation). Missing MC / RT / Filmweb URLs are recovered by
   *  the respective `*Ratings.refreshAll` hourly walks, which do their own
   *  URL discovery; missing IMDb ids are recovered by the `ImdbIdMissing`
   *  event fired from the TMDB stage at first resolution. Fires once every
   *  `TmdbRetryHours`; also clears the negative cache so previously-failed
   *  `(title, year)` lookups get one fresh shot via the next ShowtimeCache
   *  refresh. */
  private[enrichment] def retryUnresolvedTmdb(): Unit = {
    cache.clearNegatives()
    val targets = cache.entries.collect { case (k, e) if e.tmdbId.isEmpty => k }
    logger.info(s"TMDB retry: cleared negatives + re-scheduling ${targets.size} row(s) with missing tmdbId.")
    targets.foreach { k =>
      if (pending.add(k))
        worker.execute(() => try runTmdbStage(k) finally pending.remove(k))
    }
  }

  // ── TMDB resolution ────────────────────────────────────────────────────────

  // Resolution order:
  //   1. Sister-row alias match — when another cache row sharing any title
  //      alias has already been TMDB-resolved (typically because it reported
  //      a year and we didn't here, or its TMDB-resolved originalTitle is the
  //      English name we're looking up), inherit that resolution. Avoids
  //      year-less search collisions (Bez końca 1985 vs 2026, Belle 2013 vs
  //      Hosoda anime, etc.).
  //   2. Polish-localised TMDB title search.
  //   3. Same search by the cinema's `originalTitle` when present and
  //      different from the primary title (Multikino exposes one for
  //      Cirque du Soleil / opera / English-language imports).
  //
  // The IMDb id is OPTIONAL: TMDB doesn't always have a cross-reference yet
  // (very recent releases — e.g. "Za duży na bajki 3" tmdbid 1484486 has no
  // imdb_id at TMDB at the time of writing). When we have only a TMDB hit, we
  // still store the row (Filmweb / MC / RT all key off the title, not the
  // IMDb id); `ImdbIdMissing` fires from the async TMDB stage so
  // `ImdbRatings` can recover the id via IMDb's suggestion endpoint.
  private def resolveTmdb(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String] = None
  ): Option[(TmdbClient.SearchResult, Option[String])] =
    sisterRowMatch(title, year, originalTitle)
      .orElse(tmdb.search(title, year).map(hit => hit -> tmdb.imdbId(hit.id)))
      .orElse {
        originalTitle
          .filterNot(_.equalsIgnoreCase(title))
          .flatMap(t => tmdb.search(t, year))
          .map(hit => hit -> tmdb.imdbId(hit.id))
      }

  /** Walk the cache for already-resolved rows sharing any title alias with
   *  this one (cleanTitle + cinema-provided originalTitle on the self side;
   *  cleanTitle + TMDB-resolved originalTitle on the donor side). Excludes
   *  the row being resolved (a stale self must not reinforce its own wrong
   *  answer). Requires a unanimous `tmdbId` across all matching sister rows
   *  — when two cached sisters disagree (e.g. cinema A is screening the
   *  1985 cut and cinema B the 2026 remake of the same title), the ambiguity
   *  defers to TMDB rather than picking blindly.
   *
   *  Matching aliases — not just `cleanTitle == cleanTitle` — picks up cases
   *  where a long Polish-localised title carries the English original as its
   *  `originalTitle` field. Example: cache row
   *  `("Belle: smok i piegowata księżniczka", 2021, originalTitle="Belle")`
   *  donates to a plain-`"Belle"` lookup. */
  private def sisterRowMatch(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String]
  ): Option[(TmdbClient.SearchResult, Option[String])] = {
    val selfKey     = cache.keyOf(title, year)
    val selfAliases = titleAliases(selfKey.cleanTitle, originalTitle)
    if (selfAliases.isEmpty) return None

    val resolved = cache.entries.flatMap { case (k, e) =>
      if (k == selfKey || e.tmdbId.isEmpty) None
      else {
        val donorAliases = titleAliases(k.cleanTitle, e.originalTitle)
        if (selfAliases.exists(donorAliases.contains))
          Some((e.tmdbId.get, e.imdbId, e.originalTitle))
        else None
      }
    }
    val tmdbIds = resolved.map(_._1).distinct
    if (tmdbIds.size != 1) None
    else {
      val (tmdbId, imdbId, origTitle) = resolved.head
      val hit = TmdbClient.SearchResult(
        id            = tmdbId,
        title         = origTitle.getOrElse(title),
        originalTitle = origTitle,
        releaseYear   = None,   // unused by runTmdbStageSync
        popularity    = 0.0
      )
      logger.debug(s"Sister-row match: ${selfKey.cleanTitle} (${year.getOrElse("?")}) → tmdbId=$tmdbId imdbId=${imdbId.getOrElse("—")} via aliases=${selfAliases.mkString(",")}")
      Some(hit -> imdbId)
    }
  }

  /** Normalized alias set for a (cleanTitle, originalTitle) pair. Empty
   *  components and duplicates collapse; both sides go through
   *  `EnrichmentService.normalize` so accent/case differences fold to the
   *  same key. */
  private def titleAliases(cleanTitle: String, originalTitle: Option[String]): Set[String] = {
    val parts = Iterator(Option(cleanTitle), originalTitle)
      .flatten
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(EnrichmentService.normalize)
      .filter(_.nonEmpty)
    parts.toSet
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
