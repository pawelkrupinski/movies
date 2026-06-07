package services.movies

import clients.TmdbClient
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{DomainEvent, EventBus, ImdbIdMissing, MovieRecordCreated, TmdbResolved}
import tools.DaemonExecutors

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import models.{MovieRecord, Source, SourceData, Tmdb}
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

/**
 * Two-stage enrichment pipeline, event-driven.
 *
 *   - **TMDB stage** resolves `(title, year)` → tmdbId + imdbId + originalTitle,
 *     plus Filmweb + Metacritic + Rotten Tomatoes URLs (all of which key off
 *     TMDB's `originalTitle`). Triggered by `MovieRecordCreated`, and re-run once a day
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
class MovieService(
  cache: MovieCache,
  bus:   EventBus,
  tmdb:  TmdbClient,
  // Drains both enrichment stages. Defaults to a dedicated unbounded pool so
  // tests/scripts construct it as before; `Wiring` injects a shared-budget EC
  // so enrichment shares one concurrency cap with the scrape + rating
  // refreshers and can't peg the box on the hourly walk. See
  // `SharedExecutionBudget`.
  ec:    ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC("enrichment-worker"),
  // How many times a TMDB stage is retried (with backoff) after a transient
  // failure. Production keeps the default; fixture-replay tests set 0 so a
  // permanently-missing fixture isn't retried 6× — that retry churn both slows
  // the suite (backoff waits) and widens the window in which the cascade drain
  // drops in-flight enrichment, making whole-corpus renders nondeterministic.
  maxRetries: Int = 6
) extends Stoppable with Logging {

  // Active or queued TMDB-stage lookups, so we don't dispatch the same key
  // twice. (The IMDb stage doesn't dedup — it's idempotent and cheap.)
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()

  // EC notes: each lookup is mostly network wait; virtual threads make per-task
  // concurrency free, and TMDB's published rate limit (~50 req/s) is enforced
  // at the HTTP layer (back off on 429/503) rather than at the thread count.

  // Separate scheduler for delayed retries — it just hands a Runnable back to
  // the worker pool when the timer fires, so we don't tie up a worker thread
  // sleeping. Daemon so it doesn't keep the JVM alive.
  private val retryScheduler = DaemonExecutors.scheduler("enrichment-retry-scheduler")

  // Daily-tick scheduler for `retryUnresolvedTmdb`. The hourly IMDb refresh
  // lives in `ImdbRatings`.
  private val tmdbRetryScheduler = DaemonExecutors.scheduler("tmdb-retry")

  // First run fires shortly after startup so Mongo hydration has time to
  // populate the cache and we don't race app boot.
  private val StartupDelaySeconds = 10L
  private val TmdbRetryHours      = 24L

  // How many times we've retried each key after a transient failure. Cleared
  // on success / non-transient miss. Caps a runaway loop for a key that
  // perpetually fails some non-cacheable way.
  private val retryAttempts = new ConcurrentHashMap[CacheKey, Integer]()
  private val MaxRetries    = maxRetries

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

  /** Drain the queue so in-flight upserts hit Mongo before `MovieRepo`
   *  closes its client AND every TmdbResolved / ImdbIdMissing event the
   *  in-flight tasks would publish has fired (downstream listeners
   *  dispatch synchronously on this worker's thread). The caller
   *  (`AppLoader`) registers this hook so Play runs the repo's close
   *  strictly *after* this returns.
   *
   *  Waits for the whole queue to drain, not a fixed 15-s window — a
   *  fixed cap was returning before TMDB lookups against real upstreams
   *  finished, so downstream `*Ratings` pools got drained while
   *  `runTmdbStage` was still queueing tasks against them. Play's
   *  lifecycle still has its own deadline, so production is unaffected
   *  in the happy path; the bound is "every task runs to completion or
   *  the JVM is force-killed by the lifecycle". */
  def stop(): Unit = {
    ec.shutdown()
    retryScheduler.shutdown()
    tmdbRetryScheduler.shutdown()
    while (!ec.isTerminated) ec.awaitTermination(1, TimeUnit.HOURS)
  }

  // ── Event listeners ───────────────────────────────────────────────────────

  /** Subscribe on the `EventBus` to schedule the TMDB stage when a new title
   *  shows up in the cinema schedule. No-op for rows already resolved or
   *  negative-cached.
   *
   *  Captures the cinema-provided `originalTitle` (when present) as a hint
   *  the TMDB stage can use as a secondary search title — see `resolveTmdb`. */
  val onMovieRecordCreated: PartialFunction[DomainEvent, Unit] = {
    case MovieRecordCreated(title, year, originalTitle, director) =>
      scheduleTmdbStage(cache.keyOf(title, year), originalTitle, director)
  }

  // ── Public read + manual re-enrich ────────────────────────────────────────

  /** Pure cache lookup — never blocks, never schedules. Misses return None;
   *  the next `MovieRecordCreated` event re-triggers a background fetch. */
  def get(title: String, year: Option[Int]): Option[MovieRecord] =
    cache.get(cache.keyOf(title, year))

  /** Snapshot of every cached enrichment — for debug tooling. */
  def snapshot(): Seq[StoredMovieRecord] = cache.snapshot()

  /** Reload the positive cache from Mongo. Returns the number of rows loaded.
   *  Wired to the `/debug/rehydrate` admin endpoint; useful when Mongo has
   *  been edited out-of-band and the in-memory cache needs to catch up. */
  def rehydrate(): Int = cache.rehydrate()

  /** Manual re-enrich (debug page). Wipes the cached row, then runs the TMDB
   *  stage on the worker pool — the bus listener picks up `TmdbResolved` and
   *  chains the IMDb stage; the *Ratings refreshes will re-derive MC / RT /
   *  Filmweb URLs from the new TMDB id on their next tick.
   *
   *  `originalTitle` and `director` are cinema-side hints (sourced from the
   *  current `ShowtimeCache` by the controller) that feed the same
   *  director-verification + director-walk path `MovieRecordCreated` uses. Without
   *  them, the TMDB title search alone can lock in a same-title-different-
   *  film result, undoing whatever the bus-driven path already corrected.
   *
   *  Returns immediately. Tests that want synchronous results call
   *  `reEnrichSync` instead. */
  def reEnrich(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String] = None,
    director:      Option[String] = None
  ): Unit = {
    val key = cache.keyOf(title, year)
    // Don't delete the row outright — the previous implementation called
    // `cache.invalidate(key)` here, which dropped the row from positive
    // cache AND Mongo immediately, leaving a window where /debug,
    // toSchedules, and the rest of the app saw the film as "gone" until
    // the async TMDB stage put it back. Worse, if TMDB returned no match
    // (events, special screenings) or simply took longer than the JS
    // reload timer to respond, the row stayed gone permanently.
    //
    // Instead, atomically strip just the TMDB-derived fields — the row
    // stays visible while reEnrich is in flight, scrape data + cinema
    // slots are preserved, and on a TMDB miss the row remains with
    // cleared TMDB / ratings / URLs (no stale URLs survive from the
    // previous wrong resolution). The dedicated `*Ratings` classes
    // re-discover MC / RT / Filmweb URLs from the new tmdbId via the
    // `TmdbResolved` bus event.
    cache.putIfPresent(key, existing => existing.copy(
      tmdbId            = None,
      imdbId            = None,
      imdbRating        = None,
      metascore         = None,
      metacriticUrl     = None,
      rottenTomatoes    = None,
      rottenTomatoesUrl = None,
      filmwebUrl        = None,
      filmwebRating     = None,
      // Drop the TMDB SourceData slot too — synopsis / cast / director
      // / runtime / countries / poster all came from the wrong film
      // and shouldn't bleed forward. Cinema slots (per-`Cinema`
      // entries in `data`) stay intact so showtimes survive the
      // re-resolve.
      data              = existing.data - Tmdb
    ))
    Future(runTmdbStage(key, originalTitle, director))(using ec)
    ()
  }

  /** Same as `reEnrich` but blocks the calling thread and returns the row TMDB
   *  resolved (or None if TMDB has no hit). Runs the TMDB stage only — callers
   *  that also want fresh IMDb / Filmweb / Metacritic / Rotten Tomatoes data
   *  should chain the corresponding `*Ratings.refreshOneSync(title, year)`
   *  call themselves (see `scripts/EnrichmentBackfill` for the pattern). Does
   *  NOT publish bus events, so concurrent listeners don't double-fetch. */
  def reEnrichSync(title: String, year: Option[Int]): Option[MovieRecord] =
    runTmdbStageSync(cache.keyOf(title, year)).map(_._2)

  // ── TMDB stage ─────────────────────────────────────────────────────────────

  private def scheduleTmdbStage(
    key:           CacheKey,
    originalTitle: Option[String] = None,
    director:      Option[String] = None
  ): Unit = {
    if (cache.get(key).exists(_.tmdbId.isDefined)) return  // already resolved
    // Negative cache: short-circuit known misses — but ONLY when this event
    // brings no new resolution signal. A later scrape that carries a director
    // (Helios/Multikino) or originalTitle (the cinema's English title field)
    // the prior attempt didn't have is worth retrying: `directorWalk` runs
    // off the director hint, and a TMDB title search by originalTitle can
    // hit where the Polish title missed. Without this carve-out, a row
    // where the first-scraping cinema reports no director (CinemaCity,
    // Charlie Monroe) stays trapped at tmdbId=None for the full 24h
    // negative TTL — the Kurozając class of regression.
    if (cache.isNegative(key) && originalTitle.isEmpty && director.isEmpty) return
    // A sibling row already knows this raw cinema title (via cinemaTitles)
    // AND has a tmdbId. `recordCinemaScrape`'s redirect has already
    // attached this cinema's slot to that sibling, so running TMDB again
    // would just create a phantom row at the `(title, year)` key that
    // nothing would clean up — wasted TMDB call plus a stale year-
    // divergent row sitting in Mongo forever.
    if (cache.hasResolvedSiblingByTitle(key.cleanTitle)) return
    if (pending.add(key)) {
      Future(try runTmdbStage(key, originalTitle, director) finally pending.remove(key))(using ec)
      ()
    }
  }

  // Async wrapper around runTmdbStageSync: handles retry policy + event publish.
  // Two events:
  //   - `TmdbResolved`    when TMDB had an IMDb id — drives the IMDb rating
  //                       and RT score listeners.
  //   - `ImdbIdMissing`   when TMDB resolved the film but had no IMDb cross-
  //                       reference — `ImdbRatings` falls back to IMDb's
  //                       suggestion endpoint to find the id (e.g. very recent
  //                       theatrical releases TMDB hasn't been told about yet).
  private def runTmdbStage(
    key:               CacheKey,
    originalTitleHint: Option[String] = None,
    directorHint:      Option[String] = None
  ): Unit =
    Try(runTmdbStageSync(key, originalTitleHint, directorHint)) match {
      case Success(Some((finalKey, movieRecord))) =>
        retryAttempts.remove(key)
        movieRecord.imdbId match {
          case Some(id) =>
            bus.publish(TmdbResolved(finalKey.cleanTitle, finalKey.year, id))
            logger.debug(s"TMDB stage: ${finalKey.cleanTitle} (${finalKey.year.getOrElse("?")}) → $id")
          case None =>
            // IMDb's suggestion endpoint sees the cleaned-up form when TMDB
            // didn't ship an originalTitle, so accessibility-decorated rows
            // ("Kino bez barier: Freak Show (AD)") query IMDb as just
            // "Freak Show". TMDB's originalTitle, when present, is already
            // canonical and doesn't need stripping.
            val searchTitle = movieRecord.originalTitle.getOrElse(MovieService.apiQuery(finalKey.cleanTitle))
            logger.debug(s"TMDB stage: ${finalKey.cleanTitle} (${finalKey.year.getOrElse("?")}) → tmdbId=${movieRecord.tmdbId.getOrElse("—")} (no IMDb cross-reference yet); publishing ImdbIdMissing(search='$searchTitle')")
            bus.publish(ImdbIdMissing(finalKey.cleanTitle, finalKey.year, searchTitle))
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
  // MovieRecord, or None when TMDB has no match. Does NOT publish events —
  // callers decide.
  private def runTmdbStageSync(
    key:               CacheKey,
    originalTitleHint: Option[String] = None,
    directorHint:      Option[String] = None
  ): Option[(CacheKey, MovieRecord)] =
    resolveTmdb(key.cleanTitle, key.year, originalTitleHint, directorHint).map { case (hit, imdbId) =>
      // The slow HTTP fetch happens OUTSIDE the title lock so concurrent
      // cinema scrapes for the same title aren't blocked for its duration.
      val detailsOpt = tmdb.fullDetails(hit.id)
      // Read → modify → write under the per-title lock so a cinema scrape's
      // freshly-written slot, landing just before this thread enters the
      // critical section, is visible to the carry-forward below — and so
      // the rekey's invalidate→put sequence can't leave any window for a
      // concurrent scrape to see no sibling and spawn a phantom row (the
      // "Straszny film" twins regression).
      cache.withTitleLock(key.cleanTitle) {
        val existing = cache.get(key)
        // Preserve the previously-known `imdbId` when TMDB resolved the same
        // film (same `tmdbId`) but momentarily dropped the cross-reference —
        // happens for very recent releases and occasional TMDB data hiccups.
        // When TMDB resolves to a DIFFERENT `tmdbId` we accept the new film's
        // imdbId (even if None) so a stale id can't leak across a correction.
        val preserveImdbId = existing.exists(_.tmdbId.contains(hit.id))
        val resolvedImdbId = imdbId.orElse(if (preserveImdbId) existing.flatMap(_.imdbId) else None)
        // Carry the cinema-side fields forward — `recordCinemaScrape` may have
        // just landed a slot on this row, and the TMDB stage doesn't own
        // cinema data. Without this, a fresh resolve wipes every cinema's
        // slot and the row drops out of `toSchedules` until the next scrape
        // tick repopulates it.
        val carriedData = existing.map(_.data).getOrElse(Map.empty[Source, SourceData])
        // Fetch the full TMDB record in a single round-trip so the SourceData
        // (Tmdb) slot carries the Polish synopsis, director, cast, runtime,
        // year, countries and poster — not just the search-hit-shape fields.
        // On a fetch failure we fall back to the search-hit-shape so the row
        // at least keeps title + originalTitle + year going forward.
        val existingTmdbSlot = carriedData.getOrElse(Tmdb, SourceData())
        val tmdbSlot = detailsOpt match {
          case Some(d) => SourceData(
            title          = d.title.orElse(Some(hit.title).filter(_.nonEmpty)).orElse(existingTmdbSlot.title),
            originalTitle  = d.originalTitle.orElse(hit.originalTitle).orElse(existingTmdbSlot.originalTitle),
            synopsis       = d.synopsis.orElse(existingTmdbSlot.synopsis),
            cast           = if (d.cast.nonEmpty) d.cast else existingTmdbSlot.cast,
            director       = if (d.director.nonEmpty) d.director else existingTmdbSlot.director,
            runtimeMinutes = d.runtimeMinutes.orElse(existingTmdbSlot.runtimeMinutes),
            releaseYear    = d.releaseYear.orElse(hit.releaseYear).orElse(existingTmdbSlot.releaseYear),
            // Canonicalise TMDB's English country names ("United States of
            // America" → "USA") so the merged-record dedup operates on the
            // same strings cinemas already write.
            countries      = if (d.countries.nonEmpty) d.countries.map(CountryNames.canonical).distinct
                             else existingTmdbSlot.countries,
            genres         = if (d.genres.nonEmpty) d.genres else existingTmdbSlot.genres,
            posterUrl      = d.posterUrl.orElse(existingTmdbSlot.posterUrl)
          )
          case None => existingTmdbSlot.copy(
            title         = Some(hit.title).filter(_.nonEmpty).orElse(existingTmdbSlot.title),
            originalTitle = hit.originalTitle.orElse(existingTmdbSlot.originalTitle),
            releaseYear   = hit.releaseYear.orElse(existingTmdbSlot.releaseYear)
          )
        }
        val enr = MovieRecord(
          imdbId            = resolvedImdbId,
          imdbRating        = existing.flatMap(_.imdbRating),
          metascore         = existing.flatMap(_.metascore),
          filmwebUrl        = existing.flatMap(_.filmwebUrl),
          filmwebRating     = existing.flatMap(_.filmwebRating),
          rottenTomatoes    = existing.flatMap(_.rottenTomatoes),
          tmdbId            = Some(hit.id),
          metacriticUrl     = existing.flatMap(_.metacriticUrl),
          rottenTomatoesUrl = existing.flatMap(_.rottenTomatoesUrl),
          data              = carriedData + ((Tmdb: Source) -> tmdbSlot)
        )
        // Re-key the row by its resolved year when the cinema didn't supply
        // one and TMDB did. The cache + Mongo are keyed by `(cleanTitle,
        // year)`; without this, a no-year scrape that TMDB later attaches a
        // year to stays pinned at `(title, None)` while the debug page,
        // controller, and downstream consumers all see `(title, Some(YYYY))`
        // via the resolved accessor. Cinema-supplied years are left
        // untouched — if a cinema reports a year we trust that as the row's
        // identity even when TMDB's year disagrees.
        val targetKey =
          if (key.year.isEmpty && enr.releaseYear.isDefined) cache.keyOf(key.cleanTitle, enr.releaseYear)
          else key
        if (targetKey != key) {
          logger.debug(s"TMDB stage: re-keying '${key.cleanTitle}' (— → ${enr.releaseYear.get}) — cinemas didn't supply a year.")
          cache.invalidate(key)
        }
        cache.put(targetKey, enr)
        (targetKey, enr)
      }
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
  def retryUnresolvedTmdb(): Unit = {
    cache.clearNegatives()
    // Pass each row's `data`-merged director + originalTitle as
    // hints. By the time the daily retry fires, the row has absorbed every
    // cinema's slot via `recordCinemaScrape`'s redirect — even if the cinema
    // that scraped FIRST didn't report a director, a later one might have,
    // and that hint is the only path `directorWalk` can fire on for films
    // TMDB doesn't index under their Polish title.
    val targets = cache.entries.collect { case (k, e) if e.tmdbId.isEmpty => (k, e) }
    logger.info(s"TMDB retry: cleared negatives + re-scheduling ${targets.size} row(s) with missing tmdbId.")
    targets.foreach { case (k, e) =>
      val origHint = e.cinemaOriginalTitle
      val dirHint  = if (e.director.nonEmpty) Some(e.director.mkString(", ")) else None
      if (pending.add(k)) {
        Future(try runTmdbStage(k, origHint, dirHint) finally pending.remove(k))(using ec)
        ()
      }
    }
  }

  // ── TMDB resolution ────────────────────────────────────────────────────────

  // Resolution order:
  //   1. Sister-row alias match — when another cache row sharing any title
  //      alias (cleanTitle or `originalTitle` hint) has already been
  //      TMDB-resolved, inherit that resolution. Avoids year-less search
  //      collisions (Bez końca 1985 vs 2026, Belle 2013 vs Hosoda anime,
  //      etc.). This is where the `originalTitle` hint pulls its weight.
  //   2. Polish-localised TMDB title search, verified by director when the
  //      cinema reported one (rejects same-title-different-film hits).
  //   3. Director-page walk — when the cinema reports a director and the
  //      title path either missed or returned a candidate with a different
  //      director, search TMDB for the director by name and pick their
  //      filmography entry whose year matches the cinema's. Solves the
  //      Niedźwiedzica class of mis-resolution: Polish title "Niedźwiedzica"
  //      maps to Grizzly Falls 1999 on TMDB, but the cinema's reported
  //      director "Asgeir Helgestad" leads us to his 2026 film instead.
  //
  // The IMDb id is OPTIONAL: TMDB doesn't always have a cross-reference yet
  // (very recent releases — e.g. "Za duży na bajki 3" tmdbid 1484486 has no
  // imdb_id at TMDB at the time of writing). When we have only a TMDB hit, we
  // still store the row (Filmweb / MC / RT all key off the title, not the
  // IMDb id); `ImdbIdMissing` fires from the async TMDB stage so
  // `ImdbRatings` can recover the id via IMDb's suggestion endpoint.
  //
  // A `tmdb.search(originalTitle, year)` fallback was previously inserted
  // between (2) and (3). Audit on 356 films across 9 cinemas found 0 films
  // with `originalTitle` set but no `director` — i.e. every film the
  // fallback could uniquely help was also reachable via director-walk.
  // Dropped to keep the chain minimal.
  private def resolveTmdb(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String] = None,
    director:      Option[String] = None
  ): Option[(TmdbClient.SearchResult, Option[String])] = {
    // Sister-row inherits the cached imdbId (no fresh TMDB call needed); the
    // other branches fetch /external_ids per resolved hit.
    def viaTmdb(hit: TmdbClient.SearchResult): (TmdbClient.SearchResult, Option[String]) =
      hit -> tmdb.imdbId(hit.id)

    // sisterRowMatch reads the cache, which is keyed by `searchTitle`-form
    // cleanTitle — `title` already has that shape. tmdb.search hits the
    // external API; strip the accessibility-programme decoration so an
    // "Kino bez barier: Freak Show (AD)" row queries TMDB as "Freak Show"
    // and resolves to the right film instead of falling through to a
    // director-walk false positive onto a different film by the same director.
    sisterRowMatch(title, year, originalTitle)
      .orElse(verifyByDirector(tmdb.search(MovieService.apiQuery(title), year), director).map(viaTmdb))
      .orElse(directorWalk(director, year).map(viaTmdb))
  }

  /** When the cinema reports a director, drop title-search candidates whose
   *  TMDB credits don't include that director — they're probably a same-
   *  title-different-film hit. When no director is reported, pass the
   *  candidate through unchanged.
   *
   *  Director-name comparison is normalize-then-substring: cinemas often
   *  comma-list a single name ("Asgeir Helgestad") while some films have
   *  multiple credited directors, so any TMDB director containing the
   *  cinema's name (or vice versa) counts as a match. */
  private def verifyByDirector(
    candidate: Option[TmdbClient.SearchResult],
    director:  Option[String]
  ): Option[TmdbClient.SearchResult] =
    candidate.flatMap { hit =>
      director match {
        case None => Some(hit)   // no hint → can't verify, accept
        case Some(dir) =>
          val cinemaNames = dir.split(",").iterator.map(_.trim).filter(_.nonEmpty)
            .map(MovieService.normalize).toSet
          if (cinemaNames.isEmpty) Some(hit)
          else {
            val tmdbNames = tmdb.directorsFor(hit.id).map(MovieService.normalize)
            val matches = tmdbNames.exists(t => cinemaNames.exists(c => t.contains(c) || c.contains(t)))
            if (matches) Some(hit) else None
          }
      }
    }

  /** Walk a cinema-reported director's TMDB filmography and pick the entry
   *  whose release year matches the cinema's reported year. Needed when the
   *  title search lands on the wrong film (different decade, different
   *  language, popularity tie-break gone wrong). Year is required — without
   *  it we can't reliably disambiguate among the director's films. */
  private def directorWalk(
    director: Option[String],
    year:     Option[Int]
  ): Option[TmdbClient.SearchResult] = {
    for {
      dir      <- director
      y        <- year
      personId <- tmdb.findPerson(dir.split(",").head.trim)
      film     <- tmdb.personDirectorCredits(personId).find(_.releaseYear.contains(y))
    } yield {
      logger.info(s"Director-walk: '$dir' year=$y → tmdbId=${film.id} '${film.originalTitle.getOrElse(film.title)}'")
      film
    }
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
   *  `MovieService.normalize` so accent/case differences fold to the
   *  same key. */
  private def titleAliases(cleanTitle: String, originalTitle: Option[String]): Set[String] = {
    val parts = Iterator(Option(cleanTitle), originalTitle)
      .flatten
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(MovieService.normalize)
      .filter(_.nonEmpty)
    parts.toSet
  }
}

object MovieService {
  // Stable docId key for the cache + Mongo `_id`. Delegates to
  // `TitleNormalizer.sanitize`, which applies Arabic→Roman, strips display-
  // only decoration (anniversary/Cykl/wersja), folds " & " → " i " and the
  // "Gwiezdne Wojny:" prefix, and finally collapses every non-alphanumeric
  // char so punctuation/whitespace differences ("Top Gun Maverick" vs
  // "Top Gun: Maverick") share a key.
  //
  // Corpus-independent — the same title always produces the same key, so
  // cache lookups + Mongo upserts are stable across refresh ticks regardless
  // of which other films happen to be in the cache at the moment.
  def normalize(title: String): String = TitleNormalizer.sanitize(title)

  // Decoration-stripping lives in `TitleNormalizer` so the same patterns
  // are used for merging (so "Top Gun 40th Anniversary" and "Top Gun"
  // collapse into one card) AND for TMDB/Filmweb lookups (so the search
  // hits the base film). See `TitleNormalizer.searchTitle` for the full
  // list and why each anchor is shaped the way it is.
  def searchTitle(display: String): String = TitleNormalizer.searchTitle(display)

  /** Aggressive stripping for external-API queries — `searchTitle` plus the
   *  accessibility-programme decoration (Kino bez barier, Pokaz sensorycznie,
   *  "(AD + CC + PJM)"). See `TitleNormalizer.apiQuery` for the rationale:
   *  cache keys preserve the accessibility row's identity, but the upstream
   *  resolver should see the bare film title. */
  def apiQuery(display: String): String = TitleNormalizer.apiQuery(display)
}
