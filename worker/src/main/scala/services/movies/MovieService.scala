package services.movies

import clients.TmdbClient
import play.api.Logging
import services.Stoppable
import services.cinemas.CountryNames
import services.events.{DomainEvent, EventBus, ImdbIdMissing, MovieDetailsComplete, TmdbResolved}
import tools.{DaemonExecutors, Env}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import models.{MovieRecord, Source, SourceData, Tmdb}
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

/**
 * Two-stage enrichment pipeline, event-driven.
 *
 *   - **TMDB stage** resolves `(title, year)` → tmdbId + imdbId + originalTitle,
 *     plus Filmweb + Metacritic + Rotten Tomatoes URLs (all of which key off
 *     TMDB's `originalTitle`). Triggered by `MovieDetailsComplete`, and re-run once a day
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
  // How a needed single-movie TMDB resolution is DISPATCHED. Production injects
  // an enqueue of a `ResolveTmdb` task: the worker pool drains it, the task
  // queue retries (`Reschedule`) + dedups it, and `/debug` shows its queue
  // place — single-movie resolution is a first-class worker task, not a hidden
  // side-effect of the bus event. Left `None`, the stage resolves INLINE on the
  // `ec` pool (unit specs, scripts, Mongo-less dev). Either way the resolution
  // WORK is the shared `resolveTmdbOnce`; only this dispatch seam differs.
  enqueueResolveTmdb: Option[(String, Option[Int], Option[String], Option[String]) => Unit] = None
) extends Stoppable with Logging {

  // Active or queued INLINE TMDB-stage lookups, so the inline-default dispatch
  // doesn't run the same key twice concurrently. (Production dedups via the task
  // queue's per-dedupKey idempotency instead.) The IMDb stage doesn't dedup —
  // it's idempotent and cheap.
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()

  // EC notes: each lookup is mostly network wait; virtual threads make per-task
  // concurrency free, and TMDB's published rate limit (~50 req/s) is enforced
  // at the HTTP layer (back off on 429/503) rather than at the thread count.

  // Daily-tick scheduler for `retryUnresolvedTmdb`. The hourly IMDb refresh
  // lives in `ImdbRatings`.
  private val tmdbRetryScheduler = DaemonExecutors.scheduler("tmdb-retry")

  // Periodic-tick scheduler for `settle()` — re-asserting the cache's
  // one-row-per-film invariant (see `settle`). Separate from the daily TMDB
  // retry: collapsing a no-year row onto its resolved-year sibling shouldn't
  // wait a full day.
  private val settleScheduler = DaemonExecutors.scheduler("corpus-settle")

  // First run fires shortly after startup so Mongo hydration has time to
  // populate the cache and we don't race app boot.
  private val StartupDelaySeconds = 10L
  private val TmdbRetryHours      = 24L
  // Cadence of the corpus settle. canonicalizeBySanitize is an in-memory O(N)
  // cache scan that only writes when it finds a variant to collapse, so a
  // few-minute cadence is cheap; tunable for prod load. Default 10 min.
  private val SettleIntervalSeconds = Env.positiveLong("KINOWO_CORPUS_SETTLE_SECONDS", 600L)

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
    logger.info(s"Corpus settle scheduled every ${SettleIntervalSeconds}s (first run in ${StartupDelaySeconds}s).")
    settleScheduler.scheduleAtFixedRate(
      () => Try(settle()).recover {
        case ex => logger.warn(s"Corpus settle tick failed: ${ex.getMessage}")
      },
      StartupDelaySeconds, SettleIntervalSeconds, TimeUnit.SECONDS
    )
  }

  /** Re-assert the cache's one-row-per-film invariant: collapse same-title
   *  spelling/year variants — most importantly a no-year row that a later TMDB
   *  resolve re-keyed onto a resolved year, leaving the original yearless,
   *  unresolved row stranded beside it (the "Dzień objawienia" duplicate). The
   *  collapse logic lives in `MovieCache.canonicalizeBySanitize`; this is the
   *  PRODUCTION caller of it.
   *
   *  Deliberately IN-MEMORY, not corpus-scoped: collapsing over `repo.findAll()`
   *  re-keys rows by their re-derived display title, which runs DURING the
   *  enrichment cascade and races in-flight Filmweb/TMDB writes — a non-
   *  determinism the order guard (`ScrapeOrderDeterminismSpec`) catches. The
   *  in-memory pass is a pure function of the cache, so it's order-independent.
   *  Cache completeness (the reason a corpus pass was tried) is instead
   *  guaranteed by the boot-hydrate retry, so a quiescent row is in the cache
   *  for this pass to see.
   *
   *  For a long time the only caller was the fixture test harness's
   *  `converge()`, so the suite stayed green while prod never collapsed these —
   *  the duplicate lived forever in the live corpus. The harness now delegates
   *  to THIS method, so test and prod settle through one code path. */
  def settle(): Unit = cache.canonicalizeBySanitize()

  /** Drain the INLINE enrichment pool (`ec`) so any in-flight inline TMDB
   *  resolution finishes — its upserts hit Mongo and its TmdbResolved /
   *  ImdbIdMissing events fire (downstream listeners dispatch synchronously on
   *  this thread) — before `MovieRepo` closes its client. The caller
   *  (`AppLoader`) registers this hook so the repo's close runs strictly after.
   *
   *  In production single-movie resolution runs as a `ResolveTmdb` worker task,
   *  not on this pool, so the TaskWorker's own lifecycle owns its drain (and a
   *  task interrupted mid-resolve is simply re-claimed next boot); this pool is
   *  the inline-default path (Mongo-less dev, scripts). Waits for the whole pool
   *  to drain rather than a fixed window — a fixed cap returned before lookups
   *  against real upstreams finished. */
  def stop(): Unit = {
    ec.shutdown()
    tmdbRetryScheduler.shutdown()
    settleScheduler.shutdown()
    while (!ec.isTerminated) ec.awaitTermination(1, TimeUnit.HOURS)
  }

  // ── Event listeners ───────────────────────────────────────────────────────

  /** Subscribe on the `EventBus` to schedule the TMDB stage when a new title
   *  shows up in the cinema schedule. No-op for rows already resolved or
   *  negative-cached.
   *
   *  Captures the cinema-provided `originalTitle` (when present) as a hint
   *  the TMDB stage can use as a secondary search title — see `resolveTmdb`. */
  val onMovieDetailsComplete: PartialFunction[DomainEvent, Unit] = {
    case MovieDetailsComplete(title, year, originalTitle, director) =>
      if (needsTmdbResolution(cache.keyOf(title, year), originalTitle, director))
        dispatchResolve(title, year, originalTitle, director)
  }

  // ── Public read + manual re-enrich ────────────────────────────────────────

  /** Pure cache lookup — never blocks, never schedules. Misses return None;
   *  the next `MovieDetailsComplete` event re-triggers a background fetch. */
  def get(title: String, year: Option[Int]): Option[MovieRecord] =
    cache.get(cache.keyOf(title, year))

  /** Snapshot of every cached enrichment — for debug tooling. */
  def snapshot(): Seq[StoredMovieRecord] = cache.snapshot()

  /** Reload the positive cache from Mongo. Returns the number of rows loaded.
   *  Wired to the `/debug/rehydrate` admin endpoint; useful when Mongo has
   *  been edited out-of-band and the in-memory cache needs to catch up. */
  def rehydrate(): Int = cache.rehydrate()

  /** Re-resolve `(title, year)` via the TMDB stage on the calling thread and
   *  return the row TMDB resolved (or None if TMDB has no hit). Runs the TMDB
   *  stage only — callers that also want fresh IMDb / Filmweb / Metacritic /
   *  Rotten Tomatoes data should chain the corresponding
   *  `*Ratings.refreshOneSync(title, year)` call themselves (see
   *  `scripts/EnrichmentBackfill` for the pattern). Does NOT publish bus
   *  events, so concurrent listeners don't double-fetch. Used by the backfill
   *  scripts and the enrichment test harness. */
  def reEnrichSync(title: String, year: Option[Int]): Option[MovieRecord] =
    runTmdbStageSync(cache.keyOf(title, year)).map(_._2)

  // ── TMDB stage ─────────────────────────────────────────────────────────────

  /** The cheap in-memory guard: does the TMDB stage have real work for this row?
   *  Run BEFORE dispatch so we never enqueue (or inline-run) a no-op task — a
   *  resolved row shouldn't carry a phantom queue place on `/debug`, and the
   *  normal flow must not re-resolve a settled row (that can flip it to a
   *  more-popular same-title hit). The handler re-checks this too, so a row
   *  resolved between enqueue and execution is skipped. */
  private def needsTmdbResolution(
    key:           CacheKey,
    originalTitle: Option[String],
    director:      Option[String]
  ): Boolean = {
    val existing = cache.get(key)
    existing.flatMap(_.tmdbId) match {
      case Some(currentId) =>
        // Already resolved — but a director-less first scrape can land the WRONG
        // same-title film (a query with no director picks TMDB's most-popular
        // hit). The director is the only signal we can check a resolution
        // against, so a re-scrape that brings none keeps the current id (no TMDB
        // call). When THIS event carries a director, RE-VERIFY: if none of the
        // row's reported directors appear in the current tmdbId's credits, the
        // resolution is contradicted → re-resolve (the title search +
        // `directorWalk` will land the right film). When it still verifies, keep
        // it — that keep makes the stage idempotent: a correctly-resolved row
        // never re-resolves, so re-running it on every director-bearing change
        // can't churn TMDB or loop on the stage's own writes.
        if (director.isEmpty) false
        else {
          val dirs = reportedDirectors(existing, director)
          if (dirs.isEmpty ||
              verifyByDirector(Some(TmdbClient.SearchResult(currentId, "", None, None, 0.0)), Some(dirs)).isDefined)
            false
          else {
            logger.info(s"TMDB re-resolve: '${key.cleanTitle}' (${key.year.getOrElse("?")}) tmdbId=$currentId " +
                        s"no longer matches the reported director(s) [$dirs] — re-resolving.")
            true
          }
        }
      case None =>
        // Negative cache: short-circuit known misses — but ONLY when this event
        // brings no new resolution signal. A later scrape that carries a director
        // (Helios/Multikino) or originalTitle (the cinema's English title field)
        // the prior attempt didn't have is worth retrying: `directorWalk` runs
        // off the director hint, and a TMDB title search by originalTitle can
        // hit where the Polish title missed. Without this carve-out, a row
        // where the first-scraping cinema reports no director (CinemaCity,
        // Charlie Monroe) stays trapped at tmdbId=None for the full 24h
        // negative TTL — the Kurozając class of regression.
        if (cache.isNegative(key) && originalTitle.isEmpty && director.isEmpty) false
        // A sibling row already knows this raw cinema title (via cinemaTitles)
        // AND has a tmdbId. `recordCinemaScrape`'s redirect has already
        // attached this cinema's slot to that sibling, so running TMDB again
        // would just create a phantom row at the `(title, year)` key that
        // nothing would clean up — wasted TMDB call plus a stale year-
        // divergent row sitting in Mongo forever.
        else if (cache.hasResolvedSiblingByTitle(key.cleanTitle)) false
        else true
    }
  }

  /** Dispatch a needed single-movie TMDB resolution. Production enqueues a
   *  `ResolveTmdb` task (worker-pool drained, retried, deduped, shown on
   *  `/debug`); the inline default runs it on the `ec` pool, deduped by the
   *  in-memory `pending` set (the task queue's job in production). */
  private def dispatchResolve(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String],
    director:      Option[String]
  ): Unit = enqueueResolveTmdb match {
    case Some(enqueue) => enqueue(title, year, originalTitle, director)
    case None =>
      val key = cache.keyOf(title, year)
      if (pending.add(key)) {
        Future(try { resolveTmdbOnce(title, year, originalTitle, director, force = false); () }
               finally pending.remove(key))(using ec)
        ()
      }
  }

  /** The directors reported for a row — every cinema slot's plus the triggering
   *  event's — normalised the same way `resolveTmdb` derives its hint, as one
   *  comma-joined, de-duplicated, sorted string (empty when none). Sorted so the
   *  re-verification is a pure function of the row's state, not arrival order. */
  private def reportedDirectors(existing: Option[MovieRecord], eventDirector: Option[String]): String =
    (eventDirector.toSeq.flatMap(_.split(",")) ++ existing.map(_.data.values.flatMap(_.director).toSeq).getOrElse(Nil))
      .map(_.trim).filter(_.nonEmpty).distinct.sorted.mkString(",")

  /** Resolve ONE film's TMDB id, synchronously on the calling thread, and bring
   *  the row to a definitive TMDB state. This is the shared work both dispatch
   *  seams run — the worker's `ResolveTmdb` task handler in production, the
   *  inline `ec` pool otherwise.
   *
   *    - HIT: `runTmdbStageSync` writes the TMDB-side fields, then we publish
   *      `TmdbResolved` (TMDB had an IMDb id → drives the IMDb rating + RT
   *      listeners) or `ImdbIdMissing` (resolved but no IMDb cross-reference →
   *      `ImdbRatings` falls back to IMDb's suggestion endpoint). Returns true.
   *    - DEFINITIVE MISS: persist `tmdbNoMatch` so the row is `tmdbConcluded`
   *      (→ released to the read model) and that survives a restart. The daily
   *      `retryUnresolvedTmdb` sweep still re-checks it later. Returns true.
   *    - TRANSIENT FAILURE (rate-limit / network blip): returns FALSE without
   *      poisoning the negative cache, so the caller retries — the worker task
   *      returns `Reschedule`; the inline default drops it and the next
   *      scrape / daily sweep re-dispatches.
   *
   *  `force` skips the `needsTmdbResolution` guard — the operator `/debug`
   *  re-enrich button forces a re-resolve even of an already-resolved row
   *  (forcing is the whole point); the normal flow never forces. */
  def resolveTmdbOnce(
    title:         String,
    year:          Option[Int],
    originalTitle: Option[String],
    director:      Option[String],
    force:         Boolean
  ): Boolean = {
    val key = cache.keyOf(title, year)
    // Fall back to the cached row's accumulated hints when the caller brings
    // none — the operator `/debug` re-enrich enqueues only (title, year), so
    // without this its `directorWalk` would never fire (the inline operator
    // path used to derive the same hints from the row directly).
    val (cachedOrig, cachedDir) = cache.get(key).map(tmdbHints).getOrElse((None, None))
    val origHint = originalTitle.orElse(cachedOrig)
    val dirHint  = director.orElse(cachedDir)
    if (!force && !needsTmdbResolution(key, origHint, dirHint)) true
    else Try(runTmdbStageSync(key, origHint, dirHint)) match {
      case Success(Some((finalKey, movieRecord))) => publishTmdbOutcome(finalKey, movieRecord); true
      case Success(None) =>
        cache.markMissing(key)
        // Conclude as a definitive miss AND fold a stranded yearless+idless
        // sibling onto the now-concluded row in one write (same rationale as the
        // hit path), instead of leaving it held back from the read model until
        // the periodic settle.
        val liveKey = cache.canonicalKeyFor(key).getOrElse(key)
        cache.get(liveKey) match {
          case Some(rec) => cache.settleResolved(liveKey, rec.copy(tmdbNoMatch = true))
          case None      => cache.putIfPresent(liveKey, _.copy(tmdbNoMatch = true))
        }
        true
      case Failure(ex) =>
        logger.warn(s"TMDB resolve failed for '${key.cleanTitle}' (${key.year.getOrElse("?")}): ${ex.getMessage}; will retry.")
        false
    }
  }

  /** Resolve a STAGING row's TMDB state — CACHE-FREE (the staging promoter owns
   *  the write to `pending_movies`, and the `movies` merge/settle is deferred to
   *  the fold). Reuses the exact `lookupTmdb` + `buildResolvedRecord` the movies
   *  path runs, with hints derived from the row's own slots. Returns:
   *    - `Some(enriched)` on a HIT — `existing` + tmdbId + Tmdb slot;
   *    - `Some(existing.copy(tmdbNoMatch = true))` on a DEFINITIVE MISS;
   *      (both conclude the row → ready to fold into `movies`)
   *    - `None` on a TRANSIENT failure — leave the row for the promoter to retry.
   *  Publishes no events: rating enrichment runs on the merged `movies` row after
   *  the fold, not on the per-cinema staging rows. */
  def resolveStagingRecord(cleanTitle: String, year: Option[Int], existing: MovieRecord): Option[MovieRecord] = {
    val (origHint, dirHint) = tmdbHints(existing)
    Try(lookupTmdb(cleanTitle, year, origHint, dirHint)) match {
      case Success(Some((hit, imdbId, detailsOpt))) => Some(buildResolvedRecord(hit, imdbId, detailsOpt, existing))
      case Success(None)                            => Some(existing.copy(tmdbNoMatch = true))
      case Failure(ex) =>
        logger.warn(s"Staging TMDB resolve failed for '$cleanTitle' (${year.getOrElse("?")}): ${ex.getMessage}; will retry.")
        None
    }
  }

  // Publish the post-resolution event so the rating refreshers re-run for the
  // row off the existing event chain.
  private def publishTmdbOutcome(finalKey: CacheKey, movieRecord: MovieRecord): Unit =
    movieRecord.imdbId match {
      case Some(id) =>
        bus.publish(TmdbResolved(finalKey.cleanTitle, finalKey.year, id))
        logger.debug(s"TMDB stage: ${finalKey.cleanTitle} (${finalKey.year.getOrElse("?")}) → $id")
      case None =>
        // IMDb's suggestion endpoint sees the cleaned-up form when TMDB didn't
        // ship an originalTitle, so accessibility-decorated rows ("Kino bez
        // barier: Freak Show (AD)") query IMDb as just "Freak Show". TMDB's
        // originalTitle, when present, is already canonical and needs no stripping.
        val searchTitle = movieRecord.originalTitle.getOrElse(MovieService.apiQuery(finalKey.cleanTitle))
        logger.debug(s"TMDB stage: ${finalKey.cleanTitle} (${finalKey.year.getOrElse("?")}) → tmdbId=${movieRecord.tmdbId.getOrElse("—")} (no IMDb cross-reference yet); publishing ImdbIdMissing(search='$searchTitle')")
        bus.publish(ImdbIdMissing(finalKey.cleanTitle, finalKey.year, searchTitle))
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
    rawKey:            CacheKey,
    originalTitleHint: Option[String] = None,
    directorHint:      Option[String] = None
  ): Option[(CacheKey, MovieRecord)] = {
    // The event may carry a `(title, year)` the row no longer lives under —
    // `recordCinemaScrape` canonicalises a film's key as variants fold, so an
    // early cinema's `MovieDetailsComplete` can address a stale key. Resolve to
    // the live row's key up front so the read / carry-forward / re-key below all
    // act on the real row instead of spawning a phantom at the stale key.
    val key = cache.canonicalKeyFor(rawKey).getOrElse(rawKey)
    // The slow HTTP (search + full-details fetch) happens OUTSIDE the title lock
    // via the cache-free `lookupTmdb`, so concurrent cinema scrapes for the same
    // title aren't blocked for its duration. The cache read → carry-forward →
    // re-key → settle all happen under the lock below.
    lookupTmdb(key.cleanTitle, key.year, originalTitleHint, directorHint).map { case (hit, imdbId, detailsOpt) =>
      // Read → modify → write under the per-title lock so a cinema scrape's
      // freshly-written slot, landing just before this thread enters the
      // critical section, is visible to the carry-forward below — and so
      // the rekey's invalidate→put sequence can't leave any window for a
      // concurrent scrape to see no sibling and spawn a phantom row (the
      // "Straszny film" twins regression).
      cache.withTitleLock(key.cleanTitle) {
        // Re-resolve the canonical key INSIDE the lock. `key` was captured
        // before the slow lookup above; a concurrent `recordCinemaScrape` may
        // have rekeyed the row to a different-cased / different-separator
        // canonical spelling in the meantime (e.g. "Nowa fala" → "Nowa Fala",
        // "Monterey Pop | DKF" → "Monterey Pop_DKF"). Writing under the now-stale
        // `key` would resurrect a PHANTOM row at the old spelling — the
        // order-dependent split that left a film under two titles run-to-run.
        // `canonicalKeyFor` shares this row's sanitize (so the same title lock),
        // and falls back to `key` only when no live row exists yet (first resolve).
        val writeKey = cache.canonicalKeyFor(rawKey).getOrElse(key)
        val enr      = buildResolvedRecord(hit, imdbId, detailsOpt, cache.get(writeKey).getOrElse(MovieRecord()))
        // Settle this film at conclusion: write the resolved record AND fold any
        // yearless+idless sibling a concurrent scrape stranded (the "Dzień
        // objawienia" Multikino row) onto it in ONE merged write — so the row's
        // first `readyToProject` upsert already carries every cinema and the read
        // model is copied to `web_movies` only after the settle, never showing
        // the single-cinema split that made the card flicker. Also subsumes the
        // prior narrow re-key of a yearless row onto its resolved TMDB year.
        // `settleResolved` stays on the resolved row's own key and folds only the
        // unambiguous rule-(4) strays, so it's order-independent (the broader
        // ±1-year / remake clustering is owned by the periodic settle).
        val finalKey = cache.settleResolved(writeKey, enr)
        (finalKey, cache.get(finalKey).getOrElse(enr))
      }
    }
  }

  /** Cache-free TMDB lookup: search for `(cleanTitle, year)` and, on a hit, also
   *  fetch the full details. Returns the search hit, its IMDb cross-reference (if
   *  any), and the full-details payload (if the fetch succeeded). None = a
   *  definitive no-match; a thrown exception = a transient failure the caller
   *  retries. Both the movies path (`runTmdbStageSync`) and the staging promoter
   *  run this same lookup. */
  private def lookupTmdb(
    cleanTitle:        String,
    year:              Option[Int],
    originalTitleHint: Option[String],
    directorHint:      Option[String]
  ): Option[(TmdbClient.SearchResult, Option[String], Option[TmdbClient.FullDetails])] =
    resolveTmdb(cleanTitle, year, originalTitleHint, directorHint).map { case (hit, imdbId) =>
      (hit, imdbId, tmdb.fullDetails(hit.id))
    }

  /** Build the resolved `MovieRecord` from a TMDB hit + the row's `existing`
   *  record — pure (no cache, no lock), so the movies path (then `settleResolved`)
   *  and the staging promoter (then `stagingRepo.upsert`) share ONE definition of
   *  how a resolution writes the TMDB-side fields + `Tmdb` slot while carrying the
   *  cinema-side data and score fields forward. */
  private def buildResolvedRecord(
    hit:        TmdbClient.SearchResult,
    imdbId:     Option[String],
    detailsOpt: Option[TmdbClient.FullDetails],
    existing:   MovieRecord
  ): MovieRecord = {
    // Preserve the previously-known `imdbId` when TMDB resolved the same film
    // (same `tmdbId`) but momentarily dropped the cross-reference — happens for
    // very recent releases and occasional TMDB data hiccups. A DIFFERENT tmdbId
    // accepts the new film's imdbId (even None) so a stale id can't leak across.
    val preserveImdbId = existing.tmdbId.contains(hit.id)
    val resolvedImdbId = imdbId.orElse(if (preserveImdbId) existing.imdbId else None)
    // Carry the cinema-side fields forward — the TMDB stage doesn't own cinema
    // data; without this a fresh resolve would wipe every cinema's slot.
    val carriedData      = existing.data
    // Fetch the full TMDB record in a single round-trip so the SourceData (Tmdb)
    // slot carries the Polish synopsis, director, cast, runtime, year, countries
    // and poster — not just the search-hit-shape fields. On a fetch failure fall
    // back to the search-hit shape so the row at least keeps title + year.
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
        // Canonicalise TMDB's English country names ("United States of America" →
        // "USA") so the merged-record dedup operates on the same strings cinemas
        // already write.
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
    MovieRecord(
      imdbId            = resolvedImdbId,
      imdbRating        = existing.imdbRating,
      metascore         = existing.metascore,
      filmwebUrl        = existing.filmwebUrl,
      filmwebRating     = existing.filmwebRating,
      rottenTomatoes    = existing.rottenTomatoes,
      tmdbId            = Some(hit.id),
      metacriticUrl     = existing.metacriticUrl,
      rottenTomatoesUrl = existing.rottenTomatoesUrl,
      // A resolve clears any prior `tmdbNoMatch` (default `false` here); carry a
      // pending deferred-detail fetch forward so resolving TMDB first doesn't
      // prematurely mark the row detail-done.
      detailPending     = existing.detailPending,
      data              = carriedData + ((Tmdb: Source) -> tmdbSlot)
    )
  }

  // IMDb / Filmweb / Metacritic / Rotten Tomatoes refresh logic lives in the
  // dedicated *Ratings classes. `AppLoader` subscribes each one's
  // `onTmdbResolved` on the same bus that publishes `TmdbResolved` above, so
  // they react automatically to async re-enrichment. The sync path
  // (`reEnrichSync`) is TMDB-only on purpose — callers that need the score
  // fields chain `*Ratings.refreshOneSync(title, year)` themselves so the
  // worker pool stays uninvolved (see `scripts/EnrichmentBackfill`).

  // Transient TMDB failures (rate limit / network blip) retry FOREVER — a row is
  // only ever released by a *definitive* answer (a hit, or a persisted
  // `tmdbNoMatch`), never by giving up. In production that retry is the task
  // queue's: `ResolveTmdbHandler` returns `Reschedule` on a transient failure,
  // and the queue re-claims the task (with backoff) until it concludes. The
  // inline default just drops a transient failure; the next scrape or the daily
  // `retryUnresolvedTmdb` sweep re-dispatches.

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
   *  `(title, year)` lookups get one fresh shot via the next cinema scrape. */
  def retryUnresolvedTmdb(): Unit = {
    cache.clearNegatives()
    // Pass each row's `data`-merged director + originalTitle as
    // hints. By the time the daily retry fires, the row has absorbed every
    // cinema's slot via `recordCinemaScrape`'s redirect — even if the cinema
    // that scraped FIRST didn't report a director, a later one might have,
    // and that hint is the only path `directorWalk` can fire on for films
    // TMDB doesn't index under their Polish title.
    // Skip rows still awaiting detail enrichment: resolving them now would burn a
    // director-less attempt. `EnrichDetailsHandler` publishes `MovieDetailsComplete`
    // (→ TMDB) once their detail lands, and `DetailReaper` keeps that detail
    // enqueued — so this sweep only re-tries genuinely-stalled, detail-complete rows.
    val targets = cache.entries.collect { case (k, e) if e.tmdbId.isEmpty && !e.detailPending => (k, e) }
    logger.info(s"TMDB retry: cleared negatives + re-dispatching ${targets.size} row(s) with missing tmdbId.")
    targets.foreach { case (k, e) =>
      val (origHint, dirHint) = tmdbHints(e)
      dispatchResolve(k.cleanTitle, k.year, origHint, dirHint)
    }
  }

  /** The originalTitle + director hints the TMDB resolution needs, derived from
   *  a cached row — used by the daily retry sweep and as the fallback hints in
   *  `resolveTmdbOnce` when the dispatch carried none (the operator re-enrich). */
  private def tmdbHints(e: MovieRecord): (Option[String], Option[String]) =
    (e.cinemaOriginalTitle, if (e.director.nonEmpty) Some(e.director.mkString(", ")) else None)


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
    def viaTmdb(hit: TmdbClient.SearchResult): (TmdbClient.SearchResult, Option[String]) =
      hit -> tmdb.imdbId(hit.id)

    // Resolve from the row's OWN reported titles. A decorated festival/preview
    // row whose own title doesn't match TMDB ("Opętanie | ŻUŁAWSKI. KINO
    // EKSTAZY", "Ojczyzna (pokaz przedpremierowy)") must still resolve on its
    // own — depending on a relative resolving first was an enrichment-order race
    // that made whole-corpus snapshots flaky. Build candidates from every
    // cinema-reported title + every slot's original (English) title, plus
    // de-decorated forms (each side of a "X | Y" pipe, trailing "(…)" dropped),
    // and try each (verifyByDirector-gated, so extra terms can't mis-resolve
    // onto a same-title different film). `apiQuery` additionally strips the
    // accessibility-programme decoration ("Kino bez barier: Freak Show (AD)" →
    // "Freak Show") before hitting TMDB.
    val rowRecord  = cache.get(cache.keyOf(title, year))
    val cinemaTitles  = rowRecord.map(_.cinemaTitles).getOrElse(Set.empty[String])
    val slotOriginals = rowRecord.map(_.data.values.flatMap(_.originalTitle).toSet).getOrElse(Set.empty[String])
    // Sorted so the candidate order — hence which query resolves first when
    // several map to the same film — is independent of the (Set) iteration
    // order, which varied run-to-run.
    val extraTitles = (cinemaTitles ++ slotOriginals).toSeq.sorted
    val candidates = MovieService
      .searchTitleCandidates(title, originalTitle, extraTitles)
      .map(MovieService.apiQuery).filter(_.nonEmpty).distinct
    // Director hints drawn from the WHOLE merged row, not just the one cinema
    // event that happened to trigger this stage. Every cinema fires its own
    // `MovieDetailsComplete`, so the triggering event's director varied with
    // arrival order (Helios/Multikino report a director, CinemaCity/Charlie
    // Monroe don't) — and a director-bearing trigger that failed verification
    // poisoned the negative cache (`markMissing`) before a director-less trigger
    // could resolve the same row, so whether the film enriched hinged on which
    // event won the per-key `pending` race. Sourcing the hints from the row's
    // own slots (sorted) makes the resolution a deterministic function of the
    // row's state — every event computes the same outcome, so the race is moot.
    val rowDirectors = (director.toSeq.flatMap(_.split(",")) ++
      rowRecord.map(_.data.values.flatMap(_.director).toSeq).getOrElse(Nil))
      .map(_.trim).filter(_.nonEmpty).distinct.sorted
    val searchHit = candidates.iterator
      .flatMap(q => verifyByDirector(tmdb.search(q, year), Some(rowDirectors.mkString(",")).filter(_.nonEmpty)))
      .nextOption()

    // Resolve from this row's own titles only — no sister-row shortcut. Copying
    // a tmdbId from an already-resolved relative was order-dependent (it could
    // only borrow once the relative had resolved), which is what made
    // whole-corpus snapshots flaky. Own-title search + director-walk are
    // order-independent, so the row resolves to the same film every run.
    // Director-walk each reported director in turn (sorted) so a row whose
    // first-sorted director name happens to miss still recovers via the others.
    searchHit.map(viaTmdb)
      .orElse(rowDirectors.iterator.flatMap(d => directorWalk(Some(d), year)).nextOption().map(viaTmdb))
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

  // Decoration-stripping for EXTERNAL LOOKUPS only: strip the anniversary /
  // restored / wersja / Cykl / slash decoration so the TMDB/Filmweb search hits
  // the base film. This does NOT affect identity — a decoration edition keys by
  // its own form and stays a separate card (see `TitleNormalizer.searchTitle`
  // and `MovieCache.keyOf`); it just resolves to the base film's ratings.
  def searchTitle(display: String): String = TitleNormalizer.searchTitle(display)

  /** Aggressive stripping for external-API queries — `searchTitle` plus the
   *  accessibility-programme decoration (Kino bez barier, Pokaz sensorycznie,
   *  "(AD + CC + PJM)"). See `TitleNormalizer.apiQuery` for the rationale:
   *  cache keys preserve the accessibility row's identity, but the upstream
   *  resolver should see the bare film title. */
  def apiQuery(display: String): String = TitleNormalizer.apiQuery(display)

  /** TMDB title-search candidates for a row, in priority order: the row's title,
   *  the cinema-provided original title, then every other reported title
   *  (`extraTitles` = the row's cinemaTitles + per-slot original titles). Each is
   *  additionally expanded with its de-decorated forms — every side of a `" | "`
   *  festival/preview split ("Opętanie | ŻUŁAWSKI. KINO EKSTAZY",
   *  "WTF Fest | Stolik kawowy") and the trailing-parenthetical-stripped form
   *  ("Ojczyzna (pokaz przedpremierowy)" → "Ojczyzna"). Blanks/duplicates
   *  collapse. Callers verify each hit by director, so extra candidates can't
   *  mis-resolve onto a same-title different film. */
  def searchTitleCandidates(title: String, originalTitle: Option[String], extraTitles: Iterable[String] = Nil): Seq[String] = {
    def deDecorate(t: String): Seq[String] = {
      val pipeParts       = if (t.contains(" | ")) t.split("""\s+\|\s+""").toIndexedSeq else Nil
      val noTrailingParen = t.replaceAll("""\s*\([^)]*\)\s*$""", "").trim
      (Seq(t) ++ pipeParts :+ noTrailingParen)
    }
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles)
      .map(_.trim).filter(_.nonEmpty).distinct
      .flatMap(deDecorate)
      .map(_.trim).filter(_.nonEmpty).distinct
  }
}
