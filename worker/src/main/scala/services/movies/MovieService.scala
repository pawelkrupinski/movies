package services.movies

import clients.TmdbClient
import play.api.Logging
import services.Drainable
import services.cinemas.CountryNames
import services.enrichment.{LetterboxdIdResolver, WikidataClient}
import services.events.{DomainEvent, EventBus, ImdbIdMissing, MovieDetailsComplete}
import services.freshness.{FreshnessKind, FreshnessStore, InMemoryFreshnessStore}
import services.resolution.{ResolutionCache, ResolutionKeys, TitleCorroboration, TmdbBasis}
import services.tasks.RatingTasks
import tools.{DaemonExecutors, HttpStatusException}

import models.{MovieRecord, Source, SourceData, Tmdb}
import scala.concurrent.ExecutionContextExecutorService
import scala.util.{Failure, Success, Try}

/**
 * Two-stage enrichment pipeline, event-driven.
 *
 *   - **TMDB stage** resolves `(title, year)` → tmdbId + imdbId + originalTitle,
 *     plus Filmweb + Metacritic + Rotten Tomatoes URLs (all of which key off
 *     TMDB's `originalTitle`). Triggered by `MovieDetailsComplete`, and re-run once a day
 *     for cached rows whose `tmdbId` is still empty. Publishes `ImdbIdMissing`
 *     when TMDB has no IMDb cross-reference so `ImdbIdResolver` recovers the id.
 *   - **IMDb stage** fetches one row's IMDb rating. Enqueued by the queue-driven
 *     `EnrichmentReaper` (capped + phase-spread) once the row carries an
 *     `imdbId`, and refreshed once per the rating TTL — NOT off a bus event.
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
  // Powers the INLINE-default `ResolveDispatcher` (below) — a dedicated unbounded
  // pool so tests/scripts/the fixture harness construct it as before; `Wiring`
  // injects a shared-budget EC so the inline path shares one concurrency cap with
  // the scrape + rating refreshers and can't peg the box on the hourly walk (see
  // `SharedExecutionBudget`). NOT used in production: there the `QueueResolveDispatcher`
  // is wired and resolution runs on the TaskWorker pool, so this EC stays idle.
  executionContext:    ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC("enrichment-worker"),
  // How a needed single-movie TMDB resolution is DISPATCHED. Production injects a
  // `QueueResolveDispatcher`: it enqueues a `ResolveTmdb` task the worker pool
  // drains, the task queue retries (`Reschedule`) + dedups, and `/debug` shows its
  // queue place — single-movie resolution is a first-class worker task, not a hidden
  // side-effect of the bus event. Left `None`, an `InlineResolveDispatcher` resolves
  // INLINE on the `executionContext` pool (unit specs, scripts, Mongo-less dev, the
  // fixture/determinism harness). Either way the resolution WORK is the shared
  // `resolveTmdbOnce`; only this dispatch seam differs.
  dispatcher: Option[ResolveDispatcher] = None,
  // Caches the expensive TMDB-id resolution (the search + director-verify +
  // director-walk) keyed by the film's hints, so two cinema rows reporting the
  // same hints resolve once. `fullDetails`/`imdbId` are still fetched per hit
  // (cheap single round-trips) — only the search loop is cached. Defaults to a
  // passthrough so unit specs/scripts keep resolving live unless they wire one.
  tmdbIdCache: ResolutionCache = ResolutionCache.passthrough,
  // Where the row's TMDB-resolution TIME is stamped (FreshnessKind.TmdbResolve),
  // so `RatingHandler` can measure how long after resolution each site's first
  // rating attempt fired (the EnrichmentReaper first-pass latency metric).
  // Production injects the SHARED store the rating handlers read; tests default
  // to a throwaway in-memory one (the stamp is observability, not correctness).
  freshness: FreshnessStore = new InMemoryFreshnessStore,
  // Immediately enqueue a freshly-PROMOTED newcomer's due rating tasks (see
  // `announceResolvedNewMovie`) so its ratings don't wait for the
  // `EnrichmentReaper`'s next tick. A newcomer fold is a trickle, so this can't
  // reintroduce the corpus-wide enqueue burst the old `TmdbResolved` fan-out was.
  // Default no-op for tests/scripts without a task queue; production passes
  // `RatingEnqueuer.enqueueDueFor` (the SAME enqueuer the reaper walks the corpus with).
  enqueueNewcomerRatings: (CacheKey, MovieRecord) => Unit = (_, _) => (),
  // Re-fetch EVERY rating source for a just-(re)resolved row, ignoring the adaptive
  // cadence. A forced re-resolve (`resetToScrapedData`) strips the row's scores, but
  // the rating freshness stamps survive — so the reaper judges each source "recently
  // checked" and never re-fetches, leaving the film rating-less. This forces them due
  // so the scores come back. Default no-op; production passes the shared
  // `RatingEnqueuer.enqueueDueFor(..., force = true)`.
  forceRatingRefresh: (CacheKey, MovieRecord) => Unit = (_, _) => (),
  // Drop every memoised per-source resolution for a title (the `resolve_*`
  // stores). The forced re-enrich calls this: `scrapedOnly` clears the row's
  // ids/URLs, but without this the re-resolve REPLAYS those caches instead of
  // re-probing, so a wrong answer returns immediately and survives the full 24h
  // TTL — prod, "Odyseja" re-resolving to the memoised wrong Metacritic URL.
  forgetResolutions: String => Unit = _ => (),
  // Fallback id-crosswalk resolver, tried in `resolveTmdbId` ONLY after TMDB
  // title/director search AND `/find`-by-imdbId all miss on a tmdbId-less row.
  // Turns the row's known imdbId into the EXACT tmdbId via Letterboxd's page
  // scrape — coverage of the arthouse / festival long tail TMDB's own indexes
  // miss. Abstains without an imdbId, so it never guesses. Default None so unit
  // specs/scripts resolve as before; `Wiring` injects it.
  letterboxdIdResolver: Option[LetterboxdIdResolver] = None,
  // Resolves a `tmdbId`-less row that carries a Filmweb URL via the Filmweb
  // entity id → Wikidata (P5032 → P4947 = TMDB id). Once Filmweb enrichment is
  // un-gated for `tmdbId`-less rows (see `RatingSources`), a scraper-supplied or
  // Filmweb-discovered URL becomes a resolution route TMDB's own fuzzy search
  // misses — but only under hard year corroboration (see `resolveTmdbId`). Same
  // `WikidataClient` `ImdbIdResolver` uses; default None so specs resolve as
  // before; `Wiring` injects it.
  wikidata:             Option[WikidataClient]        = None,
  // Where a row holding TWO different films sends the cinemas of the second one:
  // `settle` re-diverts them here and the ordinary staging path gives each film a
  // record of its own (see `MixedFilmSplitter`). Defaults to the no-op repository,
  // so a caller without staging simply never splits.
  staging:              services.staging.StagingRepository = services.staging.StagingRepository.empty
) extends Drainable with Logging {
  // Fold titles with the rules the corpus was keyed under, not a process default.
  private val normalizer: services.movies.TitleNormalizer = cache.normalizer

  // How a needed single-movie TMDB resolution is dispatched (see the `dispatcher`
  // ctor param). The inline default dedups by the row's `CacheKey` so it doesn't
  // run the same key twice concurrently (production dedups via the task queue's
  // per-dedupKey idempotency instead); the IMDb stage doesn't dedup — it's
  // idempotent and cheap. The `resolveTmdbOnce` reference is a forward reference
  // from a closure, only invoked once a dispatch fires.
  private val resolveDispatcher: ResolveDispatcher =
    dispatcher.getOrElse(new InlineResolveDispatcher(
      executionContext, cache.keyOf,
      (t, y, ot, d, force) => { resolveTmdbOnce(t, y, ot, d, force); () }))

  // EC notes: each lookup is mostly network wait; virtual threads make per-task
  // concurrency free, and TMDB's published rate limit (~50 req/s) is enforced
  // at the HTTP layer (back off on 429/503) rather than at the thread count.

  // ── Lifecycle ──────────────────────────────────────────────────────────────
  // The scheduled, phase-spread TMDB re-try is owned by
  // `services.tasks.UnresolvedTmdbReaper` (it drives `retryResolve`); the hourly
  // IMDb refresh lives in `ImdbRatings`. This service only owns the
  // `ResolveDispatcher`'s drain — see `stop()`.

  /** Re-assert the cache's one-row-per-film invariant: collapse same-title
   *  spelling/year variants — most importantly a no-year row that a later TMDB
   *  resolve re-keyed onto a resolved year, leaving the original yearless,
   *  unresolved row stranded beside it (the "Dzień objawienia" duplicate). The
   *  collapse logic lives in `MovieCache.canonicalizeBySanitize`.
   *
   *  Deliberately IN-MEMORY, not corpus-scoped: collapsing over `repository.findAll()`
   *  re-keys rows by their re-derived display title, which runs DURING the
   *  enrichment cascade and races in-flight Filmweb/TMDB writes — a non-
   *  determinism the order guard (`ScrapeOrderDeterminismSpec`) catches. The
   *  in-memory pass is a pure function of the cache, so it's order-independent.
   *
   *  The periodic production caller is the [[services.tasks.SettleReaper]] (once
   *  per 30 min, cluster-claimed). Newcomers also settle as they graduate
   *  (`StagingFold.planGroup` runs this collapse over the staging+movies rows
   *  inside the fold). The cache hydrate deliberately does NOT call this — settling
   *  right after a load re-keys rows on their re-derived `displayTitle`, the
   *  per-deploy flap the reaper-on-its-own-tick avoids. Also the determinism
   *  harness's direct-scrape settle (`FixtureTestWiring.converge`).
   *
   *  `backfillEmbeddedYears` first re-keys any yearless row whose title carries a
   *  delimited year onto that year (then canonicalizes) — the settle-path home for
   *  the title-year persist, off the async resolve so it can't race `canonicalRank`. */
  def settle(): Unit = {
    cache.backfillEmbeddedYears()
    // Part of the settle proper, not a sweep of its own: a row holding two films is
    // a consolidation problem, and settle is where the cache already re-keys and
    // merges. Being here also puts it under the convergence suite's settle
    // assertion — "a further settle changes no key, moves no film's cinemas, folds
    // no row and writes nothing" — which is the guarantee a splitter most needs,
    // since a split that the next scrape undoes would churn forever.
    splitsSoFar += mixedFilmSplitter.splitMixedRows()
    ()
  }

  private lazy val mixedFilmSplitter = new MixedFilmSplitter(cache, staging)

  @volatile private var splitsSoFar = 0

  /** How many cinema slots settle has re-diverted as belonging to a SECOND film,
   *  cumulative over this service's life.
   *
   *  Exposed so the corpus-wide suites can assert it stays ZERO. A healthy corpus
   *  needs no splitting — the rows that do are a handful of genuine title
   *  collisions, and neither fixture corpus holds one. So any split firing over a
   *  replayed corpus means the detector has started reading ordinary data as two
   *  films, which is the failure mode that matters: it costs a good row its
   *  cinemas. Three separate signals had to be abandoned for exactly that
   *  (director, uncorroborated title, screening-year), each caught only because
   *  something counted. */
  def mixedFilmSplits: Int = splitsSoFar

  /** Drain the dispatcher's owned pool so any in-flight inline TMDB resolution
   *  finishes — its upserts hit Mongo and its `ImdbIdMissing` event fires (the id
   *  resolver dispatches synchronously on this thread) — before `MovieRepository`
   *  closes its client. The caller (`AppLoader`) registers this hook so the
   *  repository's close runs strictly after.
   *
   *  The `QueueResolveDispatcher` (production) owns no pool — single-movie
   *  resolution runs as a `ResolveTmdb` worker task whose drain is the TaskWorker's
   *  own lifecycle (a task interrupted mid-resolve is simply re-claimed next boot) —
   *  so its `stop()` no-ops. Only the `InlineResolveDispatcher` (Mongo-less dev,
   *  scripts, the fixture harness) drains its `executionContext` pool, waiting for
   *  the whole pool to drain rather than a fixed window — a fixed cap returned
   *  before lookups against real upstreams finished. */
  def stop(): Unit = resolveDispatcher.stop()

  /** Wait for in-flight inline resolutions WITHOUT ending the pool — what a harness
   *  that drains between phases wants, as opposed to [[stop]]'s one-way shutdown. */
  def drain(): Unit = resolveDispatcher.drain()

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
        resolveDispatcher.dispatch(title, year, originalTitle, director)
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
        //
        // Gate this on the `(title, year)` key carrying NO cinema slots of its
        // own. The redirect only fires when this cinema's slot was folded onto
        // the sibling (the Mortal Kombat II production-vs-release year collapse:
        // one film, adjacent years, one canonical key) — leaving this key with
        // no independent row. When this key DOES carry its own cinema slots it's
        // a genuinely distinct film that merely shares a normalised title with a
        // different-year sibling — "Zaproszenie" 2022 ("The Invitation") vs 2026
        // ("The Invite") — and must resolve on its own. Matching on cleanTitle
        // alone (year-blind) trapped every such row at tmdbId=None forever: the
        // scrape path skipped it here, and the daily `retryUnresolvedTmdb` sweep
        // re-dispatches non-forced, so it hit this same guard again.
        else if (existing.forall(_.cinemaData.isEmpty) && cache.hasResolvedSiblingByTitle(key.cleanTitle)) false
        else true
    }
  }

  /** The directors reported for a row — every CINEMA slot's plus the triggering
   *  event's — normalised the same way `resolveTmdb` derives its hint, as one
   *  comma-joined, de-duplicated, sorted string (empty when none). Sorted so the
   *  re-verification is a pure function of the row's state, not arrival order.
   *
   *  `cinemaDirector`, never `data.values` — this feeds the contradiction check
   *  in `needsTmdbResolution`, and reading the derived `Tmdb`/`Imdb`/`Filmweb`
   *  slots let the WRONG film's own director verify the wrong film. The check
   *  could then never fire: "Dreams" sat on Haugerud's "Drømmer" while its only
   *  cinema said Michel Franco, and every re-scrape confirmed it. Sourced from
   *  the cinemas alone the contradiction fires and the row re-resolves itself,
   *  so this whole class heals without an operator (`DirectorWalkResolvesSpec`). */
  private def reportedDirectors(existing: Option[MovieRecord], eventDirector: Option[String]): String =
    (eventDirector.toSeq.flatMap(_.split(",")) ++ existing.map(_.cinemaDirector).getOrElse(Nil))
      .map(_.trim).filter(_.nonEmpty).distinct.sorted.mkString(",")

  /** Reset a row to its scraped-only form ([[MovieRecord.scrapedOnly]]) and re-key it
   *  onto the SCRAPED year, returning the key to resolve against. The scraped year comes
   *  from the cinema slots (the stripped row's `resolvedYear` — its `tmdbYear` is gone),
   *  so a row self-locked on a wrong resolved year escapes: the lookup re-scopes to the
   *  cinema-reported year. `rekey` invalidates the old key (cache + Mongo) and persists
   *  the stripped row at the new one, so no stale/duplicate row is left. No-op (returns
   *  the key unchanged) when no row is cached. Only the forced re-enrich uses this. */
  private def resetToScrapedData(rawKey: CacheKey): CacheKey = {
    val live = cache.canonicalKeyFor(rawKey).getOrElse(rawKey)
    cache.get(live) match {
      case None      => rawKey
      case Some(row) =>
        val newKey = cache.keyOf(live.cleanTitle, row.scrapedOnly.resolvedYear)
        cache.clearNegative(live)
        cache.clearNegative(newKey)
        cache.rekey(live, newKey, _.scrapedOnly)
        newKey
    }
  }

  /** Resolve ONE film's TMDB id, synchronously on the calling thread, and bring
   *  the row to a definitive TMDB state. This is the shared work both dispatch
   *  seams run — the worker's `ResolveTmdb` task handler in production, the
   *  inline `executionContext` pool otherwise.
   *
   *    - HIT: `runTmdbStageSync` writes the TMDB-side fields. Ratings are enqueued
   *      by the `EnrichmentReaper` (not on resolution); only a hit without an IMDb
   *      cross-reference publishes `ImdbIdMissing` so `ImdbIdResolver` recovers the
   *      id via IMDb's suggestion endpoint. Returns true.
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
    // The operator's forced re-enrich (the /debug button — the only caller that sets
    // `force`) re-resolves off SCRAPED data, not the previously-resolved data: reset the
    // row to its cinema slots and re-key onto the scraped year, so the lookup below scopes
    // to the cinema-reported year/titles instead of a stale resolved year that would
    // re-confirm the same wrong film (a self-locked row — e.g. "Plenerowe Pałacowe:
    // Parasite" stuck at the 1982 film under key …|1982 while the cinema reports 2019).
    val key = if (force) {
      val rawKey = cache.keyOf(title, year)
      // BEFORE the reset, and unconditionally on force — not inside
      // `resetToScrapedData`, which no-ops when no row is cached yet. A stale
      // memoised resolution outlives the row, so an operator forcing a re-enrich
      // of an evicted/cold row would otherwise still replay it. One call covers
      // the re-keyed form too: `rekey` requires a shared normalised cleanTitle,
      // which is exactly what the resolution keys match on.
      forgetResolutions(rawKey.cleanTitle)
      resetToScrapedData(rawKey)
    } else cache.keyOf(title, year)
    // Fall back to the cached row's accumulated hints when the caller brings
    // none — the operator `/debug` re-enrich enqueues only (title, year), so
    // without this its `directorWalk` would never fire (the inline operator
    // path used to derive the same hints from the row directly). After a forced
    // reset the row holds only scraped slots, so these hints are scraped-only too.
    val (cachedOrig, cachedDirectory) = cache.get(key).map(tmdbHints).getOrElse((None, None))
    val origHint = originalTitle.orElse(cachedOrig)
    val directoryHint  = director.orElse(cachedDirectory)
    if (!force && !needsTmdbResolution(key, origHint, directoryHint)) true
    else {
      logger.info(s"TMDB: resolving '${key.cleanTitle}' (${key.year.getOrElse("?")})" +
        directoryHint.fold("")(d => s" [director hint: $d]"))
      Try(runTmdbStageSync(key, origHint, directoryHint)) match {
      case Success(Some((finalKey, movieRecord))) =>
        publishTmdbOutcome(finalKey, movieRecord)
        // A FORCED re-resolve stripped the row to scraped data, dropping its scores;
        // force a re-fetch of every rating source now so they come back (the cadence
        // would otherwise judge the surviving stamps fresh and never re-fetch). A
        // normal resolve doesn't strip and lets the reaper enqueue ratings as due.
        if (force) forceRatingRefresh(finalKey, movieRecord)
        true
      case Success(None) =>
        logger.info(s"TMDB: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match")
        cache.markMissing(key)
        // Conclude as a definitive miss AND fold a stranded yearless+idless
        // sibling onto the now-concluded row in one write (same rationale as the
        // hit path), instead of leaving it held back from the read model until
        // a later settle.
        val liveKey = cache.canonicalKeyFor(key).getOrElse(key)
        cache.get(liveKey) match {
          case Some(record) => cache.settleResolved(liveKey, record.copy(tmdbNoMatch = true))
          case None      => cache.putIfPresent(liveKey, _.copy(tmdbNoMatch = true))
        }
        true
      case Failure(exception) =>
        logger.warn(s"TMDB resolve failed for '${key.cleanTitle}' (${key.year.getOrElse("?")}): ${exception.getMessage}; will retry.")
        false
      }
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
   *  Publishes no events: rating enrichment is set up on the merged `movies`
   *  row at fold time (`announceResolvedNewMovie`, driven by the folder's
   *  `newPromotions`), not on the per-cinema staging rows. */
  def resolveStagingRecord(cleanTitle: String, year: Option[Int], existing: MovieRecord): Option[MovieRecord] = {
    val (origHint, directoryHint) = tmdbHints(existing)
    val label = s"'$cleanTitle' (${year.getOrElse("?")})"
    Try(lookupTmdb(cleanTitle, year, existing, origHint, directoryHint)) match {
      case Success(Some((tmdbId, hit, externalIds, detailsOpt, basis))) =>
        val resolved = buildResolvedRecord(tmdbId, hit, externalIds, detailsOpt, existing, basis)
        logger.info(s"TMDB (staging): $label → matched tmdbId=${resolved.tmdbId.getOrElse("—")} imdbId=${resolved.imdbId.getOrElse("—")}")
        Some(resolved)
      case Success(None) =>
        logger.info(s"TMDB (staging): $label → no match")
        Some(existing.copy(tmdbNoMatch = true))
      case Failure(exception) =>
        logger.warn(s"Staging TMDB resolve failed for $label: ${exception.getMessage}; will retry.")
        None
    }
  }

  /** Announce a brand-new movie's resolution outcome when it's promoted out of
   *  staging (`StagingFolder.foldGroup`'s `newPromotions`): stamp its resolution
   *  time (for the first-rating delay metric), kick IMDb-id recovery for a TMDB-only
   *  hit (`ImdbIdMissing` → `ImdbIdResolver`), and IMMEDIATELY enqueue the now-eligible
   *  rating tasks (`enqueueNewcomerRatings`) so a newcomer's ratings don't wait for the
   *  `EnrichmentReaper`'s next tick. A newcomer fold is a trickle (a handful a day), so
   *  the immediate kick can't recreate the corpus-wide burst the old `TmdbResolved`
   *  fan-out was — the bulk corpus is still owned by the reaper's capped, phase-spread
   *  walk. The stamp happens BEFORE the enqueue so the first-rating delay metric has a
   *  baseline. Only resolved promotions (a TMDB id) qualify: a `tmdbNoMatch` promotion
   *  has no id to recover or query ratings against. A row resolved without an imdbId
   *  enqueues only its non-IMDb ratings now; IMDb follows once `ImdbIdResolver` lands
   *  the id and the reaper picks it up. */
  def announceResolvedNewMovie(key: CacheKey, record: MovieRecord): Unit =
    if (record.tmdbId.isDefined) {
      publishTmdbOutcome(key, record)
      enqueueNewcomerRatings(key, record)
    } else if (record.tmdbNoMatch && record.imdbId.isEmpty) {
      // TMDB found nothing, so the match path above never published `ImdbIdMissing`
      // and the film would only ever get an id from the once-daily OMDb sweep. Kick
      // the same id-recovery chain HERE too: `ImdbIdResolver` runs its full ladder
      // (IMDb suggestion → director → Filmweb/Wikidata → Letterboxd → OMDb → Wikidata-title
      // → Cinemeta) against the freshly-folded cached row. This is what lets the
      // TMDB-less long tail (niche/foreign titles — the Flicks catalogue in
      // particular) land an imdbId → rating AND a resolved year that stabilises its
      // read-model key, instead of waiting hours for the sweep. The id is the only
      // effect; ratings follow once the reaper sees the now-eligible row.
      val searchTitle = record.searchTitle.orElse(record.originalTitle).getOrElse(cache.normalizer.searchQuery(key.cleanTitle))
      logger.info(s"TMDB: '${key.cleanTitle}' (${key.year.getOrElse("?")}) → no match; publishing ImdbIdMissing(search='$searchTitle') to attempt id recovery")
      bus.publish(ImdbIdMissing(key.cleanTitle, key.year, searchTitle))
    }

  // Publish the post-resolution event so the rating refreshers re-run for the
  // row off the existing event chain.
  private def publishTmdbOutcome(finalKey: CacheKey, movieRecord: MovieRecord): Unit = {
    // Stamp WHEN this row resolved (keyed by the immutable tmdbId) so the rating
    // handler can measure the resolved → first-rating-attempt delay per site.
    movieRecord.tmdbId.foreach(id => freshness.markFresh(RatingTasks.tmdbResolvedAtKey(id), FreshnessKind.TmdbResolve))
    movieRecord.imdbId match {
      case Some(id) =>
        // imdbId already known → nothing to recover; the EnrichmentReaper picks up
        // this row's ratings on its next due pass (no per-resolution rating event).
        logger.info(s"TMDB: '${finalKey.cleanTitle}' (${finalKey.year.getOrElse("?")}) → matched tmdbId=${movieRecord.tmdbId.getOrElse("—")} imdbId=$id")
      case None =>
        // IMDb's suggestion endpoint sees the cleaned-up form when TMDB didn't
        // ship an originalTitle, so accessibility-decorated rows ("Kino bez
        // barier: Freak Show (AD)") query IMDb as just "Freak Show". TMDB's
        // originalTitle, when present, is already canonical and needs no stripping.
        val searchTitle = movieRecord.searchTitle.orElse(movieRecord.originalTitle).getOrElse(cache.normalizer.searchQuery(finalKey.cleanTitle))
        logger.info(s"TMDB: '${finalKey.cleanTitle}' (${finalKey.year.getOrElse("?")}) → matched tmdbId=${movieRecord.tmdbId.getOrElse("—")} (no IMDb cross-reference yet); publishing ImdbIdMissing(search='$searchTitle')")
        bus.publish(ImdbIdMissing(finalKey.cleanTitle, finalKey.year, searchTitle))
    }
  }

  // Synchronous core. Resolves TMDB; on a hit, writes a row carrying ONLY the
  // TMDB-side fields (tmdbId, imdbId, originalTitle). All score/URL fields
  // (IMDb rating, Metacritic URL+score, RT URL+score, Filmweb URL+rating)
  // are owned by the dedicated *Ratings classes — the `EnrichmentReaper`
  // enqueues each one's per-row refresh. The TMDB stage preserves any existing
  // values for those fields so a re-resolve doesn't blank them while the rating
  // refreshes catch up. Returns the new
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
    // Mine candidates from the live cache row (same read `resolveTmdb` did
    // internally before `row` was passed in) — outside the lock, like the slow
    // lookup it feeds.
    val candidateRow = cache.get(cache.keyOf(key.cleanTitle, key.year)).getOrElse(MovieRecord())
    lookupTmdb(key.cleanTitle, key.year, candidateRow, originalTitleHint, directorHint).map { case (tmdbId, hit, externalIds, detailsOpt, basis) =>
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
        // Carry-forward reads `stored` (cache, else a direct `movies` read), not
        // the Caffeine-only `get`: a cold / evicted / re-keyed entry would read
        // EMPTY here, and `buildResolvedRecord` would then null every score the
        // `*Ratings` refreshers own + drop the cinema slots — the re-resolve
        // clobber that left already-fetched ratings blank (UK far more than PL,
        // its cache colder). With the real stored record, `buildResolvedRecord`'s
        // own same-tmdbId gate still discards a corrected film's stale ids.
        // A FAILED read is not an empty row. `stored` reports both as `None`, and carrying
        // `MovieRecord()` forward would write the film stripped of every rating the
        // `*Ratings` refreshers own AND every cinema slot — the same clobber the cold-cache
        // fix above prevented, from the other cause. THROW rather than return `None`:
        // `None` means "TMDB has no match" here, and `resolveTmdbOnce` turns that into
        // `markMissing` + `tmdbNoMatch = true`, poisoning a film that is perfectly fine.
        // Its `Try` already treats a Failure as "will retry", which is the deferral wanted.
        val (carryForward, readOk) = cache.storedChecked(writeKey)
        if (!readOk) throw new IllegalStateException(
          s"TMDB carry-forward read failed for '${writeKey.cleanTitle}' (${writeKey.year.getOrElse("—")}) — " +
          "deferring the resolve rather than writing the row without its ratings and cinemas")
        val enr      = buildResolvedRecord(tmdbId, hit, externalIds, detailsOpt, carryForward.getOrElse(MovieRecord()), basis)
        // Settle this film at conclusion: write the resolved record AND fold any
        // yearless+idless sibling a concurrent scrape stranded (the "Dzień
        // objawienia" Multikino row) onto it in ONE merged write — so the row's
        // first `readyToProject` upsert already carries every cinema and the read
        // model is copied to `web_movies` only after the settle, never showing
        // the single-cinema split that made the card flicker. Also subsumes the
        // prior narrow re-key of a yearless row onto its resolved TMDB year.
        // `settleResolved` stays on the resolved row's own key and folds only the
        // unambiguous rule-(4) strays, so it's order-independent (the broader
        // ±1-year / remake clustering is owned by `canonicalizeBySanitize` — run
        // by the staging fold and on every rehydrate).
        val finalKey = cache.settleResolved(writeKey, enr)
        // Re-read so the reported record carries the strays `settleResolved` folded
        // in — but never report a row LESS resolved than the one just persisted
        // (see `resolvedView`).
        (finalKey, MovieService.resolvedView(cache.get(finalKey), enr))
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
    row:               MovieRecord,
    originalTitleHint: Option[String],
    directorHint:      Option[String]
  ): Option[(Int, Option[TmdbClient.SearchResult], TmdbClient.ExternalIds, Option[TmdbClient.FullDetails], TmdbBasis)] =
    resolveTmdbId(cleanTitle, year, row, originalTitleHint, directorHint).flatMap { case (tmdbId, hit, basis) =>
      externalIdsOfLiveMovie(tmdbId, cleanTitle).map(ids => (tmdbId, hit, ids, tmdb.fullDetails(tmdbId), basis))
    }

  /** The candidate's cross-reference ids, or None when TMDB answers 404 — the id
   *  its search/find index just handed us no longer exists.
   *
   *  TMDB deletes movie entries (duplicates, cancelled productions) while the
   *  search index keeps serving them for a while, so a dead candidate is a
   *  normal, PERMANENT outcome rather than a failure to retry. It matters
   *  because this was the one unguarded throw in `lookupTmdb` — `fullDetails`
   *  already swallows — and `ResolveTmdbHandler` reschedules every throw as
   *  transient with no attempts ceiling: one dead id parked the UK row
   *  "Blade (2025)" at the 30-min backoff cap for six hours (attempts=20), the
   *  whole time reading as head-of-line starvation on the oldest-waiting-age
   *  panel. Returning None concludes it as a no-match, which the missing-id
   *  reaper re-attempts on its own cadence; `forget` drops the memoised id so
   *  that re-attempt genuinely re-searches instead of replaying the corpse for
   *  the resolution cache's 24h. A 5xx/429/IO failure still throws — a real
   *  TMDB outage must defer, not stamp the corpus unmatched. */
  private def externalIdsOfLiveMovie(tmdbId: Int, cleanTitle: String): Option[TmdbClient.ExternalIds] =
    try Some(tmdb.externalIds(tmdbId))
    catch {
      case e: HttpStatusException if e.code == 404 =>
        logger.warn(s"TMDB id $tmdbId for '$cleanTitle' is gone (${e.getMessage}) — treating the candidate as no match")
        tmdbIdCache.forget(cleanTitle)
        None
    }

  /** Build the resolved `MovieRecord` from a TMDB hit + the row's `existing`
   *  record — pure (no cache, no lock), so the movies path (then `settleResolved`)
   *  and the staging promoter (then `stagingRepository.upsert`) share ONE definition of
   *  how a resolution writes the TMDB-side fields + `Tmdb` slot while carrying the
   *  cinema-side data and score fields forward. */
  private def buildResolvedRecord(
    tmdbId:      Int,
    hit:         Option[TmdbClient.SearchResult],
    externalIds: TmdbClient.ExternalIds,
    detailsOpt:  Option[TmdbClient.FullDetails],
    existing:    MovieRecord,
    basis:       TmdbBasis
  ): MovieRecord = {
    // Preserve the previously-known `imdbId`/`wikidataId` when TMDB resolved the
    // same film (same `tmdbId`) but momentarily dropped a cross-reference —
    // happens for very recent releases and occasional TMDB data hiccups. A
    // DIFFERENT tmdbId accepts the new film's ids (even None) so a stale id
    // can't leak across.
    val sameFilm         = existing.tmdbId.contains(tmdbId)
    val resolvedImdbId   = externalIds.imdbId.orElse(if (sameFilm) existing.imdbId else None)
    val resolvedWikidata = externalIds.wikidataId.orElse(if (sameFilm) existing.wikidataId else None)
    // Every rating url and score describes a FILM, so they follow the film.
    // Carrying them forward unconditionally meant a corrected row kept the WRONG
    // film's numbers until each source's own refresh cadence came round: prod
    // served Michel Franco's "Dreams" with the Norwegian film's
    // `/movie/dreams-drommer` and metascore 81 long after the tmdbId was fixed.
    //
    // The trigger is a film that demonstrably CHANGED — not merely "not the same
    // one". A row resolving for the FIRST time has no previous tmdbId, so there is
    // nothing stale to drop and a rating a refresher already wrote must survive
    // (`TmdbCarryForwardReadFailureSpec`). Once cleared, the `*Ratings` enrichers
    // re-resolve from the new identity.
    val differentFilm = existing.tmdbId.exists(_ != tmdbId)
    def ifSameFilm[A](value: Option[A]): Option[A] = if (differentFilm) None else value
    // Carry the cinema-side fields forward — the TMDB stage doesn't own cinema
    // data; without this a fresh resolve would wipe every cinema's slot.
    val carriedData      = existing.data
    // Fetch the full TMDB record in a single round-trip so the SourceData (Tmdb)
    // slot carries the Polish synopsis, director, cast, runtime, year, countries
    // and poster — not just the search-hit-shape fields. On a fetch failure fall
    // back to the search-hit shape so the row at least keeps title + year.
    val existingTmdbSlot = carriedData.getOrElse(Tmdb, SourceData())
    // The search hit's title/originalTitle/year are the fallback when the full
    // details fetch fails. `hit` is present on a fresh resolution and None on a
    // cache hit (the cache stores only the id) — in that rare double case the
    // slot keeps whatever it already had, and the next resolution fills it.
    val hitTitle = hit.map(_.title).filter(_.nonEmpty)
    // TMDB's English release title (en-US `title`, via the same `details` call
    // MC/RT use). For a non-Latin-original film whose Polish `title` and
    // `originalTitle` both differ from the English title a cinema lists it under
    // ("Left-Handed Girl"), this is the alias that folds the English-keyed
    // duplicate onto the Polish-titled row — see `MovieRecord.tmdbTitleAliases`.
    val englishTitle = tmdb.englishTitle(tmdbId).orElse(existingTmdbSlot.englishTitle)
    // Whether the slot we're merging onto was fetched in the language we're
    // fetching in NOW. When it wasn't — the stale-language re-resolve
    // `UnresolvedTmdbReaper` exists to drive — its LOCALIZED text (title,
    // synopsis, genres, poster) is exactly the wrong-language content we came
    // to replace, so it must not survive as an `.orElse` fallback. Letting it
    // through while stamping the slot with the new language would seal the row
    // in: it reads as correctly-localised, and the reaper never looks again.
    // Language-neutral fields (runtime, year, cast, director) carry over as before.
    val existingMatchesLanguage = existingTmdbSlot.fetchedLanguageTag == tmdb.language.toLanguageTag
    def carriedLocalized[A](existing: Option[A]): Option[A] =
      if (existingMatchesLanguage) existing else None
    val tmdbSlot = detailsOpt match {
      case Some(d) => SourceData(
        title          = d.title.orElse(hitTitle).orElse(carriedLocalized(existingTmdbSlot.title)),
        originalTitle  = d.originalTitle.orElse(hit.flatMap(_.originalTitle)).orElse(existingTmdbSlot.originalTitle),
        englishTitle   = englishTitle,
        synopsis       = d.synopsis.orElse(carriedLocalized(existingTmdbSlot.synopsis)),
        cast           = if (d.cast.nonEmpty) d.cast else existingTmdbSlot.cast,
        director       = if (d.director.nonEmpty) d.director else existingTmdbSlot.director,
        runtimeMinutes = d.runtimeMinutes.orElse(existingTmdbSlot.runtimeMinutes),
        releaseYear    = d.releaseYear.orElse(hit.flatMap(_.releaseYear)).orElse(existingTmdbSlot.releaseYear),
        // Canonicalise TMDB's country names in the SAME language TMDB returned
        // them (the client's deployment language): Poland folds "United States
        // of America" → "USA" to match the strings cinemas write; a non-Polish
        // deployment keeps TMDB's already-localised name ("United Kingdom") so
        // it isn't mislabelled with a Polish one.
        countries      = if (d.countries.nonEmpty) d.countries.map(c => CountryNames.canonical(c, tmdb.language)).distinct
                         else if (existingMatchesLanguage) existingTmdbSlot.countries else Seq.empty,
        genres         = if (d.genres.nonEmpty) d.genres
                         else if (existingMatchesLanguage) existingTmdbSlot.genres else Seq.empty,
        posterUrl      = d.posterUrl.orElse(carriedLocalized(existingTmdbSlot.posterUrl)),
        // Per-country age rating (TMDB's certification for the deployment country) —
        // the fallback beneath any cinema-scraped rating (MovieRecord.ageRating is
        // cinema-first). Carried forward if a re-resolve returns none.
        ageRating      = d.ageRating.orElse(existingTmdbSlot.ageRating),
        // Stamp the language these fields were fetched in, so a slot frozen by a
        // pre-locale-fix resolve is detectable rather than silently Polish forever.
        language       = Some(tmdb.language.toLanguageTag)
      )
      case None => existingTmdbSlot.copy(
        title         = hitTitle.orElse(existingTmdbSlot.title),
        originalTitle = hit.flatMap(_.originalTitle).orElse(existingTmdbSlot.originalTitle),
        englishTitle  = englishTitle,
        releaseYear   = hit.flatMap(_.releaseYear).orElse(existingTmdbSlot.releaseYear)
      )
    }
    MovieRecord(
      imdbId            = resolvedImdbId,
      imdbRating        = ifSameFilm(existing.imdbRating),
      metascore         = ifSameFilm(existing.metascore),
      filmwebUrl        = ifSameFilm(existing.filmwebUrl),
      filmwebRating     = ifSameFilm(existing.filmwebRating),
      rottenTomatoes    = ifSameFilm(existing.rottenTomatoes),
      tmdbId            = Some(tmdbId),
      // Keep the EVIDENCE beside the conclusion. Without it a guess from a bare
      // title is indistinguishable ever after from an answer a director's
      // filmography confirmed — which is how five wrong resolutions survived weeks
      // in prod on rows that had since acquired the hints to correct them.
      tmdbBasis         = Some(basis.toString),
      wikidataId        = resolvedWikidata,
      metacriticUrl     = ifSameFilm(existing.metacriticUrl),
      rottenTomatoesUrl = ifSameFilm(existing.rottenTomatoesUrl),
      // A resolve clears any prior `tmdbNoMatch` (default `false` here); carry a
      // pending deferred-detail fetch forward so resolving TMDB first doesn't
      // prematurely mark the row detail-done.
      detailPending     = existing.detailPending,
      data              = carriedData + ((Tmdb: Source) -> tmdbSlot)
    )
  }

  // IMDb / Filmweb / Metacritic / Rotten Tomatoes refresh logic lives in the
  // dedicated *Ratings classes, driven via the queue: the `EnrichmentReaper`
  // enqueues each row's refresh (capped + phase-spread) and a `RatingHandler`
  // runs `refreshOneSync` at pickup — they're no longer driven by a resolution
  // bus event. The sync path (`reEnrichSync`) is TMDB-only on purpose — callers that need
  // the score fields chain `*Ratings.refreshOneSync(title, year)` themselves so
  // the worker pool stays uninvolved (see `scripts/EnrichmentBackfill`).

  // Transient TMDB failures (rate limit / network blip) retry FOREVER — a row is
  // only ever released by a *definitive* answer (a hit, or a persisted
  // `tmdbNoMatch`), never by giving up. In production that retry is the task
  // queue's: `ResolveTmdbHandler` returns `Reschedule` on a transient failure,
  // and the queue re-claims the task (with backoff) until it concludes. The
  // inline default just drops a transient failure; the next scrape or the daily
  // `retryUnresolvedTmdb` sweep re-dispatches.

  // ── Bulk TMDB re-try (operator sweep) ───────────────────────────────────────
  // The scheduled, phase-spread re-try is owned by `UnresolvedTmdbReaper`; this
  // is the corpus-wide form behind the `RefreshAllTmdb` button. The hourly IMDb
  // refresh lives in `ImdbRatings.refreshAll`.

  /** Walk every cached row with no `tmdbId` yet and re-run the TMDB stage on
   *  it. Rows that DO have a `tmdbId` are intentionally left alone — once a
   *  row is TMDB-resolved (whether via title search, sister-row inheritance,
   *  or a manual override), we trust that resolution. Re-resolving could
   *  flip the row to a different film when TMDB's title search lands on a
   *  more popular same-title hit, undoing earlier corrections (override or
   *  sister-row donation). Missing MC / RT / Filmweb URLs are recovered by
   *  the respective `*Ratings.refreshAll` walks — operator-triggered from the
   *  /tasks buttons, NOT scheduled — which do their own
   *  URL discovery; missing IMDb ids are recovered by the `ImdbIdMissing`
   *  event fired from the TMDB stage at first resolution. Clears the negative
   *  cache so previously-failed `(title, year)` lookups get one fresh shot. This
   *  bulk form backs the operator `RefreshAllTmdb` button; the scheduled,
   *  phase-spread re-try is owned by [[services.tasks.UnresolvedTmdbReaper]]
   *  (via [[retryResolve]]) so the backlog drains as a trickle, not a burst. */
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
    targets.foreach { case (k, e) => dispatchWithHints(k, e) }
  }

  /** Re-attempt ONE still-unresolved row's TMDB resolution, clearing just that
   *  row's negative marker first (the scoped form of [[retryUnresolvedTmdb]]'s
   *  global `clearNegatives`). Driven by
   *  [[services.tasks.UnresolvedTmdbReaper]]'s phase-spread tick so the
   *  unresolved backlog re-tries as a flat trickle instead of a boot/period
   *  burst. No-op once the row has resolved or is awaiting detail (its detail
   *  completing re-triggers TMDB via `MovieDetailsComplete`). */
  /** [[retryResolve]] addressed by `(title, year)`, for a caller outside `services`
   *  — `CacheKey` is `private[services]`, so the fixture harness cannot name a row
   *  any other way. Without it the only reachable re-resolve was the operator-scale
   *  [[retryUnresolvedTmdb]], whose corpus-wide `clearNegatives` un-concludes every
   *  unresolved row at once: in the e2e corpus that dropped ten decorated
   *  banner films ("Cinema Italia Oggi: Kochanie", "Kino bez barier: Pieśni lasu")
   *  out of the read model, because `readyToProject` needs `tmdbConcluded` and the
   *  re-dispatch does not restore it. A per-row retry touches only the row that
   *  earned one. */
  def retryResolve(title: String, year: Option[Int]): Unit = retryResolve(cache.keyOf(title, year))

  /** Offer one row's ratings to the enqueuer, addressed by `(title, year)`.
   *
   *  Exists for the same reason the [[retryResolve]] overload above does: `CacheKey`
   *  is `private[services]`, so a caller outside the package cannot name a row. The
   *  fixture harness stands in for `EnrichmentReaper`'s tick, and without this it had
   *  to restate the reaper's eligibility itself — which is precisely how it came to
   *  gate every rating source on `tmdbId` while production gates IMDb on an `imdbId`
   *  and Filmweb on `tmdbId OR filmwebUrl`. Handing the row to the real enqueuer keeps
   *  that judgement in one place. */
  def enqueueRatingsFor(title: String, year: Option[Int]): Unit =
    cache.get(cache.keyOf(title, year)).foreach(record =>
      enqueueNewcomerRatings(cache.keyOf(title, year), record))

  def retryResolve(key: CacheKey): Unit =
    cache.get(key).filter(e => e.tmdbId.isEmpty && !e.detailPending).foreach { e =>
      cache.clearNegative(key)
      dispatchWithHints(key, e)
    }

  /** Re-resolve a row that ALREADY has a `tmdbId`, so its `Tmdb` slot is re-fetched
   *  rather than left frozen at whatever the first resolve stored. The stale-language
   *  sweep ([[services.tasks.UnresolvedTmdbReaper]]) drives this: `fullDetails` is
   *  fetched only at resolve time, so a row enriched before its deployment learned
   *  its own language keeps Polish text until something forces the re-fetch. */
  def forceResolve(key: CacheKey): Unit =
    cache.get(key).foreach(e => dispatchWithHints(key, e, force = true))

  /** Dispatch a row's TMDB resolution with its `data`-merged director +
   *  originalTitle hints (the only path `directorWalk` can fire on for films
   *  TMDB doesn't index under their Polish title). Shared by the bulk
   *  [[retryUnresolvedTmdb]] sweep, the per-row [[retryResolve]], and
   *  [[forceResolve]]. */
  private def dispatchWithHints(key: CacheKey, e: MovieRecord, force: Boolean = false): Unit = {
    val (origHint, directoryHint) = tmdbHints(e)
    resolveDispatcher.dispatch(key.cleanTitle, key.year, origHint, directoryHint, force)
  }

  /** The originalTitle + director hints the TMDB resolution needs, derived from
   *  a cached row — used by the retry sweeps and as the fallback hints in
   *  `resolveTmdbOnce` when the dispatch carried none (the operator re-enrich). */
  private def tmdbHints(e: MovieRecord): (Option[String], Option[String]) =
    (e.cinemaOriginalTitle, if (e.cinemaDirector.nonEmpty) Some(e.cinemaDirector.mkString(", ")) else None)


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
  private def resolveTmdbId(
    title:         String,
    year:          Option[Int],
    row:           MovieRecord,
    originalTitle: Option[String] = None,
    director:      Option[String] = None
  ): Option[(Int, Option[TmdbClient.SearchResult], TmdbBasis)] = {
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
    // `row` carries the cinema slots to mine for candidates. On the movies path
    // it's the live cache row; on the cache-free staging path it's the union of
    // the film's per-cinema staging rows (`resolveStagingRecord`'s `existing`).
    // Both therefore build the SAME candidate set — without it the staging row,
    // absent from the cache, collapsed to its single title and missed films the
    // direct path resolved via a cinema-reported / original title.
    val cinemaTitles  = row.cinemaTitles
    val slotOriginals = row.data.values.flatMap(_.originalTitle).toSet
    // Sorted so the candidate order — hence which query resolves first when
    // several map to the same film — is independent of the (Set) iteration
    // order, which varied run-to-run.
    val extraTitles = (cinemaTitles ++ slotOriginals).toSeq.sorted
    // Try BOTH the raw-stripped and the re-cased-stripped form of every
    // candidate: a cinema that reports a title ALL-CAPS resolves via the re-cased
    // query ("MONTEREY POP" → "Monterey pop"), while one that reports it as-is
    // resolves via the raw query — and TMDB is case-insensitive, so trying both
    // costs nothing but covers every spelling. Order is deterministic
    // (`searchTitleCandidates` is pre-sorted), so resolution is order-independent.
    def queryForms(titles: Iterable[String]): Seq[String] = MovieService
      .searchTitleCandidates(title, originalTitle, titles)
      .flatMap(t => Seq(cache.normalizer.apiQuery(t), cache.normalizer.searchQuery(t)))
      .filter(_.nonEmpty).distinct
    val candidates = queryForms(extraTitles)
    // The same set restricted to what the CINEMAS published — `title` and
    // `originalTitle` are already cinema-side (`tmdbHints` reads
    // `cinemaOriginalTitle`), so only `slotOriginals` is dropped. The walk below
    // ranks a credit matching one of these ABOVE a credit matching a derived
    // title, so a row's own previous resolution can never outbid the cinemas.
    val cinemaCandidates = queryForms(cinemaTitles.toSeq.sorted)
    // How many CINEMA SLOTS published each title form. `cinemaTitles` is a set, so it
    // cannot tell the film 38 venues are showing from the one a single venue lists
    // under a different name — and when a row really does hold two films, both of
    // their titles are cinema-reported and the walk's lowest-id tie-break decides
    // between them by accident. Kino Klaps's "DOBRE Kino - Maryja. Matka Papieża"
    // sat on the "Mistyczka" row that way, and 1646379 < 1731866 handed the row to
    // the one venue. Weight per SLOT, over the same de-decorated forms the search
    // uses, so the banner-stripped "Maryja. Matka Papieża" counts once and
    // "Mistyczka" counts as often as it is published.
    val cinemaTitleWeight: Map[String, Int] = row.cinemaSlots.iterator
      .flatMap { case (_, sd) => sd.title.toSeq }
      .flatMap { t =>
        // The SAME forms `cinemaCandidates` is built from, or a credit could sit in
        // the cinema tier and still score zero: `apiQuery` strips the accessibility
        // decoration ("Kino bez barier: Freak Show (AD)" → "Freak Show") that
        // `searchTitleCandidates` leaves alone, so the venue naming the film that way
        // would not be counted as naming it. `.distinct` per slot because the two
        // query forms usually sanitize to the same string — one venue, one vote.
        MovieService.searchTitleCandidates(t, None)
          .flatMap(f => Seq(cache.normalizer.apiQuery(f), cache.normalizer.searchQuery(f)))
          .map(cache.normalizer.sanitize).filter(_.nonEmpty).distinct
      }
      .toSeq.groupBy(identity).view.mapValues(_.size).toMap
    // Director hints drawn from EVERY cinema slot on the merged row, not just the
    // one cinema event that happened to trigger this stage. Every cinema fires its
    // own `MovieDetailsComplete`, so the triggering event's director varied with
    // arrival order (Helios/Multikino report a director, CinemaCity/Charlie
    // Monroe don't) — and a director-bearing trigger that failed verification
    // poisoned the negative cache (`markMissing`) before a director-less trigger
    // could resolve the same row, so whether the film enriched hinged on which
    // event won the per-key `pending` race. Sourcing the hints from the row's
    // own slots (sorted) makes the resolution a deterministic function of the
    // row's state — every event computes the same outcome, so the race is moot.
    // CINEMA slots only, though. Mining `row.data.values` swept in the derived
    // `Tmdb`/`Imdb`/`Filmweb` slots too — the previous resolution's own output —
    // so a mis-resolved row corroborated itself: it grew a second "reported"
    // director (the wrong film's), and because the hints are `.sorted` and the
    // walk below takes the FIRST that hits, which film the row re-resolved to
    // came down to alphabetical order. Kino Malta's "Dreams" reports Michel
    // Franco; the row's stale match to Dag Johan Haugerud's "Drømmer" sorted
    // ahead of it and re-won every cycle, so Michel Franco was never walked at
    // all (`MovieServiceTmdbHintsSpec`). `cinemaOriginalTitle`, the other half of
    // this hint pair, is cinema-only for exactly the same reason.
    val rowDirectors = (director.toSeq.flatMap(_.split(",")) ++ row.cinemaDirector)
      .map(_.trim).filter(_.nonEmpty).distinct.sorted

    // Cache the id resolution per hint-combination: two cinema rows (or two
    // scrape cycles) with the same title + year + director set + original-title
    // hint resolve to the same film, so the search loop + director-walk runs
    // once and the answer is reused for 24h. The key is built from exactly those
    // hints (sorted, so it's order-independent — see `ResolutionKeys`). Only a
    // HIT is cached; a no-match re-resolves next cycle.
    // A row whose scrape carried no year reads a DELIMITED year written into its
    // title ("Klasyka w NCKF: Generał (1926) 4K", "Following (1998)") as the lookup
    // year — a deterministic hint (pure function of the title). Here it's used only
    // for the SEARCH, so it can't race the canonical rank; the same `EmbeddedYear`
    // is separately PERSISTED onto the row at the scrape boundary
    // (`MovieCache.recordCinemaScrape`), where `canonicalRank` owns the re-key. The
    // `.orElse` means a persisted year already wins here. It unblocks the year-less
    // singleton guard below: with a year the director-less branch takes the safe
    // year-scoped exact-title path instead of refusing every multi-hit title as
    // `searchUnique` does when the year is absent.
    // A year a CINEMA reported, when the row itself has none. A staging row's `year`
    // column is fixed when the newcomer is diverted — before any detail page has been
    // fetched — and the detail then merges `releaseYear` onto the SLOT with nothing
    // back-filling the column. Reading only the column sent films to TMDB year-less
    // whose detail page stated the year plainly: production's Kino Iluzjon slot for
    // "Pokój 666" carries releaseYear 1982 and director Wim Wenders, both parsed from
    // the very page the replay had recorded and merged, and the leg still logged
    // `'Pokój 666' (?) → no match`. A year-less, director-less search is one this
    // method rightly refuses to guess at, so the row never stood a chance.
    //
    // Sorted and distinct so the choice is a pure function of the row rather than of
    // slot iteration order, and taken only when the row has no year of its own — the
    // persisted year still wins. `ImdbIdResolver.resolve` already reads the
    // cinema-reported years exactly this way.
    val reportedYears = row.data.values.flatMap(_.releaseYear).toSeq.distinct.sorted
    // A key year STAMPED FROM A GUESS is not evidence — it is the guess, handed
    // back as if it were a fact. `MovieCache.settleResolved` re-keys a yearless row
    // onto the year of whatever film resolved it, so a TitleOnly conclusion writes
    // its own answer into the key; every later attempt then searches that year and
    // finds the same film, however loudly the cinemas disagree. Prod's
    // `homosapiens|1960` was keyed 1960 because a title-only search picked a
    // 9-minute 1960 short, while twelve venues published 2025. So a row whose
    // conclusion was TitleOnly falls through to what the CINEMAS published — the one
    // year here not derived from the resolution being re-examined. Any stronger
    // basis keeps its year: those were narrowed by evidence in the first place.
    val keyYearIsEvidence = !row.tmdbBasis.flatMap(TmdbBasis.parse).contains(TmdbBasis.TitleOnly)
    val keyYear = Option.when(keyYearIsEvidence)(year).flatten
    // The same trap once more, for the rows that predate a recorded basis — 6194 of
    // prod's 6201 resolved rows carry none, so the guard above passes them all. A key
    // year NO SOURCE EVER REPORTED has only one place it can have come from:
    // `settleResolved` stamping the resolution's own answer back into the key. So it
    // yields to a year the VENUE wrote into its own title, which is the venue saying
    // WHICH film it means. Two UK venues published "Scarface (1932)" and 93 minutes at
    // a row keyed 1983, and the 1983 in the key kept winning.
    //
    // A key year a source DID report is the cinema's own evidence and keeps its
    // precedence, so a decorative "(1926)" on a row scraped as 2015 still loses.
    // `cinemaYears` and not `reportedYears`: the latter includes the TMDB slot's own
    // year, so a mis-resolved row would corroborate its key year with the very
    // resolution being re-examined — "scarface|1983" carries a 1983 TMDB slot.
    val corroboratedKeyYear = keyYear.filter(row.cinemaYears.contains)
    // Only the EMBEDDED year is promoted above the key year. `reportedYears` stays
    // BELOW it, as it always was: it is sorted ascending and spans every slot, so
    // one venue misreporting 1999 on a 2024 film would otherwise hand the search the
    // older year.
    val effectiveYear = corroboratedKeyYear
      .orElse(EmbeddedYear.ofAll(Seq(title) ++ candidates ++ cinemaTitles))
      .orElse(keyYear)
      .orElse(reportedYears.headOption)
    val hintKey = ResolutionKeys.tmdb(title, effectiveYear, rowDirectors, originalTitle, cache.normalizer)
    // `freshHit` captures the SearchResult on a cache MISS (the loader runs on
    // this thread), so the caller keeps the hit's title/year as a fallback when
    // the full-details fetch fails. On a cache HIT the loader doesn't run and it
    // stays None — only the id is cached.
    // Without a DIRECTOR there is no reliable way to tell same-title films apart, so
    // any title search returning several would resolve via the popularity/year-
    // proximity tie-break — a GUESS, and an order-dependent one once a merged year or
    // a more-popular sibling drifts in ("Guru" alone maps to THREE TMDB films: a
    // Persian "لؤ گورو", Yann Gozlan's "Gourou", and his unrelated "Dalloway"). Refuse
    // rather than guess: when no director is reported, resolve ONLY when the search
    // (year-scoped if a year is present) is unambiguous — exactly one result. A
    // director-bearing row keeps the richer director-walk path below, which can
    // disambiguate. This is the generalised "Zaproszenie" guard (a bare title with two
    // same-title TMDB entries) — see StagingOrderDeterminismSpec.
    var searchBasis: TmdbBasis = TmdbBasis.TitleOnly
    var freshHit: Option[TmdbClient.SearchResult] = None
    val resolvedId = tmdbIdCache.getOrResolve(hintKey) {
      val hit =
        if (rowDirectors.isEmpty)
          // First the strict singleton rule; then, when a YEAR is present, accept a
          // year-scoped search whose TOP hit is an exact-title match even if it
          // returned several films (e.g. "Sundown" alongside "Sundown Town", "DJ at
          // Sundown"). The year + verbatim top is confidence the singleton rule
          // lacks — still no popularity guess (a non-exact top doesn't resolve), and
          // yearless rows are untouched (searchYearExactTop is a no-op without a year).
          candidates.iterator.flatMap(q => tmdb.searchUnique(q, effectiveYear)).nextOption()
            .orElse(candidates.iterator.flatMap(q => tmdb.searchYearExactTop(q, effectiveYear)).nextOption())
            .map(hit => { searchBasis = if (effectiveYear.isDefined) TmdbBasis.YearScoped else TmdbBasis.TitleOnly; hit })
        else {
          // Resolve from this row's own titles only — no sister-row shortcut. Copying
          // a tmdbId from an already-resolved relative was order-dependent (it could
          // only borrow once the relative had resolved), which is what made
          // whole-corpus snapshots flaky. Own-title search + director-walk are
          // order-independent, so the row resolves to the same film every run.
          // Director-walk each reported director in turn (sorted) so a row whose
          // first-sorted director name happens to miss still recovers via the others.
          // When the row reports a director, the WALK is the resolution — the only
          // one. It walks the director's filmography and picks the title-matching
          // credit by lowest id, deterministic across a TMDB adjacent-year
          // DUPLICATE of one film (Yann Gozlan's "Gourou").
          //
          // There is deliberately NO title-search fallback here. Verifying a search
          // hit by director only asks "do this candidate's credits name the reported
          // director" — which cannot separate two films by the SAME director. Gozlan's
          // "Gourou" and "Dalloway" both pass that check, so whichever the title
          // search happened to return won, and the answer moved with the row's
          // (merge-order-dependent) key year. Walking the filmography and matching the
          // title is what actually picks between them; when the walk can't find the
          // film, no match is the honest answer, not a rubber-stamped guess
          // (`DirectorWalkResolvesSpec`). Director-LESS rows are unaffected — they
          // still take the strict unambiguous-search branch above, which never picks
          // between candidates either.
          // The row's own cinema-published runtime and cast travel with the walk so
          // a year-pinned credit can be corroborated by something other than the
          // title. CINEMA-only, like every other hint here — reading the merged
          // fields would hand the check the previous resolution's own numbers.
          rowDirectors.iterator
            .flatMap(d => directorWalk(Some(d), effectiveYear, candidates, row.cinemaRuntimesMinutes, row.cinemaCast, cinemaCandidates, cinemaTitleWeight))
            .nextOption()
            .map(hit => { searchBasis = TmdbBasis.DirectorWalk; hit })
        }
      freshHit = hit
      hit.map(_.id.toString)
    }.map(_.toInt)
      // Whatever path resolved it (director-walk, year-scoped search, popularity),
      // pin a same-director adjacent-year TMDB duplicate to its lowest id so the
      // outcome can't drift with scrape/merge order. No-op when no director or no dup.
      .map(rid => if (rowDirectors.nonEmpty) collapseDirectorDuplicate(rid, rowDirectors) else rid)
    resolvedId
      // VETO: a title search can land on a film of an entirely different kind —
      // prod matched "Vivaldi i ja" to an 18-minute STABAT MATER concert short
      // while 46 venues advertised the 110-minute feature, and "Homo sapiens?" to
      // a 9-minute animated short. The row then carried that film's year, poster
      // and ratings, which is worse than carrying none. The cinemas' own minutes
      // are the check — they can't be derived from the resolution being tested —
      // and the band is wide enough that ordinary padding and rounding pass (see
      // `RuntimeCorroboration.plausible`). Only the SEARCH paths are vetoed; the
      // id-based fallbacks below are exact reverse lookups, not guesses.
      .filter { id =>
        val credible = RuntimeCorroboration.plausible(
          row.cinemaRuntimesMinutes, tmdb.fullDetails(id).flatMap(_.runtimeMinutes))
        if (!credible)
          logger.info(s"TMDB: '$title' (${year.getOrElse("?")}) → rejecting $id: its " +
            s"runtime is not credible against the cinemas' ${row.cinemaRuntimesMinutes.mkString("/")} min")
        credible
      }
      .map(id => (id, freshHit, searchBasis))
      // FALLBACK — exact reverse lookup by a known imdbId, only when the title /
      // director search above found nothing AND the row has no tmdbId yet. Such a
      // row can carry an imdbId from a NON-TMDB source (`OmdbBackfill` recovers one
      // by title+year search for exactly the films TMDB's fuzzy search misses), so
      // TMDB's `/find` returns the exact tmdbId that search couldn't. Gated on an
      // absent tmdbId so a row that's already TMDB-resolved never resurrects a
      // drifted resolution from its TMDB-DERIVED imdbId — a re-enrich whose search
      // now misses must leave that row untouched, not re-confirm the stale id.
      // After TMDB's own `/find` misses, cross to the other id-keyed source —
      // Letterboxd's page scrape — which can hold the imdbId→tmdbId mapping TMDB
      // itself lacks for an obscure title. Same `tmdbId.isEmpty` gate: never
      // resurrect a drifted resolution from a TMDB-derived imdbId. `None`
      // SearchResult — the tmdbId alone drives the by-id details fetch
      // downstream (as on a cache hit).
      .orElse {
        if (row.tmdbId.isEmpty) {
          def viaLetterboxd: Option[Int] =
            for {
              id       <- row.imdbId
              resolver <- letterboxdIdResolver
              tmdbId   <- resolver.resolveTmdbId(id)
            } yield tmdbId
          // Filmweb→Wikidata backstop — for a row with a Filmweb URL but no
          // imdbId to cross-walk. Filmweb enrichment is now un-gated for
          // tmdbId-less rows (see `RatingSources`), so a scraper-supplied /
          // Filmweb-discovered URL yields an entity id Wikidata maps to a TMDB id
          // (P5032 → P4947) — the route for the arthouse/repertoire long tail
          // TMDB's own fuzzy search misses. The chain crosses two external
          // cross-references either of which can be mis-linked (the stored URL can
          // point at the wrong edition), so accept the tmdbId ONLY when the
          // resolved TMDB film's OWN year equals the row's — hard equality, the
          // same-title-different-film guard. `/serial/` URLs (TV, never the
          // screened film) and rows without a year to check both abstain.
          def viaFilmwebWikidata: Option[Int] =
            for {
              client   <- wikidata
              url      <- row.filmwebUrl
              if !url.contains("/serial/")
              filmwebId <- WikidataClient.filmwebEntityId(url)
              ids      <- client.findIdsByFilmwebId(filmwebId)
              tmdbId   <- ids.tmdbId
              rowYear  <- effectiveYear
              if tmdb.fullDetails(tmdbId).flatMap(_.releaseYear).contains(rowYear)
            } yield tmdbId
          row.imdbId.flatMap(tmdb.findByImdbId).map(hit => (hit.id, Some(hit), TmdbBasis.ExternalId))
            .orElse(viaLetterboxd.map((_, Option.empty[TmdbClient.SearchResult], TmdbBasis.ExternalId)))
            .orElse(viaFilmwebWikidata.map((_, Option.empty[TmdbClient.SearchResult], TmdbBasis.ExternalId)))
        } else None
      }
  }

  /** When the cinema reports a director, drop title-search candidates whose
   *  TMDB credits don't include that director — they're probably a same-
   *  title-different-film hit. When no director is reported, pass the
   *  candidate through unchanged.
   *
   *  Director-name comparison (`MovieService.directorNameMatches`) is
   *  substring-OR-token-set: cinemas often comma-list a single name ("Asgeir
   *  Helgestad") while some films have multiple credited directors, so any TMDB
   *  director containing the cinema's name (or vice versa) counts — AND the two
   *  names match when they share the same word-token set in any order, so a
   *  cinema reporting "Yimou Zhang" (Western given-first) still verifies against
   *  TMDB's "Zhang Yimou" (native family-first). */
  private def verifyByDirector(
    candidate: Option[TmdbClient.SearchResult],
    director:  Option[String]
  ): Option[TmdbClient.SearchResult] =
    candidate.flatMap { hit =>
      director match {
        case None => Some(hit)   // no hint → can't verify, accept
        case Some(directory) =>
          val cinemaNames = directory.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSeq
          if (cinemaNames.isEmpty) Some(hit)
          else {
            val tmdbNames = tmdb.directorsFor(hit.id)
            val matches = tmdbNames.exists(t => cinemaNames.exists(c => MovieService.directorNameMatches(c, t, cache.normalizer)))
            if (matches) Some(hit) else None
          }
      }
    }

  /** Walk a cinema-reported director's TMDB filmography and pick the entry the
   *  cinema is actually showing. Needed when the title search lands on the wrong
   *  film (different decade, different language, popularity tie-break gone wrong).
   *
   *  Two ways to pick, in order:
   *    1. TITLE match — a credit whose (clean) title equals one of the cinema's
   *       search candidates. Cinemas routinely report a PRODUCTION year that
   *       drifts ±1 from TMDB's first-release year ("Mi Amor": cinema 2025, TMDB
   *       dates Nicloux's film 2026-05-06), so an exact-year walk would miss it.
   *       The title is the unambiguous signal — a director's two same-year films
   *       almost never share a title — so prefer it, and accept a ±1-year window
   *       to absorb the production/release drift without matching an unrelated
   *       decade's remake.
   *    2. EXACT year — for the case where the cinema's title doesn't match TMDB's
   *       spelling but the year pins the film. Requires a year, AND requires that
   *       year to be UNIQUE in the filmography: a director with two same-year
   *       credits (Andrew Stanton's "In the Blink of an Eye" + "Toy Story 5", both
   *       2026) can't be disambiguated by year, so the walk abstains rather than
   *       guess the first — better no ratings than another film's ratings. */
  private def directorWalk(
    director:         Option[String],
    year:             Option[Int],
    candidates:       Seq[String] = Nil,
    cinemaRuntimes:   Seq[Int] = Nil,
    cinemaCast:       Seq[String] = Nil,
    cinemaCandidates: Seq[String] = Nil,
    cinemaTitleWeight: Map[String, Int] = Map.empty
  ): Option[TmdbClient.SearchResult] = {
    director.flatMap { directory =>
      // Try each person the name could mean, in turn — TMDB's top hit is wrong
      // often enough (a credit-less duplicate stub, or an alias another director
      // lists) that trusting it costs the whole resolution now the walk is the
      // only resolver. A person whose filmography doesn't contain the film simply
      // yields nothing here, which is exactly the signal to try the next one.
      tmdb.findPersonCandidates(directory.split(",").head.trim).iterator.zipWithIndex.flatMap { case (personId, candidateIndex) =>
        // Directing credits first — the common case, unchanged. A cinema that
        // printed the WRITER instead ("Drzewo magii" is directed by Ben Gregor and
        // written by Simon Farnaby, and cinemas print either) would otherwise walk
        // an empty filmography and resolve to nothing, so fall back to what this
        // person WROTE. Same response, same round-trip — TMDB returns the whole
        // crew — and every guard below is unchanged, so widening where the film may
        // be FOUND does not widen what counts as a match.
        val directed = tmdb.personDirectorCredits(personId)
        val credits  = if (directed.nonEmpty) directed else tmdb.personWriterCredits(personId)
        def sanitizedSet(titles: Seq[String]): Set[String] =
          titles.iterator.map(cache.normalizer.sanitize).filter(_.nonEmpty).toSet
        val wanted       = sanitizedSet(candidates)
        val wantedCinema = sanitizedSet(cinemaCandidates)
        def titleOf(f: TmdbClient.SearchResult): Set[String] =
          (Seq(f.title) ++ f.originalTitle.toSeq).map(cache.normalizer.sanitize).filter(_.nonEmpty).toSet
        // Fuzzy title match, scoped to THIS director's filmography (a small, trusted
        // set): a cinema's spelling of a foreign title drifts from TMDB's ("Guru" vs
        // Yann Gozlan's "Gourou"), so an exact match misses and the year-only `byYear`
        // below then pins whichever of the director's films sits at the row's
        // (cinema-disagreed, merge-order-dependent) year — "Dalloway" 2025 vs "Gourou"
        // 2026, the SAME-director cross-film flip. A tight edit-distance match (≤2 and
        // ≤1/3 of the longer title) ties "guru"→"gourou" but never "guru"→"dalloway".
        def titleClose(f: TmdbClient.SearchResult, want: Set[String] = wanted): Boolean =
          titleOf(f).exists(t => want.exists { w =>
            val d = TitleCorroboration.editDistance(w, t)
            d <= 2 && d * 3 <= math.max(w.length, t.length)
          })
        // Title match first (±1-year-tolerant); fall back to an exact-year match,
        // but ONLY when that year is unambiguous in the filmography. A director
        // with two same-year credits (Andrew Stanton: "In the Blink of an Eye"
        // and "Toy Story 5", both 2026) can't be told apart by year alone — the
        // old `.find` returned whichever came first, binding a Ukrainian-dubbed
        // "Toy Story 5" listing to "In the Blink of an Eye"'s ratings. Refuse.
        // Pick the LOWEST tmdbId among title-matching credits, not the first in
        // filmography order: a director's film duplicated in TMDB under adjacent
        // years + ids (Yann Gozlan's "Gourou" exists as BOTH 1259983/2026 and
        // 1315702/2025 — the SAME film, two entries) both match here, so `.find`
        // returned whichever the credits happened to list first, flipping the
        // resolved id with scrape/merge order. A genuinely-different same-title
        // remake is still kept apart by the ±1-year window; only a true adjacent-year
        // duplicate ties, and lowest-id breaks that tie deterministically
        // (StagingOrderDeterminismSpec).
        // An EXACT title outranks a merely close one. `titleClose` is fuzzy on
        // purpose, and a SEQUEL sits one character from the film it follows
        // ("Diabeł ubiera się u Prady 2" vs "…u Prady", "Piep*zyć Mickiewicza 3"
        // vs "…Mickiewicza"), so both survive the filter and the lowest-id
        // tie-break — written to collapse a TMDB duplicate of ONE film — hands the
        // row the OLDER one. A year normally hides it, but the year arrives with
        // whichever cinema publishes it, so rows scraped before that resolved
        // year-less onto the wrong film and the corpus settled differently per
        // arrival order (`StagingOrderDeterminismSpec`). Lowest-id still breaks
        // ties among equally exact credits, which is the case it was written for.
        val eligible = if (wanted.isEmpty) Seq.empty else credits.filter { f =>
          titleClose(f) && year.forall(y => f.releaseYear.forall(fy => math.abs(fy - y) <= 1))
        }
        // A CINEMA-reported title outranks a title this row's own earlier resolution
        // wrote. The candidate set deliberately includes the derived Tmdb/Imdb/Filmweb
        // slots' `originalTitle` — that is how a film TMDB doesn't index under its
        // Polish title resolves once Filmweb supplies the original — but as one flat
        // set they competed on equal terms and the lowest-id tie-break decided.
        // "Mistyczka" (Jan Sobierajski, 2026) had drifted onto his OTHER 2026 film
        // "Maryja. Matka papieża"; once TMDB listed the real film both credits matched
        // a candidate exactly and 1646379 < 1731866, so the wrong film re-won every
        // cycle (`MovieServiceTmdbHintsSpec`). Four tiers, cinema-exact first, each
        // still tie-broken by lowest id; derived titles resolve only when no cinema
        // title reaches a credit, which is the case they were added for.
        // Within a tier, the credit the MOST venues name wins; lowest id still breaks
        // a genuine tie (the TMDB adjacent-year duplicate this was written for, where
        // both entries carry the same title and so the same weight).
        def venuesNaming(f: TmdbClient.SearchResult): Int =
          titleOf(f).iterator.map(cinemaTitleWeight.getOrElse(_, 0)).maxOption.getOrElse(0)
        def bestOf(tier: Seq[TmdbClient.SearchResult]): Option[TmdbClient.SearchResult] =
          tier.sortBy(f => (-venuesNaming(f), f.id)).headOption
        val byTitle = Seq(
          eligible.filter(f => titleOf(f).exists(wantedCinema.contains)),
          eligible.filter(f => titleClose(f, wantedCinema)),
          eligible.filter(f => titleOf(f).exists(wanted.contains)),
          eligible
        ).find(_.nonEmpty).flatMap(bestOf)
        // The year-pinned branch below exists for films whose Polish title has no
        // TMDB entry at all: pl-PL credits fall back to the ORIGINAL title, so
        // "Giulietta i duchy" faces "Giulietta degli spiriti" and `titleClose`
        // cannot bridge that. But pinning on the year ALONE asserts nothing about
        // the film, and a single cinema publishing a wrong director or year then
        // resolves confidently to a stranger — "Głos Hind Rajab" became Łukasz
        // Kowalski's "Lombard", "Zawieście czerwone latarnie" a different Zhang
        // Yimou film.
        //
        // So the year picks the candidate and the TITLE still has to corroborate
        // it, just loosely enough to survive translation: the two must share a
        // distinctive word. Titles of the same film keep a proper noun across
        // languages ("Giulietta", "Munch", "Mavka"); unrelated films share nothing.
        // `TitleCorroboration` owns which words count and how they are folded —
        // Filmweb's director+year override answers the same question.
        def corroboratedByTitle(f: TmdbClient.SearchResult): Boolean =
          TitleCorroboration.sharesDistinctiveToken(
            candidates, Seq(f.title) ++ f.originalTitle.toSeq, cache.normalizer.sanitize)

        // When the title is FULLY translated it keeps nothing to share — "Trener
        // Tenisa" against "Il Maestro", "Kochanie" against "Gioia mia" — and the
        // year-pinned branch is the only way those resolve. Runtime and cast are
        // evidence the title cannot give and translation cannot touch, and both
        // arrive in the ONE `fullDetails` call, fetched only when the cheap title
        // check has already failed.
        //
        // Measured over the corpus they separate the cases cleanly: the genuine
        // translations agree on runtime to within a minute or two (125/125, 98/97,
        // 93/95) while the bogus matches are nowhere near (89 against Lombard's 78,
        // 142 against Codename Cougar's 76). Cast is the stronger signal where a
        // cinema publishes one — an actor's name is a proper noun in every
        // language — though the cinemas behind these particular rows publish none.
        // ANY reported runtime agreeing is enough, rather than a single chosen one:
        // cinemas disagree by a minute or two, and picking "the" runtime would make
        // the answer depend on which cinema had arrived (`StagingOrderDeterminismSpec`).
        def corroboratedByFacts(f: TmdbClient.SearchResult): Boolean =
          (cinemaRuntimes.nonEmpty || cinemaCast.nonEmpty) && {
            val full = tmdb.fullDetails(f.id)
            val runtimeAgrees = full.flatMap(_.runtimeMinutes).exists(actual =>
              cinemaRuntimes.exists(reported => math.abs(actual - reported) <= MovieService.RuntimeAgreementMinutes))
            val castAgrees = full.exists { d =>
              val theirs = d.cast.map(cache.normalizer.sanitize).filter(_.nonEmpty).toSet
              cinemaCast.map(cache.normalizer.sanitize).exists(n => n.nonEmpty && theirs.contains(n))
            }
            runtimeAgrees || castAgrees
          }
        val byYear = year.flatMap(y => credits.filter(_.releaseYear.contains(y)) match {
          case Seq(only) if corroboratedByTitle(only) || corroboratedByFacts(only) =>
            // Collapse a TMDB adjacent-year DUPLICATE of one film: if the year-pinned
            // credit shares its title with a credit ±1 year off (the same film entered
            // twice — "Gourou" as both 2025/1315702 and 2026/1259983), they're ONE
            // film; pick the lowest id so the merge-order-dependent KEY year can't pin
            // whichever duplicate sits at it (StagingOrderDeterminismSpec).
            Some(credits.filter(f => titleOf(f) == titleOf(only) &&
              f.releaseYear.exists(fy => math.abs(fy - y) <= 1)).minBy(_.id))
          case _         => None         // 0 or >1 at the exact year, or no title corroboration → don't guess
        })
        byTitle.orElse(byYear).map { film =>
          logger.info(s"Director-walk: '$directory' (person $personId) year=${year.getOrElse("?")} → tmdbId=${film.id} '${film.originalTitle.getOrElse(film.title)}'")
          film
        }
      }.nextOption()
    }
  }

  /** Collapse a TMDB adjacent-year DUPLICATE of ONE film to its lowest id. TMDB
   *  occasionally lists a single film under two ids a year apart (Yann Gozlan's
   *  "Gourou" as 1315702/2025 AND 1259983/2026 — same title, same director). When
   *  the row reports the director, those entries are provably one film (shared
   *  director + a ±1-year shared title), so the resolved id is pinned to the lowest
   *  — independent of which duplicate the row's (merge-order-dependent) key year or
   *  a popularity tie-break happened to land on. A genuinely-different same-title
   *  remake by the same director is kept apart by needing the SAME title AND ±1
   *  year (a remake is years apart); this only fuses true duplicates. Reuses the
   *  director credits `directorWalk` already fetched (cached), so no extra calls.
   *  Order-independent — see `StagingOrderDeterminismSpec`. */
  private def collapseDirectorDuplicate(id: Int, directors: Seq[String]): Int = {
    val credits = directors.iterator
      .flatMap(d => tmdb.findPerson(d.split(",").head.trim).iterator.flatMap(tmdb.personDirectorCredits))
      .toSeq.distinctBy(_.id)
    def titles(f: TmdbClient.SearchResult): Set[String] =
      (Seq(f.title) ++ f.originalTitle.toSeq).map(cache.normalizer.sanitize).filter(_.nonEmpty).toSet
    credits.find(_.id == id).fold(id) { resolved =>
      // `minOption.getOrElse(id)`, not `.min`: the adjacency filter is EMPTY
      // whenever the resolved credit carries no TMDB release year (its own
      // `resolved.releaseYear.exists(…)` is false, so nothing — not even itself —
      // matches), and a bare `.min` on that empty Seq threw
      // `UnsupportedOperationException: empty.min`, which the resolve mistook for a
      // transient failure and retried forever. No year ⇒ no provable duplicate ⇒
      // keep the resolved id.
      credits.filter(f => titles(f) == titles(resolved) &&
        f.releaseYear.exists(ry => resolved.releaseYear.exists(ay => math.abs(ry - ay) <= 1)))
        .map(_.id).minOption.getOrElse(id)
    }
  }

}

object MovieService {
  /** How far a cinema's published runtime may sit from TMDB's and still count as
   *  the same film. Two minutes is what the corpus shows genuine pairs differing
   *  by (rounding, and whether the credits roll is counted); the wrong matches it
   *  has to exclude are out by tens of minutes, so there is no need to stretch it. */
  private[movies] val RuntimeAgreementMinutes = 2

  /** What a completed resolution reports back to its callers: the row as re-read
   *  from the cache (`cached`), backfilled from the record we just persisted
   *  (`resolved`) for anything the re-read lacks.
   *
   *  The re-read is what carries the strays `settleResolved` folded in, so it stays
   *  the canonical side. But it must never come back LESS resolved than what was
   *  written: everything downstream keys on the ids — `publishTmdbOutcome` decides
   *  on `imdbId` whether to publish `ImdbIdMissing`, and the forced rating refresh
   *  derives BOTH its freshness dedup key (`…|tmdb:<id>`) and its per-source
   *  eligibility from `tmdbId`/`imdbId`. An id-less re-read therefore invalidates the
   *  wrong stamps and finds no source eligible, so a forced re-enrich strips the
   *  row's scores and never re-fetches them (prod, "Odyseja", 2026-07-19 — the
   *  re-read came back id-less for reasons not yet reproduced, which is exactly why
   *  this is an invariant here rather than a fix at the presumed cause).
   *
   *  `union` keeps `cached`'s fields and fills only its gaps, so this is a no-op on
   *  the healthy path. */
  private[movies] def resolvedView(cached: Option[MovieRecord], resolved: MovieRecord): MovieRecord =
    cached.fold(resolved)(MovieRecordMerge.union(_, resolved))

  // Stable documentId key for the cache + Mongo `_id`. Delegates to
  // `TitleNormalizer.sanitize`, which applies Arabic→Roman, strips display-
  // only decoration (anniversary/Cykl/wersja), folds " & " → " i " and the
  // "Gwiezdne Wojny:" prefix, and finally collapses every non-alphanumeric
  // char so punctuation/whitespace differences ("Top Gun Maverick" vs
  // "Top Gun: Maverick") share a key.
  //
  // Corpus-independent — the same title always produces the same key, so
  // cache lookups + Mongo upserts are stable across refresh ticks regardless
  // of which other films happen to be in the cache at the moment.

  /** Does a cinema-reported director name refer to the same person as a
   *  TMDB-credited one? Two independent signals, EITHER suffices:
   *   1. Substring on the collapsed (`normalize`d) forms — a single surname
   *      vs the full name, or one of a film's several credited directors.
   *   2. Word-token-set containment — `nameTokens` splits each name on word
   *      boundaries (BEFORE `sanitize` collapses whitespace away), so the same
   *      name in a different order matches: a cinema reporting a Chinese /
   *      Hungarian / Korean name given-first ("Yimou Zhang") verifies against
   *      TMDB's native family-first credit ("Zhang Yimou"). Order-sensitive
   *      substring alone rejected that correct hit, stranding the row as a
   *      no-match that only recovered if a SIBLING cinema happened to report the
   *      canonical order — an arrival-order dependence. Set containment (not
   *      strict equality) keeps signal 1's partial-name tolerance. */
  def directorNameMatches(cinemaName: String, tmdbName: String, normalizer: TitleNormalizer): Boolean = {
    val (a, b) = (normalizer.sanitize(cinemaName), normalizer.sanitize(tmdbName))
    if (a.nonEmpty && b.nonEmpty && (a.contains(b) || b.contains(a))) true
    else {
      val (ca, ct) = (nameTokens(cinemaName, normalizer), nameTokens(tmdbName, normalizer))
      ca.nonEmpty && ct.nonEmpty && (ca.subsetOf(ct) || ct.subsetOf(ca))
    }
  }

  /** The set of word tokens in a person name, each `sanitize`d (diacritics
   *  folded, lowercased), split on any non-letter/digit FIRST so the word
   *  boundaries survive — unlike `sanitize`, which collapses the whole string
   *  to one alphanumeric token. "Zhang Yimou" → {"zhang", "yimou"}. */
  private[movies] def nameTokens(name: String, normalizer: TitleNormalizer): Set[String] =
    name.split("[^\\p{L}\\p{N}]+").iterator.map(normalizer.sanitize).filter(_.nonEmpty).toSet

  /** Aggressive stripping for external-API queries: the anniversary / restored /
   *  wersja / Cykl / slash decoration PLUS the accessibility-programme decoration
   *  (Kino bez barier, Pokaz sensorycznie, "(AD + CC + PJM)", "+ <event>") so the
   *  TMDB/Filmweb/IMDb search hits the base film. This does NOT affect identity —
   *  a decoration / programme edition keys by its own form and stays a separate
   *  card (see `TitleNormalizer.sanitize` and `MovieCache.keyOf`); it just
   *  resolves to the base film's ratings. */


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
      // A programme banner is joined with a DASH as often as a pipe or a colon
      // ("Filmoczule Dla Edukacji … – 500 mil", "Ladies Night - Narodziny
      // gwiazdy"), and the film's own title is the part after it. Hyphen, en dash
      // and em dash all appear; the surrounding spaces are what mark it as a
      // separator rather than a hyphenated word ("Spider-Man" is untouched).
      // Purely ADDITIVE — the undivided title stays a candidate — and a candidate
      // only ever becomes a resolution by matching, so an over-eager split on a
      // title that genuinely contains " - " costs nothing.
      val dashParts       = if (t.matches(""".*\s[-–—]\s.*""")) t.split("""\s+[-–—]\s+""").toIndexedSeq else Nil
      val noTrailingParen = t.replaceAll("""\s*\([^)]*\)\s*$""", "").trim
      (Seq(t) ++ pipeParts ++ dashParts :+ noTrailingParen)
    }
    (Seq(title) ++ originalTitle.toSeq ++ extraTitles)
      .map(_.trim).filter(_.nonEmpty).distinct
      .flatMap(deDecorate)
      .map(_.trim).filter(_.nonEmpty).distinct
  }
}
