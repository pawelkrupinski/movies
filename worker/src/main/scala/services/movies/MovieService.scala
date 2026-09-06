package services.movies

import clients.TmdbClient
import play.api.Logging
import services.Drainable
import services.cinemas.CountryNames
import services.enrichment.{LetterboxdIdResolver, WikidataClient}
import services.events.{DomainEvent, EventBus, ImdbIdMissing, MovieDetailsComplete}
import services.freshness.{FreshnessKind, FreshnessStore, InMemoryFreshnessStore}
import services.resolution.{Candidate, Contradiction, FilmEvidence, ResolutionCache, TmdbAttempt, TmdbBasis, Verdict}
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
  staging:              services.staging.StagingRepository = services.staging.StagingRepository.empty,
  // Where the splitter reports the slots it re-diverted (`kinowo_worker_splits_total`).
  // Production wires `WorkerTaskMetrics`; unit specs and scripts leave it silent.
  splitMetrics:         SplitMetrics = SplitMetrics.noop,
  // Stamps a no-match `TmdbAttempt` and judges whether a remembered one still
  // stands (`TmdbAttempt.RetryAfter`). Injectable so a spec can age a miss.
  clock:                java.time.Clock = java.time.Clock.systemUTC()
) extends Drainable with Logging {
  // Fold titles with the rules the corpus was keyed under, not a process default.
  private val normalizer: services.movies.TitleNormalizer = cache.normalizer

  // How a needed single-movie TMDB resolution is dispatched (see the `dispatcher`
  // ctor param). The inline default dedups by the row's `CacheKey` so it doesn't
  // run the same key twice concurrently (production dedups via the task queue's
  // per-dedupKey idempotency instead); the IMDb stage doesn't dedup — it's
  // idempotent and cheap. The `resolveTmdbOnce` reference is a forward reference
  // from a closure, only invoked once a dispatch fires.
  // The search half of the TMDB stage; this class keeps the write half.
  private val candidateSearch = new TmdbCandidateSearch(tmdb, normalizer, tmdbIdCache, letterboxdIdResolver, wikidata)

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

  private lazy val mixedFilmSplitter = new MixedFilmSplitter(cache, staging, splitMetrics)

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
        // call). When THIS event carries a director, RE-VERIFY with the one
        // `Verdict` the sweep uses: a credited name that matches nobody on the
        // current film's crew is a contradiction → re-resolve (the director walk
        // will land the right film). Anything else — agreement, or a crew TMDB
        // could not read — keeps it; an unanswered question is not evidence, and
        // the keep is what makes the stage idempotent so a correctly-resolved row
        // never churns TMDB on every director-bearing change.
        if (director.isEmpty) false
        else {
          val evidence = existing.fold(FilmEvidence.empty)(_.evidence)
            .withDirectors(director.toSeq.flatMap(_.split(",")))
          if (evidence.directors.isEmpty) false
          else Verdict.of(evidence, Candidate(currentId, crew = tmdb.directorsFor(currentId).toSeq)) match {
            case Verdict.Reject(Contradiction.Director) =>
              logger.info(s"TMDB re-resolve: '${key.cleanTitle}' (${key.year.getOrElse("?")}) tmdbId=$currentId " +
                          s"no longer matches the reported director(s) [${evidence.directors.mkString(",")}] — re-resolving.")
              true
            case _ => false
          }
        }
      case None =>
        // A remembered miss stands only while nothing it was reached on has changed:
        // the row's `tmdbAttempt` fingerprints the cinemas' evidence and the derived
        // search terms of the search that found nothing, and this event's hints are
        // folded in before comparing. A director a later cinema brings, a Filmweb-
        // supplied original title, a new venue's spelling — each changes the
        // fingerprint and re-opens the row at once instead of waiting out a TTL (the
        // Kurozając class of regression: a row whose first-scraping cinema reports
        // no director stayed trapped for the full 24h). The same inputs within
        // `TmdbAttempt.RetryAfter` are not searched again.
        if (existing.flatMap(_.tmdbAttempt).exists(_.covers(attemptFingerprint(existing, originalTitle, director), clock.instant()))) false
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

  /** What a TMDB search for this row consumes — the cinemas' evidence with the
   *  event's hints folded in, plus the derived search terms — as the fingerprint a
   *  no-match is recorded under and later compared against. ONE definition for both
   *  sides, or a miss could be recorded under one fingerprint and checked under
   *  another. */
  private def attemptFingerprint(row: Option[MovieRecord], originalTitle: Option[String], director: Option[String]): String =
    TmdbAttempt.fingerprint(
      row.fold(FilmEvidence.empty)(_.evidence)
        .withDirectors(director.toSeq.flatMap(_.split(",")))
        .withOriginalTitle(originalTitle),
      row.toSeq.flatMap(_.resolverOriginalTitles))

  /** The no-match record of what THIS search consumed, stamped now. */
  private def attemptFor(row: MovieRecord, originalTitle: Option[String], director: Option[String]): TmdbAttempt =
    TmdbAttempt(attemptFingerprint(Some(row), originalTitle, director), clock.instant())

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
        cache.rekey(live, newKey, _.scrapedOnly, services.movies.RekeyReason.ForcedReset)
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
   *    - DEFINITIVE MISS: persist a `tmdbAttempt` so the row is `tmdbConcluded`
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
        // Conclude as a definitive miss — recording what the search consumed, so the
        // next look knows whether anything changed — AND fold a stranded
        // yearless+idless sibling onto the now-concluded row in one write (same
        // rationale as the hit path), instead of leaving it held back from the read
        // model until a later settle.
        val liveKey = cache.canonicalKeyFor(key).getOrElse(key)
        def missed(record: MovieRecord): MovieRecord =
          record.copy(tmdbAttempt = Some(attemptFor(record, origHint, directoryHint)))
        cache.get(liveKey) match {
          case Some(record) => cache.settleResolved(liveKey, missed(record))
          case None      => cache.putIfPresent(liveKey, missed)
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
   *    - `Some(existing.copy(tmdbAttempt = Some(…)))` on a DEFINITIVE MISS;
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
        Some(existing.copy(tmdbAttempt = Some(attemptFor(existing, origHint, directoryHint))))
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
        // a recorded no-match `tmdbAttempt`, poisoning a film that is perfectly fine.
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
  ): Option[(Int, Option[TmdbClient.SearchResult], TmdbClient.ExternalIds, Option[TmdbClient.FullDetails], Option[TmdbBasis])] =
    candidateSearch.resolve(cleanTitle, year, row, originalTitleHint, directorHint).flatMap { case (tmdbId, hit, basis) =>
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
    // None when the resolution came off the id cache, so the conclusion's real basis
    // is unknown here; the row then KEEPS the basis it already recorded rather than
    // being downgraded to the weakest one. See `resolveTmdbId`.
    basis:       Option[TmdbBasis]
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
      tmdbBasis         = basis.map(_.toString).orElse(existing.tmdbBasis),
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
   *  event fired from the TMDB stage at first resolution. Drops each row's
   *  remembered miss so previously-failed `(title, year)` lookups get one fresh
   *  shot. This bulk form backs the operator `RefreshAllTmdb` button; the scheduled,
   *  phase-spread re-try is owned by [[services.tasks.UnresolvedTmdbReaper]]
   *  (via [[retryResolve]]) so the backlog drains as a trickle, not a burst. */
  def retryUnresolvedTmdb(): Unit = {
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
    logger.info(s"TMDB retry: re-dispatching ${targets.size} row(s) with missing tmdbId, their remembered misses dropped.")
    targets.foreach { case (k, e) =>
      cache.putIfPresent(k, _.copy(tmdbAttempt = None))
      dispatchWithHints(k, e)
    }
  }

  /** Re-attempt ONE still-unresolved row's TMDB resolution, dropping just that
   *  row's remembered miss first (the scoped form of [[retryUnresolvedTmdb]]). Driven by
   *  [[services.tasks.UnresolvedTmdbReaper]]'s phase-spread tick so the
   *  unresolved backlog re-tries as a flat trickle instead of a boot/period
   *  burst. No-op once the row has resolved or is awaiting detail (its detail
   *  completing re-triggers TMDB via `MovieDetailsComplete`). */
  /** [[retryResolve]] addressed by `(title, year)`, for a caller outside `services`
   *  — `CacheKey` is `private[services]`, so the fixture harness cannot name a row
   *  any other way. Without it the only reachable re-resolve was the operator-scale
   *  [[retryUnresolvedTmdb]], whose corpus-wide miss-dropping un-concludes every
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
      cache.putIfPresent(key, _.copy(tmdbAttempt = None))
      dispatchWithHints(key, e)
    }

  /** Re-resolve a row that ALREADY has a `tmdbId`, so its `Tmdb` slot is re-fetched
   *  rather than left frozen at whatever the first resolve stored. The stale-language
   *  sweep ([[services.tasks.UnresolvedTmdbReaper]]) drives this: `fullDetails` is
   *  fetched only at resolve time, so a row enriched before its deployment learned
   *  its own language keeps Polish text until something forces the re-fetch. */
  def forceResolve(key: CacheKey): Unit =
    cache.get(key).foreach(e => dispatchWithHints(key, e, force = true))

  /** Give a RESOLVED row back the `Tmdb` slot it has lost, by id — no search.
   *
   *  A row that carries a `tmdbId` but no `Tmdb` slot renders without TMDB's poster,
   *  synopsis, genres and runtime, and nothing re-fetches them: resolution is a
   *  one-shot, and every other sweep asks "is the id wrong?", not "is the slot
   *  there?". Prod, 2026-09-06: 483 such rows across PL/UK/DE, every one resolved
   *  inside the 2026-07-27 → 08-06 window the slot migration ran in, none since —
   *  a bounded artefact, but one nothing was going to heal. Re-SEARCHING would be
   *  wrong here (a row with screenings can re-resolve to a stranger and be pruned;
   *  see `docs/misresolution-sweep.md`); the row already knows which film it is, so
   *  the details are fetched by that id and written through the same builder a
   *  resolution uses, ratings and cinemas carried forward untouched.
   *
   *  Returns true when a slot was written; false when the row needs no refill or TMDB
   *  could not answer (the reaper sees it again next period). */
  def refillTmdbSlot(key: CacheKey): Boolean =
    cache.get(key).filter(e => e.tmdbId.isDefined && !e.data.contains(Tmdb)).exists { e =>
      val tmdbId = e.tmdbId.get
      tmdb.fullDetails(tmdbId).exists { details =>
        val ids = Try(tmdb.externalIds(tmdbId)).getOrElse(TmdbClient.ExternalIds(e.imdbId, e.wikidataId))
        cache.putIfPresent(key, cur => buildResolvedRecord(tmdbId, hit = None, ids, Some(details), cur, basis = None))
      }
    }

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
    (e.evidence.originalTitle, e.evidence.directorHint)


}

object MovieService {

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

  /** Aggressive stripping for external-API queries: the anniversary / restored /
   *  wersja / Cykl / slash decoration PLUS the accessibility-programme decoration
   *  (Kino bez barier, Pokaz sensorycznie, "(AD + CC + PJM)", "+ <event>") so the
   *  TMDB/Filmweb/IMDb search hits the base film. This does NOT affect identity —
   *  a decoration / programme edition keys by its own form and stays a separate
   *  card (see `TitleNormalizer.sanitize` and `MovieCache.keyOf`); it just
   *  resolves to the base film's ratings. */


}
