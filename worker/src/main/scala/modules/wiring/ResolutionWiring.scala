package modules.wiring

import modules.WorkerWiring
import services.enrichment.{CinemetaClient, ImdbIdResolver}
import services.movies.{MovieService, QueueResolveDispatcher}
import services.resolution.{MongoResolutionStore, ResolutionCache, ResolutionOutcome, UnresolvedPolicy, WriteThroughResolutionCache}
import services.tasks.{CrewConfirmation, SettleReaper, UnresolvedTmdbReaper}
import tools.Env

import scala.concurrent.duration.{DurationLong, FiniteDuration}

/** Identity resolution: the TMDB stage (`MovieService`), IMDb-id recovery, the
 *  per-source resolution caches a forced re-enrich clears, and the reapers that
 *  re-try the unresolved backlog and re-assert one-row-per-film. */
trait ResolutionWiring { self: WorkerWiring =>

  lazy val imdbIdCache: ResolutionCache = resolutionCache("resolve_imdb")
  // Named (not inline) so the forced re-enrich can reach every one of them to
  // forget a film's memoised resolutions — and so each is ONE instance rather
  // than a fresh Caffeine per call site.
  //
  // The three rating-LINK caches remember empty answers as well as hits: "this
  // site has no page for this film" is the common outcome and a stable one, and
  // re-deriving it costs a full probe ladder every four hours. The two IDENTITY
  // caches (TMDB, IMDb) keep retrying, because there an empty answer is expected
  // to become a real one on its own once the upstream indexes the film.
  lazy val rtLinkCache: ResolutionCache      = resolutionCache("resolve_rt",      UnresolvedPolicy.Remember)
  lazy val mcLinkCache: ResolutionCache      = resolutionCache("resolve_mc",      UnresolvedPolicy.Remember)
  lazy val filmwebLinkCache: ResolutionCache = resolutionCache("resolve_filmweb", UnresolvedPolicy.Remember)
  /** Every per-source resolution cache — what a forced re-enrich clears. */
  lazy val resolutionCaches: Seq[ResolutionCache] =
    Seq(tmdbIdCache, imdbIdCache, rtLinkCache, mcLinkCache, filmwebLinkCache)
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient,
    backgroundBudget.executionContext("imdb-id-resolver"), imdbIdCache = imdbIdCache,
    wikidata = Some(wikidataClient),
    letterboxdIdResolver = Some(letterboxdIdResolver),
    // Same OMDB_API_KEY gate as `omdbBackfill` — the OMDb rung is inert when unset.
    omdb = Env.get("OMDB_API_KEY").map(_ => omdbClient),
    // Cinemeta needs no key — always wired as the final free rung.
    cinemeta = Some(new CinemetaClient(enrichmentFetch)))

  // Single-movie TMDB resolution is dispatched as a `ResolveTmdb` worker task:
  // drained by the TaskWorker, retried (`Reschedule`) + deduped by the queue,
  // and shown with a live queue place on `/debug`. `taskQueue` is a lazy val;
  // the closure defers reading it, so there's no init cycle.
  // Hint-keyed resolution caches (Caffeine + per-source Mongo collection, 24h
  // TTL): the same hints resolve once instead of hitting the upstream each cycle.
  // One factory so the test wiring can swap in a passthrough (the fixture harness
  // proves the pipeline is a pure function of the corpus, with no shared cache).
  protected def resolutionCache(
    collection: String,
    unresolved: UnresolvedPolicy = UnresolvedPolicy.Retry
  ): ResolutionCache =
    new WriteThroughResolutionCache(
      new MongoResolutionStore(mongoConnection.database, collection, normalizer = titleNormalizer),
      // Labels the counter with the source this collection serves (`resolve_rt` →
      // `rt`), so `kinowo_worker_resolution_total` breaks the saving down per
      // rating source rather than lumping all five together.
      workerMetrics.resolutionMetrics.recorderFor(country.code, ResolutionOutcome.sourceOf(collection)),
      unresolved)
  lazy val tmdbIdCache: ResolutionCache = resolutionCache("resolve_tmdb")

  /** How a single-movie TMDB resolution is dispatched. Production queues it; the
   *  fixture harness runs it inline, because it never starts a `TaskWorker`.
   *
   *  A SEAM, so that is the only thing a harness has to change. It used to rebuild
   *  `MovieService` wholesale to swap this, and rebuilding it positionally silently
   *  dropped whatever the rebuild forgot — `letterboxdIdResolver` and `wikidata` (two
   *  rungs of the resolution ladder), `enqueueNewcomerRatings`, `freshness`,
   *  `tmdbIdCache`, `forgetResolutions`. Each was invisible until something measured
   *  it. Overriding one `def` cannot drop the rest. */
  protected def resolveDispatcher: Option[services.movies.ResolveDispatcher] = Some(new QueueResolveDispatcher(taskQueue))

  lazy val movieService: MovieService = new MovieService(
    movieCache, eventBus, tmdbClient,
    dispatcher = resolveDispatcher,
    splitMetrics = taskMetrics,
    clock = clock,
    tmdbIdCache = tmdbIdCache,
    // SAME store the rating handlers read, so the resolved → first-rating delay
    // (stamped here on resolution, observed there on first attempt) correlates.
    freshness = freshnessStore,
    // Kick a freshly-promoted newcomer's ratings the instant it folds — the SAME
    // enqueuer the EnrichmentReaper walks the corpus with, so a newcomer and a
    // reaper sweep share the eligibility + due gate. A fold is a trickle, so this
    // doesn't recreate the old TmdbResolved corpus-wide burst.
    enqueueNewcomerRatings = (key, record) => { ratingEnqueuer.enqueueDueFor(key, record, java.time.Instant.now()); () },
    // A (re)resolve forces every rating source due again — heals the scores a forced
    // re-resolve strips, which the cadence would otherwise keep from re-fetching.
    forceRatingRefresh = (key, record) => { ratingEnqueuer.enqueueDueFor(key, record, java.time.Instant.now(), force = true); () },
    forgetResolutions = cleanTitle => resolutionCaches.foreach(_.forget(cleanTitle)),
    letterboxdIdResolver = Some(letterboxdIdResolver),
    // Same WikidataClient ImdbIdResolver uses — lets a tmdbId-less row with a
    // Filmweb URL resolve via P5032 → P4947 (corroborated) once Filmweb is un-gated.
    wikidata = Some(wikidataClient),
    // Where `settle` re-diverts the cinemas of a row's second film.
    staging = stagingRepository)

  // The whole-corpus settle on its OWN periodic tick, decoupled from the cache
  // hydrate. The settle used to ride `MovieCache.rehydrate`'s backstop reload
  // (KINOWO_CACHE_REHYDRATE_SECONDS=1800s), but a reload re-derives every key as
  // `displayTitle`, so settling right after it re-keyed the spelling-variant rows —
  // the per-deploy flap. Now the load is a pure read and this reaper re-asserts the
  // one-row-per-film invariant once per the SAME 30-min window (cluster-claimed).
  def settleIntervalSeconds: FiniteDuration =
    Env.positiveLong("KINOWO_SETTLE_INTERVAL_SECONDS", SettleReaper.DefaultInterval.toSeconds).seconds
  lazy val settleReaper = new SettleReaper(() => movieService.settle(),
    interval = settleIntervalSeconds, runStore = scheduledRunStore)

  // Re-tries unresolved-TMDB rows once per 24h, phase-spread across the period —
  // the queue-era replacement for MovieService's old daily, all-at-once
  // `retryUnresolvedTmdb` scheduler (it re-dispatched the whole unresolved
  // backlog 10s after boot, the boot ResolveTmdb burst that pinned the
  // shared-CPU credit). `retryResolve` clears each due row's negative + dispatches
  // its ResolveTmdb. Cap bounds a clock-jump/cold burst the same way the rating
  // reaper does — the leftover stays due and re-tries next period.
  def maxTmdbRetryEnqueuePerTick: Int = Env.positiveLong("KINOWO_TMDB_RETRY_MAX_ENQUEUE_PER_TICK", 100L).toInt
  lazy val unresolvedTmdbReaper = new UnresolvedTmdbReaper(movieCache, movieService.retryResolve,
    // `forceResolve` + `country` drive the stale-language sweep: a row whose Tmdb slot
    // was fetched in another deployment's language gets re-resolved so its title /
    // synopsis / genres come back in this country's own.
    forceRetry = movieService.forceResolve, refill = movieService.refillTmdbSlot, country = country,
    // A cinema-vs-resolution DIRECTOR disagreement is confirmed against the film's
    // TMDB crew before the sweep acts: the venue crediting a film's other director,
    // or the person behind a pseudonym, is not a wrong film, and re-resolving a
    // correct row can lose its resolution. Runtime disagreements pass straight
    // through — they compare numbers, not names.
    confirmContradiction = crewConfirmation.confirmed,
    maxEnqueuePerTick = maxTmdbRetryEnqueuePerTick,
    runStore = scheduledRunStore)

  lazy val crewConfirmation: CrewConfirmation = new CrewConfirmation(new CrewConfirmation.Credits {
    def personIds(name: String): Seq[Int] = tmdbClient.findPersonCandidates(name)
    def crewIds(tmdbId: Int): Set[Int]    = tmdbClient.crewIds(tmdbId)
  })
}
