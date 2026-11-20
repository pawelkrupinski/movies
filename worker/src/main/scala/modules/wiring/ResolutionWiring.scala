package modules.wiring


import services.corpus.MovieService
import services.enrichment.{CinemetaClient, ImdbCrossReferences, ImdbIdLadder, ImdbIdRecovery, ImdbIdResolver, ImdbSuggestions}
import services.resolution.{MongoResolutionStore, ResolutionCache, ResolutionOutcome, UnresolvedPolicy, WriteThroughResolutionCache}

/** A film's TMDB side once the identity model has named it (`MovieService`), IMDb-id recovery, the
  *  per-source resolution caches an operator's refresh forgets. (The projection's schedule is IdentityProjectionWiring's.) */
trait ResolutionWiring { self: WorkerWiring =>

  lazy val imdbIdCache: ResolutionCache = resolutionCache("resolve_imdb")
  // Named (not inline) so the operator's refreshes (the /tasks forget-all, a
  // RefreshSource's per-film forget: OperatorWiring) reach the very instance the
  // rating source resolves through — ONE instance rather than a fresh Caffeine per
  // call site, or a forget would clear a layer nobody reads.
  //
  // The three rating-LINK caches remember empty answers as well as hits: "this
  // site has no page for this film" is the common outcome and a stable one, and
  // re-deriving it costs a full probe ladder every four hours. The IMDb-id cache
  // keeps retrying, because there an empty answer is expected to become a real one
  // on its own once the upstream indexes the film.
  lazy val rtLinkCache: ResolutionCache      = resolutionCache("resolve_rt",      UnresolvedPolicy.Remember)
  lazy val mcLinkCache: ResolutionCache      = resolutionCache("resolve_mc",      UnresolvedPolicy.Remember)
  lazy val filmwebLinkCache: ResolutionCache = resolutionCache("resolve_filmweb", UnresolvedPolicy.Remember)
  /** One row's missing IMDb id, looked up on every rung and written back. */
  lazy val imdbIdRecovery = new ImdbIdRecovery(movieCache, new ImdbIdLadder(
    new ImdbSuggestions(imdbClient, imdbIdCache, movieCache.normalizer),
    new ImdbCrossReferences(wikidata = Some(wikidataClient), letterboxdIdResolver = Some(letterboxdIdResolver),
      // Same OMDB_API_KEY gate as `omdbBackfill` — the OMDb rung is inert when unset.
      omdb = configuration.omdbApiKey.map(_ => omdbClient),
      // Cinemeta needs no key — always wired as the final free rung.
      cinemeta = Some(new CinemetaClient(enrichmentFetch)))))
  lazy val imdbIdResolver = managedResources.stopping(new ImdbIdResolver(imdbIdRecovery,
    backgroundBudget.executionContext("imdb-id-resolver"), retries = threadPools.scheduler("imdb-id-retry"),
    retriesFailures = imdbIdRetriesFailures, stopwatch = stopwatch, sleep = sleep))
  /** Whether a failed IMDb-id lookup is asked again later ([[ImdbIdResolver]]'s `retriesFailures`): always, but over a
   *  hermetic replay, which overrides it. */
  protected def imdbIdRetriesFailures: Boolean = true

  // Hint-keyed resolution caches (Caffeine + per-source Mongo collection, 24h
  // TTL): the same hints resolve once instead of hitting the upstream each cycle.
  // One factory so the test wiring can swap in a passthrough (the fixture harness
  // proves the pipeline is a pure function of the corpus, with no shared cache).
  protected def resolutionCache(
    collection: String,
    unresolved: UnresolvedPolicy = UnresolvedPolicy.Retry
  ): ResolutionCache =
    new WriteThroughResolutionCache(
      new MongoResolutionStore(mongoConnection.database, collection, normalizer = titleNormalizer,
        ttlMismatches = workerMetrics.ttlIndexMismatches, clock = clock),
      // Labels the counter with the source this collection serves (`resolve_rt` →
      // `rt`), so `kinowo_worker_resolution_total` breaks the saving down per
      // rating source rather than lumping all five together.
      workerMetrics.resolutionMetrics.recorderFor(country.code, ResolutionOutcome.sourceOf(collection)),
      unresolved)
  lazy val movieService: MovieService = new MovieService(
    movieCache, eventBus, tmdbClient,
    // SAME store the rating handlers read, so the resolved → first-rating delay
    // (stamped here on resolution, observed there on first attempt) correlates.
    freshness = freshnessStore,
    // Kick a newly identified film's ratings the instant it is written — the SAME enqueuer the
    // EnrichmentReaper walks the corpus with, so both share the eligibility + due gate.
    enqueueNewcomerRatings = (key, record) => { ratingEnqueuer.enqueueDueFor(key, record, clock.instant()); () },
    // A re-identified film forces every rating source due again: its record is rebuilt without
    // the ratings its former identity held, which the cadence would otherwise keep from re-fetching.
    forceRatingRefresh = (key, record) => { ratingEnqueuer.enqueueDueFor(key, record, clock.instant(), force = true); () },
    clock = clock)
}
