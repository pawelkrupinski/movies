package modules.wiring

import clients.TmdbClient
import modules.WorkerWiring
import services.enrichment.{FilmwebClient, ImdbClient, LetterboxdClient, LetterboxdIdResolver, MetacriticClient, OMDbClient, RottenTomatoesClient, WikidataClient}
import services.metrics.WorkerHttpMetrics
import tools.{CountingHttpFetch, HostCircuitBreakerHttpFetch, HttpFetch, MonitoringHttpFetch, RateLimitedHttpFetch, RealHttpFetch, ThrottledHttpFetch}

/** The two phase-labelled HTTP chains over ONE wire leaf, and the third-party
 *  metadata / rating / resolution clients that draw from the `enrich` one.
 *  Cinema-site egress (proxies, Zyte) is [[EgressWiring]]. */
trait HttpWiring { self: WorkerWiring =>

  // `cinemaScraperCatalog.scrapeHosts` is passed BY-NAME (the catalog fetches
  // through this very `httoFetch`, so eager evaluation would cycle). It's forced
  // once on the first request and tells the monitor which hosts are cinema
  // scrapes — suppressed, since RetryingCinemaScraper already tracks each cinema
  // under its displayName.
  // ThrottledHttpFetch sits closest to the wire: a per-host 429 gate that pauses
  // ALL callers to a rate-limited host together (honoring Retry-After) instead of
  // each of the ~12 concurrent TMDB callers retrying independently into the same
  // burst. Inside MonitoringHttpFetch so its waits don't skew uptime.
  // The per-host circuit breaker sits closest to the wire: after a few consecutive
  // timeouts/5xx from a host it OPENS and fast-fails every further call to that
  // host for a cooldown, so a slow/hanging origin (Helios's restapi, 2026-06-23)
  // stops pinning the ParallelDetailFetch slots for its whole timeout on every
  // call. Generalises RealHttpFetch's static per-host timeout policy (HostPolicies) to
  // any host, with no allowlist. Wrapped by ThrottledHttpFetch (429 gate) and
  // MonitoringHttpFetch (so a fast-fail still shows as the real unavailability).
  // RateLimitedHttpFetch is innermost: the PROACTIVE half of rate-limit handling,
  // pacing the hosts whose HostPolicies row names a minRequestInterval (Filmstarts)
  // so we stay under the limit instead of rediscovering it via 429 every sweep.
  // Inside the breaker so a fast-failed call never waits for (or consumes) a slot,
  // and inside the 429 gate so a real Retry-After still overrides the steady pace.
  // CountingHttpFetch is INNERMOST (around the leaf RealHttpFetch): it tallies
  // every wire attempt's outcome into kinowo_worker_http_total for THIS country
  // under the chain's call PHASE label. Innermost is deliberate — each 429 retry
  // (ThrottledHttpFetch) and each scraper backoff retry re-hits the leaf as a
  // distinct call, so every attempt is counted once; a circuit-breaker fast-fail
  // never reaches here and so isn't miscounted as a real attempt.
  //
  // ── Phase split ───────────────────────────────────────────────────────────
  // We build the SAME chain twice, once per call phase (see WorkerHttpMetrics.Phase),
  // differing ONLY at the innermost counter's `phase` label: `httoFetch` tags every
  // cinema-site call `scrape`, `enrichmentFetch` tags every third-party
  // metadata/rating/resolution call `enrich`. Both wrap ONE shared RealHttpFetch
  // leaf, so there is still one connection pool / cookie jar exactly as before —
  // the split is a labelling concern, not a second wire. The two chains own
  // independent per-host 429-gate / circuit-breaker state, which is fine: their
  // host sets are disjoint (cinema sites vs metadata APIs) bar Filmweb, whose
  // rating client and fallback-scraper client legitimately live on opposite phases.
  protected def realHttpLeaf: HttpFetch = new RealHttpFetch()
  private lazy val sharedRealHttpLeaf: HttpFetch = realHttpLeaf
  // `protected`, not private: the archive-replay wiring rebuilds the enrich-phase
  // chain to hang its own cache OUTSIDE it (a cache hit must not be metered,
  // throttled or rate-limited — it never touches the wire).
  protected def phaseFetch(phase: String): HttpFetch =
    new MonitoringHttpFetch(
      new ThrottledHttpFetch(
        new HostCircuitBreakerHttpFetch(
          new RateLimitedHttpFetch(
            new CountingHttpFetch(sharedRealHttpLeaf,
              workerMetrics.httpMetrics.recorderFor(country.code, phase))))),
      uptimeMonitor, cinemaScraperCatalog.scrapeHosts)

  // Cinema-site HTTP — every listing scrape, chunk scrape and per-film detail
  // fetch. The `scrape` phase; dominates volume and is what the scrape-health panel
  // isolates. The catalog + Multikino/biletyna/Zyte proxy chains + detail cache all
  // draw from this fetch, so they all tally under `scrape`.
  lazy val httoFetch: HttpFetch =
    phaseFetch(WorkerHttpMetrics.Phase.Scrape)
  // Third-party metadata/rating/resolution APIs. The `enrich` phase; separated so
  // its by-design 404 slug-probing and API 429s don't blur the cinema-scrape
  // failure budget. Every rating/resolution client (below) draws from this fetch.
  lazy val enrichmentFetch: HttpFetch =
    phaseFetch(WorkerHttpMetrics.Phase.Enrich)

  // ── External API clients ──────────────────────────────────────────────────
  // All draw from `enrichmentFetch` so their attempts tally under the `enrich`
  // phase, apart from the cinema-facing scrapers/resolvers which use `httoFetch`.
  lazy val tmdbClient = new TmdbClient(enrichmentFetch, language = country.language)
  lazy val filmwebClient = new FilmwebClient(enrichmentFetch)
  lazy val imdbClient = new ImdbClient(enrichmentFetch)
  lazy val metacriticClient = new MetacriticClient(enrichmentFetch)
  lazy val rottenTomatoesClient = new RottenTomatoesClient(enrichmentFetch)
  // OMDb (omdbapi.com) — feature-gated fallback for the three IMDb-keyed ratings.
  // The client itself no-ops (returns None, makes no HTTP call) when OMDB_API_KEY
  // is unset; `omdbBackfill` in the ratings block builds the refresher only when
  // the key is present.
  lazy val omdbClient = new OMDbClient(enrichmentFetch)
  // Letterboxd — an id-crosswalk resolution SOURCE (not a rating source): it
  // turns a known imdbId into the exact tmdbId (and vice versa) for the
  // arthouse/festival long tail TMDB's own indexes leave unmapped, by scraping
  // its film pages. Wired into `resolveTmdbId` (after TMDB /find) and
  // `ImdbIdResolver` (after Wikidata) as a last-resort fallback.
  lazy val letterboxdClient     = new LetterboxdClient(enrichmentFetch)
  lazy val letterboxdIdResolver = new LetterboxdIdResolver(letterboxdClient)
  lazy val wikidataClient = new WikidataClient(enrichmentFetch)
}
