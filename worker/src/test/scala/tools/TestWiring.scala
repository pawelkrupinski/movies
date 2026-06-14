package tools

import clients.TmdbClient
import models.Cinema
import modules.WorkerWiring
import services.{MongoConnection, Stoppable}
import services.events.{DomainEvent, EventBus}
import services.freshness.{FreshnessStore, InMemoryFreshnessStore}
import services.movies.MovieService
import services.tasks.{EnrichDetailsHandler, InMemoryTaskQueue, TaskQueue, TaskType}

import scala.concurrent.{Await, ExecutionContextExecutorService, Future}
import scala.concurrent.duration._

/** Test seam over the worker's [[WorkerWiring]] composition root: pins a
 *  disabled Mongo, a stub TMDB key, and the full city catalogue so fixture
 *  replay and the coverage spec see every cinema. The serving-side seams
 *  (controllerComponents, materializer, environmentMode) are gone — the worker
 *  is a plain `def main` app, not Play, so they no longer exist to override. */
trait TestWiring extends WorkerWiring {

  // Scrape every city in tests, independent of any KINOWO_SCRAPE_CITIES the
  // local/CI env might set, so the recorded fixtures and the coverage spec
  // always see the full catalogue. (Production already defaults to every city;
  // this pin just makes the test set immune to a narrowing override.)
  override def scrapeCities: Set[String] = ScrapeCities.allCities

  // Pin a DISABLED Mongo connection. Tests get their movie data from
  // `InMemoryMovieRepository` / fixtures and don't exercise the user repos, so a real
  // Mongo is never needed — but the production `fromEnv` would still CONNECT to
  // whatever `MONGODB_URI` (`.env.local`) is reachable. With a developer's
  // `flyctl proxy 27017` tunnel up, that hydrated real PRODUCTION enrichment
  // into otherwise-hermetic end-to-end specs, so a row's ratings differed
  // depending on whether the tunnel happened to be open — a flake that never
  // reproduced on CI (no tunnel there). Disabling it here makes every test
  // wiring deterministic regardless of the local environment.
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = "kinowo", required = false)

  // In-memory task queue + freshness store so the queue-driven wiring boots
  // without Mongo: the bus enqueuers (ratingEnqueuer, and the detail enqueuers
  // when deferred detail is on) write here harmlessly. The harness never runs
  // the TaskWorker — it drives enrichment synchronously (see `enrichRatingsSync`
  // / `converge`) — so these stay drained.
  override lazy val taskQueue: TaskQueue = new InMemoryTaskQueue
  override lazy val freshnessStore: FreshnessStore = new InMemoryFreshnessStore

  // The harness's detail handler publishes its `MovieDetailsComplete` (the TMDB
  // re-trigger after a deferred detail lands) to THIS buffer rather than straight
  // to the bus. `enrichDetailsSync` flushes the buffer to the real bus only after
  // EVERY detail in the pass has merged — preserving the "settle the whole tick,
  // THEN publish" invariant `runOneScrapeTick` relies on (publishing mid-drain
  // would let the async TMDB stage race other films' detail merges → an
  // order-dependent single-pass corpus). Production publishes inline (no buffer)
  // so a detail-complete film enriches immediately.
  private val detailEventBuffer = scala.collection.mutable.ListBuffer.empty[DomainEvent]
  private val detailCaptureBus: EventBus = new EventBus {
    def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
    def publish(event: DomainEvent): Unit = { detailEventBuffer += event; () }
  }
  override lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache, freshnessStore, uptimeMonitor, detailCaptureBus
  )

  // No Filmweb fallback in tests: pin the id map empty so fixture replay never
  // resolves (one GET per Filmweb city) or fetches Filmweb live. Eligible scrapers
  // are still wrapped in FilmwebFallbackScraper, but with no id the fallback
  // short-circuits to the primary's real outcome — identical to pre-fallback
  // behaviour, so fixture snapshots are unaffected.
  override protected lazy val filmwebFallbackIds: Map[Cinema, Int] = Map.empty

  // Inject a stub TMDB API key so the test doesn't depend on a `TMDB_API_KEY`
  // env var. `TmdbClient.search` short-circuits to `None` when the key is
  // absent — without an override, every CI runner (and any local box without
  // `.env.local`) sees no TMDB resolution, no TmdbResolved events, no
  // enrichment cascade. The fixture replay doesn't need a real key (the URL's
  // `api_key` query param is stripped from the fixture fingerprint via
  // `RecordingHttpFetch.stableQueryFingerprint`), so any non-empty string works.
  override lazy val tmdbClient: TmdbClient = new TmdbClient(httoFetch, apiKey = Some("test-api-key"))

  // Resolve TMDB INLINE in fixture replay. Production dispatches single-movie
  // resolution as a `ResolveTmdb` task (drained by the TaskWorker), but the
  // harness never runs the worker — it drives enrichment synchronously
  // (`drainServices` / `converge`) and relies on `taskQueue` staying drained.
  // Overriding the production `enqueueResolveTmdb` seam back to None makes a
  // `MovieDetailsComplete` resolve on the `ec` pool exactly as before, so the
  // determinism + snapshot harness is unchanged. The shared `resolveTmdbOnce`
  // is the same work either way; the enqueue seam is covered by the unit +
  // WorkerWiring specs. A missing fixture is a permanent miss (the inline path
  // drops a transient failure without retrying — no cascade churn).
  override lazy val movieService =
    new MovieService(movieCache, eventBus, tmdbClient, backgroundBudget.ec("enrichment-worker"))

  // Staging repository/folder are Mongo-backed in prod; pin in-memory here — TestWiring's
  // Mongo is disabled, so the inherited MongoStagingRepository would silently drop the
  // diverted newcomers and MongoStagingFolder couldn't open a transaction. The
  // fixture wiring drives promote+fold explicitly (see FixtureTestWiring.drainStaging).
  override lazy val stagingRepository: services.staging.StagingRepository = new services.staging.InMemoryStagingRepository()
  override lazy val stagingFolder: services.staging.StagingFolder =
    new services.staging.InMemoryStagingFolder(stagingRepository, movieRepository)

  // Don't retry cinema scrapes in fixture replay: a missing fixture is permanent,
  // so backoff per fixture-less cinema just multiplies fixture-server boot time
  // (FixtureServerMain scrapes the whole 40+-city catalogue; the retry churn was
  // pushing boot past CI's 300s port-file ceiling → iOS/Android LocalServer
  // "never wrote a port file"). The ceiling clamps EVERY cinema's own
  // `maxFetchAttempts` down to a single no-retry attempt.
  override def scrapeAttemptCeiling: Int = 1

  /** Synchronously force one title all the way through the enrichment cascade:
   *  TMDB resolve → IMDb id recovery → the four `*Ratings.refreshOneSync` URL
   *  discovery + rating scrapes. Idempotent — safe to call after the bus-driven
   *  path; already-resolved rows re-hit the same URLs (so `RecordingHttpFetch`
   *  overwrites each fixture with byte-identical content). Test/tooling-only:
   *  the fixture recorder uses it as a belt-and-braces pass so no row is left
   *  half-enriched by an async retry that outlived the drain. */
  def fullySyncOne(title: String, year: Option[Int]): Unit = {
    if (movieService.get(title, year).flatMap(_.tmdbId).isEmpty) movieService.reEnrichSync(title, year)
    for {
      row <- movieService.get(title, year)
      _   <- row.tmdbId if row.imdbId.isEmpty
    } imdbIdResolver.resolveSync(title, year, row.originalTitle.getOrElse(title))
    imdbRatings.refreshOneSync(title, year)
    rottenTomatoesRatings.refreshOneSync(title, year)
    metascoreRatings.refreshOneSync(title, year)
    // `auditOneSync`, not `refreshOneSync`: Filmweb resolves by a fuzzy
    // title/director SEARCH, so the bus-driven async pass — which runs against a
    // partially-merged row — can land (or miss) a different URL run-to-run.
    // `refreshOneSync` would then take the cheap rating-only path and PRESERVE
    // that order-dependent URL; the audit instead RE-resolves against the now
    // settled row and overwrites/drops it, so Filmweb is a pure function of the
    // final row like every IMDb-id-keyed source already is.
    filmwebRatings.auditOneSync(title, year)
  }

  /** Synchronously refresh all four ratings for every TMDB-resolved row — the
   *  same per-row `refreshOneSync` the queue's `RatingHandler` runs per task,
   *  applied to the whole corpus in a deterministic title order. Production
   *  enqueues a rating task only off `TmdbResolved` / `ImdbIdMissing` — i.e.
   *  only for rows TMDB matched (a `tmdbId`); rows with no TMDB match get no
   *  rating enrichment. We mirror that gate here, so a row Filmweb could find by
   *  title but TMDB couldn't isn't enriched in the harness when it wouldn't be
   *  in prod. Production drains these as queue tasks on the `TaskWorker`; the
   *  harness doesn't run the worker, so it drives the same refresh inline (after
   *  the async TMDB + IMDb-id cascade has settled in `drainServices`). */
  def enrichRatingsSync(): Unit =
    movieCache.snapshot()
      .filter(_.record.tmdbId.isDefined)
      .map(r => (r.title, r.year))
      .sortBy { case (t, y) => (t, y.getOrElse(Int.MinValue)) }
      .foreach { case (t, y) =>
        try {
          imdbRatings.refreshOneSync(t, y)
          rottenTomatoesRatings.refreshOneSync(t, y)
          metascoreRatings.refreshOneSync(t, y)
          filmwebRatings.refreshOneSync(t, y)
        } catch { case _: Exception => () }
      }

  /** Apply deferred per-film detail to the bare-scraped rows, the way the
   *  production TaskWorker does — but synchronously, in one pass. First
   *  `detailReaper.tick()` enqueues a detail task per (deferred cinema, CURRENT
   *  film) keyed off each row's present CacheKey (robust to a film re-keyed by a
   *  later cinema in the same tick, which the CinemaMovieAdded-driven enqueue
   *  can't follow — prod's reaper fixes that across ticks). Then drain every
   *  EnrichDetails task through the real `EnrichDetailsHandler` (fetch the detail
   *  page + merge into the slot). Called right after the bare scrape and BEFORE
   *  TMDB resolution, so a detail-page director/originalTitle/year is on the row
   *  when the TMDB stage runs (the pre-deferral inline path had it in fetch()). */
  def enrichDetailsSync(): Unit = {
    detailReaper.tick()
    val workerId = "detail-sync"
    Iterator.continually(taskQueue.claim(workerId, 5.minutes))
      .takeWhile(_.isDefined).flatten
      .foreach { task =>
        if (task.taskType == TaskType.EnrichDetails)
          try enrichDetailsHandler.handle(task) catch { case _: Exception => () }
        taskQueue.complete(task.id, workerId)
      }
    // Every detail has merged + cleared `detailPending`; now re-trigger TMDB for
    // the films whose detail just landed, against a fully-settled cache.
    val ready = detailEventBuffer.toList
    detailEventBuffer.clear()
    ready.foreach(eventBus.publish)
  }

  def quiesce(stoppables: Stoppable*): Unit =
    stoppables.foreach(_.stop())

  /** Drain the event-cascade worker pools so every `TmdbResolved` /
   *  `ImdbIdMissing` / `ImdbIdResolved` published during the scrape is processed
   *  end to end. Uses the same `cascadeDrainOrder` production shutdown does —
   *  single source of truth for the producer→consumer ordering.
   *
   *  Kino Muza's detail-page synopsis/poster/trailer is no longer settled here:
   *  it now rides the standard deferred-detail pipeline (a deduped
   *  `EnrichDetails` task), which `enrichDetailsSync` drains to completion
   *  alongside every other `DetailEnricher` before the snapshot is taken. */
  def drainServices(): Unit =
    quiesce(cascadeDrainOrder*)

  /** Drive the staging incubation the worker's promoter scheduler runs in prod:
   *  promote each `pending_movies` row (detail-enrich then TMDB-resolve); a
   *  concluded row auto-folds into `movies` via the `StagingFilmEnriched`
   *  subscription. Loop until no further fold, then FORCE-fold any leftover
   *  (TMDB-fixture-less) rows — `concludeEnrichment` then marks those still-
   *  unresolved `movies` rows `tmdbNoMatch`, the same end state a no-fixture film
   *  reaches on the direct route. The harness has no `movies` change stream, so
   *  finally rehydrate the cache from the repository (prod's stream does this), which
   *  also re-settles the freshly-folded rows.
   *
   *  Staging ingest is always-on in prod, so EVERY newcomer is diverted to
   *  `pending_movies` on a cold cache. Both the replay harness (bootStartup /
   *  converge) and the fixture recorder must drive this, or the `movies` cache
   *  stays empty and no TMDB/IMDb/rating enrichment ever runs. */
  def drainStaging(): Unit = {
    var folded = true
    while (folded) {
      val before = stagingRepository.findAll().size
      if (before == 0) folded = false
      else {
        try stagingPromoter.runOnce() catch { case _: Exception => () }
        folded = stagingRepository.findAll().size < before
      }
    }
    stagingRepository.findAll().map(r => (r.title, r.year)).distinct.foreach { case (t, y) =>
      try stagingFolder.foldFilm(t, y) catch { case _: Exception => () }
    }
    movieCache.rehydrate()
    // Settle the freshly-folded rows the way prod's PERIODIC settle does once the
    // change stream catches the fold up: the fold keeps each variant's OWN year
    // (so the CC ±1 split lands two rows), and `canonicalizeBySanitize` then
    // collapses them AND re-keys the resolved cluster to its TMDB year — exactly
    // what the direct path's `settleResolved` does inline at resolution. Without
    // this the folded row keeps its lower cinema-reported key year, which only
    // shows up downstream as a different Filmweb year-tie-break (a film resolving
    // to a different / no Filmweb entry).
    movieService.settle()
  }

  /** Scrape every cinema once in parallel, blocking until all have settled.
   *  Each scraper runs the shared `cinemaScrapeRunner` (record + publish), so
   *  every `MovieDetailsComplete` is published synchronously before this returns
   *  and the caller can drain the downstream pools without racing the scrape. */
  def scrapeAllOnce(): Unit = {
    val ec: ExecutionContextExecutorService = DaemonExecutors.boundedEC("record-scrape", 8)
    try Await.ready(
      Future.sequence(cinemaScrapers.map(s =>
        Future(scala.util.Try(cinemaScrapeRunner.run(s)))(using ec)))(using implicitly, ec),
      Duration.Inf)
    finally ec.shutdown()
    ()
  }

  /** Cold cache → fully-scraped, staging-drained `movies` cache: the boot
   *  sequence the fixture recorder shares with its regression spec
   *  (`RecorderStagingDrainSpec`). Scrape every cinema, apply deferred detail,
   *  drain the async cascade, THEN graduate newcomers out of always-on staging.
   *
   *  The `drainStaging` step is load-bearing: staging ingest is always-on, so on
   *  a cold cache `recordCinemaScrape` diverts every scraped film to
   *  `pending_movies`. Omit the drain and `movies` stays EMPTY — nothing
   *  downstream (TMDB/IMDb/MC/RT/Filmweb) has a row to enrich, and a fixture
   *  recording captures only the cinema scrapes. Keeping the sequence here, not
   *  inline in the recorder, is what lets the spec catch a dropped stage. */
  def scrapeAndDrainToCache(): Unit = {
    scrapeAllOnce()
    enrichDetailsSync()
    drainServices()
    drainStaging()
  }
}
