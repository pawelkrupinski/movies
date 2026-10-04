package modules.wiring

import modules.WorkerWiring
import services.metrics.{CinemaContentCensus, CinemaScrapeCensus, CorpusCensus, CorpusCensusMetrics, RatingRunCensus, RetiredVenueCensus, WorkerTaskMetrics}

/** This country's slice of the process-wide `/metrics` registry: the
 *  per-country task-pipeline facade, the cache-occupancy gauges, and the
 *  off-band censuses (corpus, rating backlog, scrape staleness, content). */
trait MetricsWiring { self: WorkerWiring =>

  // Publish this wiring's cache occupancy under `kinowo_worker_cache_*`.
  def registerCacheMetrics(): Unit = {
    // The resident corpus (unbounded — entries only) and the task dedup cache
    // (count-bounded).
    workerMetrics.registerCache(country.code, "movie_corpus", () => movieCache.occupancy)
    workerMetrics.registerCache(country.code, "task_dedup", () => taskDedupCache.occupancy)
  }

  // Task-pipeline metrics, exposed at /metrics (WorkerMain) and scraped by Fly
  // Prometheus. The queue is wrapped so every enqueue is metered centrally; the
  // TaskWorker reports claims/outcomes/durations via the same object as its
  // `TaskObserver`; the /metrics handler refreshes the queue gauges per scrape.
  // Worker metrics live in the process-wide `workerMetrics` bundle (ONE registry +
  // one set of metric objects, shared across every country's wiring), injected at
  // construction. This wiring holds only the PER-COUNTRY views/samplers that write
  // its own `country="…"` slice; the shared registry is served once by WorkerMain.
  //
  // Per-country task-pipeline facade (enqueue/claim/finish/merge/… all tagged with
  // this country) over the shared, registered-once Series.
  lazy val taskMetrics: WorkerTaskMetrics = workerMetrics.taskMetricsFor(country)
  // THIS country's `movies` census — corpus coverage, per-city films served (overlaid on the web's
  // read-model gauge), per-city upcoming showtimes, the widest film — kept film by film as the
  // cache changes, with no read of its own (see CorpusCensus).
  lazy val corpusCensus: CorpusCensus = managedResources.stopping(
    new CorpusCensus(movieCache, workerMetrics.corpusGauge, workerMetrics.servedGauge, workerMetrics.showtimesGauge,
      workerMetrics.widestSlotsGauge, country.code, country.cities, clock,
      CorpusCensusMetrics.prometheus(workerMetrics.corpusCensusIncomplete, country.code)))
  // One venue's scraped feed under another's name, told by booking sessions as each scrape lands
  // (CopiedFeedArchive) — it replaced the programme-comparing census the corpus scan used to carry.
  // Only over the venues read through an upstream known to copy feeds; None where there are none.
  lazy val copiedFeedDetector: Option[services.cinemas.roster.CopiedFeedDetector] =
    Some(services.cinemas.roster.CopiedFeedDetector.watchedVenues(countryScrapers)).filter(_.nonEmpty)
      .map(new services.cinemas.roster.CopiedFeedDetector(workerMetrics.copiedFeedPairsGauge, country, _))
  // Per-site backlog of resolved films whose rating has NEVER run — the never-run
  // latency the first-attempt histogram can't show (see RatingRunCensus).
  lazy val ratingRunCensus: RatingRunCensus = managedResources.stopping(
    new RatingRunCensus(movieCache, freshnessStore, workerMetrics.ratingNotRunGauge, workerMetrics.ratingOldestAgeGauge, country, clock = clock))
  // Worst-case scrape staleness across this country's roster — the cinema that has
  // gone longest without a successful scrape, plus the never-scraped count. Reads
  // the SAME freshness stamps the ScrapeReaper schedules from, so the metric and
  // the scheduler can't disagree about how overdue a cinema is (see CinemaScrapeCensus).
  lazy val cinemaScrapeCensus: CinemaScrapeCensus = managedResources.stopping(
    new CinemaScrapeCensus(cinemaScrapers, freshnessStore,
      workerMetrics.scrapeOldestAgeGauge, workerMetrics.scrapeNeverScrapedGauge, country, clock = clock))
  // The other half of that picture: cinemas that scrape FINE and produce nothing.
  // A drifted selector keeps its scrape fresh, so the census above reads it as
  // healthy — only the archive remembers when a cinema last had real content.
  lazy val cinemaContentCensus: CinemaContentCensus = managedResources.stopping(
    new CinemaContentCensus(cinemaScrapers, scrapeArchive,
      workerMetrics.contentOldestAgeGauge, workerMetrics.neverContentGauge, workerMetrics.contentStaleVenuesGauge, country, clock = clock))
  // Side rows a venue left behind when it was dropped from the roster — nothing serves them and
  // nothing deleted them (Kino Etiuda OBK, 2026-09). The watchdog for the cleanup that should.
  lazy val retiredVenueCensus: RetiredVenueCensus = managedResources.stopping(
    new RetiredVenueCensus(screeningsRepository, slotsRepository, services.movies.VenueRoster.venuesOf(country),
      workerMetrics.retiredVenueRowsGauge, workerMetrics.retiredVenueFutureGauge, country, clock = clock))
}
