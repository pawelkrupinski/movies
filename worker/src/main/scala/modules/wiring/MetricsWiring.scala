package modules.wiring

import modules.WorkerWiring
import services.metrics.{CinemaContentCensus, CinemaScrapeCensus, CorpusScanMetrics, RatingRunCensus, WorkerCorpusMetrics, WorkerCorpusScan, WorkerShowtimesMetrics, WorkerSlotFanoutMetrics, WorkerSourceFilmsMetrics, WorkerTaskMetrics}

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
  // Periodic census of THIS country's movies corpus (counts of resolved/rated
  // rows), sampled off-band into the shared corpus gauge — see WorkerCorpusMetrics.
  lazy val corpusMetrics: WorkerCorpusMetrics =
    new WorkerCorpusMetrics(workerMetrics.corpusGauge, country.code)

  // Per-city count of films the SOURCE `movies` collection would serve in this
  // country — the worker-side mirror of the web's kinowo_web_movies_served (read
  // model), so a Grafana panel overlays the two and a divergence flags drift.
  lazy val sourceFilmsMetrics: WorkerSourceFilmsMetrics =
    new WorkerSourceFilmsMetrics(workerMetrics.servedGauge, country.code, cities = country.cities)
  // Per-city (and, summed, country total) count of individual upcoming SHOWTIMES
  // the source `movies` collection would serve — the slot-volume complement to
  // sourceFilmsMetrics, exposed as kinowo_worker_showtimes{country,city}.
  lazy val showtimesMetrics: WorkerShowtimesMetrics =
    new WorkerShowtimesMetrics(workerMetrics.showtimesGauge, country.code, cities = country.cities)
  // The widest film's slot count — the blast radius of one film's write, since every write
  // path is per-film and the screenings cursor rings once per row written (see
  // WorkerSlotFanoutMetrics). Rides the same corpus pass as the three censuses above.
  lazy val slotFanoutMetrics: WorkerSlotFanoutMetrics =
    new WorkerSlotFanoutMetrics(workerMetrics.widestSlotsGauge, country.code)
  // ONE 5-minute corpus scan feeding all three censuses above. They each used to run
  // their own timer AND their own full scan of the same rows — 14,704 documents per
  // country per 5 min for Poland alone (measured 2026-07-18) — see WorkerCorpusScan.
  lazy val corpusScan: WorkerCorpusScan =
    new WorkerCorpusScan(movieRepository,
      Seq(corpusMetrics, sourceFilmsMetrics, showtimesMetrics, slotFanoutMetrics),
      metrics = CorpusScanMetrics.prometheus(workerMetrics.corpusScanIncomplete, country.code))
  // Per-site backlog of resolved films whose rating has NEVER run — the never-run
  // latency the first-attempt histogram can't show (see RatingRunCensus).
  lazy val ratingRunCensus: RatingRunCensus =
    new RatingRunCensus(movieCache, freshnessStore, workerMetrics.ratingNotRunGauge, workerMetrics.ratingOldestAgeGauge, country)
  // Worst-case scrape staleness across this country's roster — the cinema that has
  // gone longest without a successful scrape, plus the never-scraped count. Reads
  // the SAME freshness stamps the ScrapeReaper schedules from, so the metric and
  // the scheduler can't disagree about how overdue a cinema is (see CinemaScrapeCensus).
  lazy val cinemaScrapeCensus: CinemaScrapeCensus =
    new CinemaScrapeCensus(cinemaScrapers, freshnessStore,
      workerMetrics.scrapeOldestAgeGauge, workerMetrics.scrapeNeverScrapedGauge, country)
  // The other half of that picture: cinemas that scrape FINE and produce nothing.
  // A drifted selector keeps its scrape fresh, so the census above reads it as
  // healthy — only the archive remembers when a cinema last had real content.
  lazy val cinemaContentCensus: CinemaContentCensus =
    new CinemaContentCensus(cinemaScrapers, scrapeArchive,
      workerMetrics.contentOldestAgeGauge, workerMetrics.neverContentGauge, country)
}
