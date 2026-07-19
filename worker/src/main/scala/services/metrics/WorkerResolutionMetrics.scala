package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.resolution.{ResolutionOutcome, ResolutionOutcomeRecorder}

/**
 * `kinowo_worker_resolution_total` — one counter incremented per identity
 * RESOLUTION attempt (the "which TMDB id / IMDb id / Filmweb-RT-Metacritic url
 * is this film" step), labelled by `country`, `source` (tmdb / imdb / rt / mc /
 * filmweb) and `outcome` ([[ResolutionOutcome]]).
 *
 * WHY: the resolution cache's value was never measured. A hit avoids a probe
 * chain that costs up to ~30 upstream GETs for RT/Metacritic and ~36 for
 * Filmweb; a miss runs it. But the resolved url is ALSO persisted onto the film
 * record, so most films never re-enter the chain regardless of the cache — which
 * makes it an open question whether the cache earns its keep. These are the
 * numbers that settle it:
 *
 *  - `hit_memory` + `hit_store` vs `miss_*` per source — the actual saving.
 *  - `hit_store` alone — how much of that saving survives a restart, i.e. what
 *    the durable Mongo half (5 collections + TTL index reconciliation) buys over
 *    a plain in-process Caffeine.
 *  - `miss_unresolved` — chains run that produced nothing. Hits-only means these
 *    are never cached, so this rate is the load the cache does NOT absorb. If it
 *    dominates, caching negative results is the change worth making, not
 *    deleting the cache.
 *
 * Registered ONCE on the shared [[WorkerMetrics]] registry with a leading
 * `country` label, and seeded to 0 across the full country × source × outcome
 * grid so no Grafana line starts with a gap.
 */
class WorkerResolutionMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`, so the name is declared without it.
  private val resolutions: Counter = Counter.builder()
    .name("kinowo_worker_resolution")
    .help("Identity-resolution attempts (film → TMDB id / IMDb id / rating-site url) since boot, " +
      "by country, source and outcome (hit_memory / hit_store / miss_resolved / miss_unresolved). " +
      "A hit is a probe chain avoided; miss_unresolved is a chain that ran and cached nothing.")
    .labelNames("country", "source", "outcome")
    .register(registry)

  seed()

  private def seed(): Unit =
    for (c <- countryCodes; s <- WorkerResolutionMetrics.Sources; o <- ResolutionOutcome.all)
      resolutions.labelValues(c, s, o)

  /** A recorder bound to one country and source — what a [[modules.WorkerWiring]]
   *  injects into each per-source [[services.resolution.WriteThroughResolutionCache]]. */
  def recorderFor(country: String, source: String): ResolutionOutcomeRecorder =
    (outcome: String) => resolutions.labelValues(country, source, outcome).inc()
}

object WorkerResolutionMetrics {
  /** The sources that own a resolution cache, matching the `resolve_*` Mongo
   *  collection names the wiring builds. Listed here only to seed the grid — the
   *  label value itself comes from the wiring's collection name. */
  val Sources: Seq[String] = Seq("tmdb", "imdb", "rt", "mc", "filmweb")
}
