package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * Per-city census of how many INDIVIDUAL SHOWTIMES (single dated slots) the source
 * `movies` collection would serve — the volume complement to [[WorkerSourceFilmsMetrics]],
 * which counts distinct FILMS per city. A city can hold a steady film count while its
 * showtime volume swings (a chain adds evening slots, a venue drops a screen), so this
 * gauge tracks the raw slot throughput the read model carries.
 *
 * The general total is `sum(kinowo_worker_showtimes)` in Grafana — the per-city series
 * sum to it exactly, since every slot belongs to exactly one city.
 *
 * Apples-to-apples with the films gauge by construction: the same cards and cities, the
 * same `readyToProject` gate (a film still pending TMDB enrichment contributes 0 slots,
 * matching the read model), and only UPCOMING slots ([[models.Showtime.isUpcoming]] in the
 * city's own zone) so retained past showings don't inflate the count.
 *
 * Counted by [[CorpusCensus]] from each venue slot's showtime STARTS, which is all the
 * worker's cache keeps of them: a venue listing one showtime twice counts it twice, where
 * the read model lists it once (see [[CorpusCensus]]).
 */
object WorkerShowtimesMetrics {
  /** Sibling of `kinowo_worker_movies_served` — same worker prefix, same
   *  `country`+`city` labels; a gauge (not a counter), so no `_total` suffix. */
  val Name = "kinowo_worker_showtimes"

  /** Build and register the ONE shared gauge every country's census writes into
   *  (leading `country` label, then `city`). Called once when the shared worker
   *  registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Upcoming individual showtimes (single dated slots) the source `movies` collection would serve per country and city, counted through the projection's own card split and gated on readyToProject. sum() across cities is the country total. The volume complement to kinowo_worker_movies_served (which counts distinct films).")
      .labelNames("country", "city")
      .register(registry)
}
