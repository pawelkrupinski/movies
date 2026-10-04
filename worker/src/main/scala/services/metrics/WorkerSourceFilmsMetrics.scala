package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * Per-city census of how many films the SOURCE `movies` collection would serve —
 * the worker-side mirror of the web's [[controllers.WebMovieMetrics]]
 * (`kinowo_web_movies_served`), which counts the same thing off the READ MODEL the
 * web actually serves. Both gauges carry identical `{city,scope}` labels and apply
 * the identical future/tomorrow rule ([[models.Showtime.isUpcoming]]), so a Grafana
 * panel plots them side by side: when `kinowo_worker_movies_served` and
 * `kinowo_web_movies_served` diverge for a city, the projection or a read-model
 * write has drifted from what the corpus holds — the exact signal the read-model
 * outage (a malformed `web_movies` doc silently empties a city) needs.
 *
 * Apples-to-apples by construction: the count splits each row into cards by the
 * projection's own display-title groups ([[services.readmodel.ReadModelProjection.titleKeyOf]]),
 * files each venue under its city as the projection does, and gates on the same
 * `readyToProject` predicate the projector writes by, so a film still pending TMDB
 * enrichment is absent from BOTH sides rather than inflating the source count.
 *
 * Counted by [[CorpusCensus]] from the films the worker's cache holds, at every tick.
 */
object WorkerSourceFilmsMetrics {
  /** Paired with the web's `kinowo_web_movies_served`: same suffix, same city/scope
   *  labels (plus the worker's leading `country`), worker-vs-web prefix — so Grafana
   *  overlays the two as source-vs-read-model. */
  val Name = "kinowo_worker_movies_served"

  /** Build and register the ONE shared gauge every country's census writes into
   *  (leading `country` label, then `city`, `scope`). Called once when the shared
   *  worker registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Films the source `movies` collection would serve per country and city, by scope (all = any future showing, tomorrow = showing tomorrow) — the projection-side mirror of the web's kinowo_web_movies_served, for spotting read-model drift.")
      .labelNames("country", "city", "scope")
      .register(registry)

  /** Scope label values, matching `kinowo_web_movies_served`'s scopes exactly. */
  object Scope {
    val All      = "all"
    val Tomorrow = "tomorrow"
    val all: Seq[String] = Seq(All, Tomorrow)
  }
}
