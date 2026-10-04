package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, GaugeWithCallback}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.readmodel.ReadModelStreamMetrics

/**
 * The web read model's change streams, per `collection` (web_movies / web_screenings):
 *
 *  - `kinowo_web_readmodel_stream_live` — 1 while the stream delivers, 0 once it has ended. At 0 the
 *    pod sees that collection's writes only from a catch-up read; one pod stuck at 0 serves stale.
 *  - `kinowo_web_readmodel_stream_reopens_total` — each reopen of an ended stream (on the cold-retry
 *    cadence, backing off). A steady climb is a stream that cannot stay open.
 *
 * Before these, a Mongo blip that ended the streams was a WARN line, and the pod served writes up
 * to 30 minutes late with nothing on a panel.
 */
class WebReadModelStreamMetrics(registry: PrometheusRegistry, country: String, live: String => Boolean) extends ReadModelStreamMetrics {

  // The client auto-appends `_total`.
  private val reopens: Counter = Counter.builder()
    .name("kinowo_web_readmodel_stream_reopens")
    .help("Reopens of a read-model change stream that had ended (a Mongo outage outlasting the driver's resume), " +
      "by country and collection. Zero is the healthy reading.")
    .labelNames("country", "collection")
    .register(registry)

  GaugeWithCallback.builder()
    .name("kinowo_web_readmodel_stream_live")
    .help("1 while the read model's change stream for the collection is delivering, 0 once it has ended " +
      "(writes then reach this pod only through a catch-up read), by country and collection.")
    .labelNames("country", "collection")
    .callback(callback => ReadModelStreamMetrics.Collections.foreach(c => callback.call(if (live(c)) 1.0 else 0.0, country, c)))
    .register(registry)

  ReadModelStreamMetrics.Collections.foreach(c => reopens.labelValues(country, c))

  def reopened(collection: String): Unit = reopens.labelValues(country, collection).inc()
}
