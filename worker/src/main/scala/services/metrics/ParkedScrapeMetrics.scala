package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, GaugeWithCallback}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.tasks.ProxyParking.Cause

import java.util.concurrent.ConcurrentHashMap

/**
 * Venues whose scrapes are parked, by `country` and `cause` (`services.tasks.ProxyParking.Cause`):
 *
 *  - `kinowo_worker_scrapes_parked` — parked right now: `residential_proxy_down` waits on the proxy's probe and is
 *    released the moment it passes; `own_failures` waits out its own normal freshness window.
 *  - `kinowo_worker_scrapes_unparked_total` — venues released because their cause's probe passed again.
 *
 * Until these existed parking was a log line only ("failed N consecutive scrapes; parking it"): the 2026-10-27 Decodo
 * outage parked 826 venues across PL and the UK and no panel showed it, nor would one have shown them coming back.
 */
class ParkedScrapeMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val sources = new ConcurrentHashMap[String, () => Map[String, Int]]()

  GaugeWithCallback.builder()
    .name("kinowo_worker_scrapes_parked")
    .help("Venues whose scrapes are parked right now, by country and cause (residential_proxy_down = released when " +
      "the proxy's probe passes; own_failures = back on its normal freshness window).")
    .labelNames("country", "cause")
    .callback { callback =>
      for (c <- countryCodes) {
        val counts = Option(sources.get(c)).fold(Map.empty[String, Int])(_())
        for (cause <- Cause.all) callback.call(counts.getOrElse(cause, 0).toDouble, c, cause)
      }
    }
    .register(registry)

  // The client auto-appends `_total`.
  private val unparked: Counter = Counter.builder()
    .name("kinowo_worker_scrapes_unparked")
    .help("Venues released from parking because their cause's probe passed again, by country and cause.")
    .labelNames("country", "cause")
    .register(registry)

  for (c <- countryCodes; cause <- Cause.all) unparked.labelValues(c, cause)

  /** Where `country`'s parked counts are read from (its `ScrapeFreshnessPolicy.parkedByCause`). */
  def watch(country: String, counts: () => Map[String, Int]): Unit = { sources.put(country, counts); () }

  def recordUnparked(country: String, cause: String, n: Int): Unit =
    if (n > 0) unparked.labelValues(country, cause).inc(n.toDouble)
}
