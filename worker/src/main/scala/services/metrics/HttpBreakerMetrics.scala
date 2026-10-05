package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, GaugeWithCallback}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.CircuitBreakerMeter

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList}
import scala.jdk.CollectionConverters._

/**
 * The per-host circuit breakers ([[tools.HostCircuitBreakerHttpFetch]]) as metrics, by `country` and
 * `leg` — a closed set ([[HttpBreakerMetrics.Leg]]), never the host:
 *
 *  - `kinowo_worker_http_breaker_open_hosts` — hosts skipped outright right now. A non-zero
 *    `decodo` is the residential proxy failing for those destinations (every call falls straight
 *    through to direct); a climbing `scrape` / `enrich` is cinema sites / metadata APIs down.
 *  - `kinowo_worker_http_breaker_opens_total` — each closed → open transition.
 *
 * Until these existed an open breaker was a log line only ("Circuit OPEN for …"): a Decodo outage
 * or a TMDB 5xx spell the breaker absorbed showed on no panel.
 */
class HttpBreakerMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {
  import HttpBreakerMetrics.Leg

  // Every breaker built for one (country, leg) — a wiring builds several per leg (one per proxied chain).
  private val watched = new ConcurrentHashMap[(String, String), CopyOnWriteArrayList[() => Int]]()

  private val opens: Counter = Counter.builder()
    .name("kinowo_worker_http_breaker_opens")
    .help("Per-host circuit breakers that OPENED since boot (closed -> open; a failed half-open probe is " +
      "not counted again), by country and leg (scrape = cinema sites, enrich = metadata/rating APIs, " +
      "decodo = the residential-proxy leg).")
    .labelNames("country", "leg")
    .register(registry)

  GaugeWithCallback.builder()
    .name("kinowo_worker_http_breaker_open_hosts")
    .help("Hosts whose circuit breaker is OPEN right now — every call to them fast-fails without " +
      "touching the wire — by country and leg (scrape / enrich / decodo).")
    .labelNames("country", "leg")
    .callback { callback =>
      for (c <- countryCodes; l <- Leg.all)
        callback.call(openHosts(c, l).toDouble, c, l)
    }
    .register(registry)

  for (c <- countryCodes; l <- Leg.all) opens.labelValues(c, l)

  /** How many hosts are open now across every breaker of (`country`, `leg`). */
  def openHosts(country: String, leg: String): Int =
    Option(watched.get((country, leg))).fold(0)(_.asScala.iterator.map(_()).sum)

  /** The meter one breaker of (`country`, `leg`) reports to. */
  def meterFor(country: String, leg: String): CircuitBreakerMeter = new CircuitBreakerMeter {
    def watch(openHosts: () => Int): Unit = {
      watched.computeIfAbsent((country, leg), _ => new CopyOnWriteArrayList[() => Int]()).add(openHosts); ()
    }
    def opened(): Unit = opens.labelValues(country, leg).inc()
  }
}

object HttpBreakerMetrics {
  /** The `leg` label values — the alert, the panel and the seed agree on these strings. */
  object Leg {
    val Scrape = WorkerHttpMetrics.Phase.Scrape
    val Enrich = WorkerHttpMetrics.Phase.Enrich
    val Decodo = "decodo"
    val all: Seq[String] = Seq(Scrape, Enrich, Decodo)
  }
}
