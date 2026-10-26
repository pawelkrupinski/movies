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
 *  - `kinowo_worker_http_circuit_open{host}` — 1 per host that tripped and has not recovered, series
 *    present ONLY while it is so (bounded by the outage, not by the roster). `WorkerHttpHostCircuitOpen`
 *    alerts on it.
 *
 * Until these existed an open breaker was a log line only ("Circuit OPEN for …"): a Decodo outage
 * or a TMDB 5xx spell the breaker absorbed showed on no panel.
 */
class HttpBreakerMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {
  import HttpBreakerMetrics.Leg

  // Every breaker built for one (country, leg) — a wiring builds several per leg (one per proxied chain).
  private val watched = new ConcurrentHashMap[(String, String), CopyOnWriteArrayList[() => Int]]()

  private val watchedHosts = new ConcurrentHashMap[(String, String), CopyOnWriteArrayList[() => Seq[String]]]()

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

  // One series per host that has NOT recovered (tripped, no answer since), so the label set is the outage
  // itself and empty when all is well. It holds through the half-open gaps between failed probes, which
  // `open_hosts` does not: that is what lets an alert say "this host, continuously, for an hour".
  GaugeWithCallback.builder()
    .name("kinowo_worker_http_circuit_open")
    .help("1 for each host whose circuit breaker has tripped and not yet recovered (open, or half-open with " +
      "its probe still failing), by country, leg and host. Absent while the host is healthy.")
    .labelNames("country", "leg", "host")
    .callback { callback =>
      for (((c, l), _) <- watchedHosts.asScala.toSeq.sortBy(_._1); h <- unrecoveredHosts(c, l))
        callback.call(1.0, c, l, h)
    }
    .register(registry)

  for (c <- countryCodes; l <- Leg.all) opens.labelValues(c, l)

  /** The hosts of (`country`, `leg`) whose breaker has tripped and not recovered, across every breaker of it. */
  def unrecoveredHosts(country: String, leg: String): Seq[String] =
    Option(watchedHosts.get((country, leg)))
      .fold(Seq.empty[String])(_.asScala.iterator.flatMap(_()).toSeq.distinct.sorted)

  /** How many hosts are open now across every breaker of (`country`, `leg`). */
  def openHosts(country: String, leg: String): Int =
    Option(watched.get((country, leg))).fold(0)(_.asScala.iterator.map(_()).sum)

  /** The meter one breaker of (`country`, `leg`) reports to. */
  def meterFor(country: String, leg: String): CircuitBreakerMeter = new CircuitBreakerMeter {
    def watch(openHosts: () => Int): Unit = {
      watched.computeIfAbsent((country, leg), _ => new CopyOnWriteArrayList[() => Int]()).add(openHosts); ()
    }
    override def watchHosts(hosts: () => Seq[String]): Unit = {
      watchedHosts.computeIfAbsent((country, leg), _ => new CopyOnWriteArrayList[() => Seq[String]]()).add(hosts); ()
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
