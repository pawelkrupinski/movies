package modules.webwiring

import controllers.{MetricsController, WebMovieMetrics}
import modules.Wiring
import services.metrics.{WebCacheMetrics, WebHostMetrics, WebHttpMetrics, WebJvmMetrics}

/** ── /metrics ──────────────────────────────────────────────────────────────
 *  Everything the web tier exposes to Prometheus, on ONE registry: the served
 *  corpus, the JVM, request rate/latency, the host and the in-heap caches. */
trait MetricsWiring { self: Wiring =>

  // Exposes the in-app /uptime health (Mongo `uptimeBuckets`) as Prometheus
  // gauges for the self-hosted Grafana — host metrics alone can't see a service
  // failing silently behind a fallback (the residential proxy → Zyte case).
  // Samples per-city served-film counts every minute (all future / showing
  // tomorrow), appended to /metrics for Grafana to graph + alert on swings.
  // A web deployment serves exactly one country; tag its /metrics with that
  // country so its series line up with the worker's per-country series in Grafana.
  private val metricsCountry = models.Country.fromEnv
  lazy val webMovieMetrics = new WebMovieMetrics(movieControllerService, cities = metricsCountry.cities, country = metricsCountry.code)
  lazy val webJvmMetrics = new WebJvmMetrics
  // Request rate / error rate / latency, recorded by `HttpMetricsFilter` on the
  // SAME registry the JVM collectors use — so it surfaces on the existing
  // /metrics body with no new endpoint. Replaces the dead Fly-proxy panels
  // (`fly_app_http_*`); see WebHttpMetrics for the cardinality rules.
  lazy val webHttpMetrics = new WebHttpMetrics(webJvmMetrics.registry, metricsCountry.code)
  // The MACHINE's free RAM and free disk, read from the process's own kernel.
  // Same registry again, same reason — and same cause: Fly's host metrics
  // (`fly_instance_memory_*`, `fly_volume_*`) died with the managed-Prometheus
  // token, and nothing else scrapes the web tier's host.
  // NOT lazy: nothing reads this object again — registering its callback gauges
  // on the registry IS its whole job — so a `lazy val` would never be forced and
  // the panels would stay as blank as they were with Fly's metrics gone.
  private val webHostMetrics = new WebHostMetrics(webJvmMetrics.registry, metricsCountry.code)
  // How much heap the gzipped-response cache is holding, against its budget. NOT
  // lazy for the same reason as the line above: registering the gauges is the
  // whole job. It forces `encodedResponseCache`, which is only a map — no I/O, no
  // ordering constraint.
  // Every in-heap cache this tier holds, on one `kinowo_web_cache_*` family. The
  // two share-card caches are separate budgets (film cards get four times the
  // city cards'), so they are separate series rather than a sum.
  private val webCacheMetrics = new WebCacheMetrics(webJvmMetrics.registry, metricsCountry.code, Seq(
    "response"     -> (() => encodedResponseCache.occupancy),
    "og_card_film" -> (() => ogCardService.cacheOccupancy),
    "og_card_city" -> (() => cityOgCardService.cacheOccupancy)))
  lazy val metricsController = new MetricsController(controllerComponents, uptimeMonitor, webMovieMetrics, webJvmMetrics, metricsCountry.code)
}
