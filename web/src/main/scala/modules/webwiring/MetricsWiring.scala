package modules.webwiring

import controllers.{MetricsController, WebMovieMetrics}
import modules.Wiring
import services.metrics.{LegacyUserStateMetrics, UserStateIndexMetrics, UserStateWriteMetrics, WebCacheMetrics, WebHostMetrics, WebHttpMetrics, WebJvmMetrics}

/** ── /metrics ──────────────────────────────────────────────────────────────
 *  Everything the web tier exposes to Prometheus, on ONE registry: the served
 *  corpus, the JVM, request rate/latency, the host, the in-heap caches, and
 *  per-scraper-client fallback saturation. `filmwebFallbackStore` (below) is
 *  `AdminWiring`'s, reached through the shared `self: Wiring` self-type. */
trait MetricsWiring { self: Wiring =>

  // Exposes the in-app /uptime health (Mongo `uptimeBuckets`) as Prometheus
  // gauges for the self-hosted Grafana — host metrics alone can't see a service
  // failing silently behind a fallback (the residential proxy → Zyte case).
  // Samples per-city served-film counts every minute (all future / showing
  // tomorrow), appended to /metrics for Grafana to graph + alert on swings.
  // A web deployment serves exactly one country; tag its /metrics with that
  // country so its series line up with the worker's per-country series in Grafana.
  private def metricsCountry = country
  lazy val webMovieMetrics = new WebMovieMetrics(movieControllerService, country)
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
  // Every in-heap cache this tier holds, on one `kinowo_web_cache_*` family.
  private val webCacheMetrics = new WebCacheMetrics(webJvmMetrics.registry, metricsCountry.code, Seq(
    "response" -> (() => encodedResponseCache.occupancy)))
  lazy val metricsController = new MetricsController(controllerComponents, uptimeMonitor, filmwebFallbackStore, webMovieMetrics, webJvmMetrics, metricsCountry.code, clock)
  // Retirement signal for the legacy PUT /api/me/state — see the class doc.
  // Safe as `lazy`, unlike webHostMetrics/webCacheMetrics above: userStateController
  // (below, in UsersWiring) holds a reference and is itself forced at boot by
  // the router, so this gets forced too — nothing here needs an eager `val`.
  lazy val legacyUserStateMetrics = new LegacyUserStateMetrics(webJvmMetrics.registry, metricsCountry.code, clock)
  // Every atomic user-state write, by endpoint and outcome — see the class doc.
  // Lazy for the same reason as the line above: `userStateRepository` (UsersWiring)
  // holds it and is forced at boot through the router.
  lazy val userStateWriteMetrics = new UserStateWriteMetrics(webJvmMetrics.registry, metricsCountry.code)
  // Whether `userStates` has its unique userId index — see the class doc. Lazy, as above.
  lazy val userStateIndexMetrics = new UserStateIndexMetrics(webJvmMetrics.registry, metricsCountry.code)
}
