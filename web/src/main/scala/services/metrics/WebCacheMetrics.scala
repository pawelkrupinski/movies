package services.metrics

import controllers.GzippedResponseCache
import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * `kinowo_web_response_cache_*` — how much heap the gzipped-response cache is
 * holding, and across how many paths.
 *
 * WHY THIS EXISTS. The cache was unbounded, and nothing in the process could say
 * what that cost. It did not matter while a deployment's whole corpus was "a
 * handful of cities × a few paths"; on the US, where a city is a STATE and the
 * largest listing is 1.06 MB gzipped, a crawler walking the sitemap pinned every
 * one of 55 states in a 768m heap that also holds the read model, and `web-us`
 * OOM-crash-looped roughly hourly. Diagnosing that meant guessing at the split
 * between the cache and the read model, because the JRE image carries no `jcmd`
 * and there is no heap dump to look at.
 *
 * So the bound has a gauge beside it. `held_bytes` against
 * [[GzippedResponseCache.DefaultMaxBytes]] says whether a deployment is evicting
 * at all — Poland's corpus fits several times over and should sit flat well under
 * the ceiling, while the US should ride at it — and a country that starts
 * evicting when it never used to is a corpus that grew.
 *
 * Read at SCRAPE time through callback gauges, like [[WebHostMetrics]]: both are
 * a field read behind one lock, far cheaper than the JVM collectors on the same
 * scrape, and a timer would only add staleness.
 */
class WebCacheMetrics(registry: PrometheusRegistry, country: String, cache: GzippedResponseCache) {

  private def gauge(name: String, help: String, unit: Option[io.prometheus.metrics.model.snapshots.Unit], read: () => Double): Unit = {
    val builder = GaugeWithCallback.builder()
      .name(name)
      .help(help)
      .labelNames("country")
      .callback(callback => callback.call(read(), country))
    unit.foreach(builder.unit)
    builder.register(registry)
  }

  gauge(
    "kinowo_web_response_cache_held_bytes",
    "Compressed response bodies the web process is holding in heap, against the cache's byte budget.",
    Some(io.prometheus.metrics.model.snapshots.Unit.BYTES),
    () => cache.heldBytes.toDouble
  )

  gauge(
    "kinowo_web_response_cache_entries",
    "How many distinct paths the response cache is holding. Flat below the roster means nothing is " +
      "evicting; at or below it while held_bytes rides the budget means the long tail is falling out.",
    None,
    () => cache.heldEntries.toDouble
  )
}
