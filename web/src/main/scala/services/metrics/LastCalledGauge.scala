package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap

/**
 * A retirement signal's LAST-CALLED gauge: the Unix time each label combination was last
 * stamped, read through `max_over_time(…[30d])` across restarts — a timestamp rather than a
 * counter because `increase()` drops the first increment of a series born after a restart,
 * which is exactly the lone late call such a signal exists to see. A series is absent until
 * its first stamp.
 *
 * `country` is every series' first label; `labelNames` the rest, stamped in that order.
 * Shared by [[WebRouteUsageMetrics]] and [[LegacyAddressMetrics]].
 */
final class LastCalledGauge(registry: PrometheusRegistry, name: String, help: String, country: String, clock: Clock,
                            labelNames: String*) {

  private val lastCalled = new ConcurrentHashMap[Seq[String], Instant]()

  GaugeWithCallback.builder()
    .name(name)
    .help(help)
    .labelNames(("country" +: labelNames)*)
    .callback { callback =>
      lastCalled.forEach((labels, instant) => callback.call(instant.getEpochSecond.toDouble, (country +: labels)*))
    }
    .register(registry)

  /** Stamp `labels` (one value per label name after `country`) as called now. */
  def stamp(labels: String*): Unit = {
    require(labels.size == labelNames.size, s"$name takes ${labelNames.size} labels besides country, got ${labels.size}")
    lastCalled.put(labels.toVector, clock.instant())
    ()
  }
}
