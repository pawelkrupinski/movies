package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry

/** A [[LegacyAddressMetrics]] on a registry of its own, for specs that build the
 *  redirect filters and do not read what they count. */
object SpecLegacyAddressMetrics {
  def fresh(): LegacyAddressMetrics = new LegacyAddressMetrics(new PrometheusRegistry(), "pl", tools.SpecClock.Pinned)
}
