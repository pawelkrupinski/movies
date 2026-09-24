package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.users.UserStateIndexHealth

/**
 * `kinowo_web_user_state_userid_index_unique{country}` — 1 when this pod's
 * `userStates` store found (or built) its unique `userId` index at boot, 0 when
 * the build failed; absent until the store first reaches its collection. See
 * [[UserStateIndexHealth]] for why a missing index matters, and
 * `UserStateUniqueIndexMissing` (web-errors.rules) for the alert on 0.
 */
class UserStateIndexMetrics(registry: PrometheusRegistry, country: String) extends UserStateIndexHealth {

  private val unique: Gauge = Gauge.builder()
    .name("kinowo_web_user_state_userid_index_unique")
    .help("1 when the userStates collection has its unique userId index (checked once per boot), 0 when " +
      "building it failed — usually duplicate rows for one userId.")
    .labelNames("country")
    .register(registry)

  def uniqueUserIdIndex(present: Boolean): Unit = unique.labelValues(country).set(if (present) 1 else 0)
}
