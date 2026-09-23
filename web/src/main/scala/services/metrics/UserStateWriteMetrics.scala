package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.users.UserStateWriteOutcomes
import services.users.UserStateWriteOutcomes.{Endpoint, Outcome}

/**
 * `kinowo_web_user_state_writes_total{country, endpoint, outcome}` — every
 * atomic write to a signed-in visitor's state, by the API it came through and
 * how the store took it (see [[UserStateWriteOutcomes]] for the four outcomes).
 *
 * `kinowo_web_http_requests_total` already says a route answered 503 — and
 * `UserStateWritesFailing` (web-errors.rules) alerts on exactly that — but not
 * WHY: a store that threw and a pod with no store at all are both a 503. Nor can
 * it see a `conflict`, which answers 200. Seeded at 0 so every line exists from
 * boot.
 */
class UserStateWriteMetrics(registry: PrometheusRegistry, country: String) extends UserStateWriteOutcomes {

  // The client auto-appends `_total`.
  private val writes: Counter = Counter.builder()
    .name("kinowo_web_user_state_writes")
    .help("Atomic writes to a signed-in visitor's state since boot, by country, endpoint (hide / unhide / " +
      "clear / legacy_put) and outcome (ok; conflict = raced another first write and succeeded on the retry; " +
      "store_failure = the store threw, answered 503; unavailable = no users store, answered 503).")
    .labelNames("country", "endpoint", "outcome")
    .register(registry)

  for (e <- Endpoint.all; o <- Outcome.all) writes.labelValues(country, e, o)

  def record(endpoint: String, outcome: String): Unit = writes.labelValues(country, endpoint, outcome).inc()
}
