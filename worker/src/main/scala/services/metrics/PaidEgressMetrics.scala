package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.{HttpOutcome, HttpOutcomeRecorder}

/**
 * `kinowo_worker_paid_egress_total` — one count per request the worker sends
 * through a PAID egress provider (Zyte per request, Decodo per plan), labelled
 * by `country`, `provider` and `outcome` ([[HttpOutcome]]).
 *
 * WHY IT EXISTS: Odeon's Zyte fallback sent every paid request without its
 * `Authorization` header, so every one came back 401 — paid for, useless, and
 * counted nowhere. The Zyte leg had no meter at all, and the Decodo leg's only
 * meter was the /uptime "Residential proxy" row, which pools every venue into
 * one success ratio a single always-failing client can hide behind. This is the
 * series `PaidEgressFailing` (residential-proxy.rules) divides.
 *
 * `kinowo_worker_http_total` does not answer this: it is wired innermost in the
 * DIRECT chain, and neither provider's requests pass through it.
 *
 * Fed by a [[tools.CountingHttpFetch]] around each provider's leg (see
 * `EgressWiring` and `ZyteFallback`), so each attempt that actually reached the
 * provider counts once — a circuit-breaker fast-fail, which sends nothing, does
 * not. Seeded to 0 over the whole grid so the ratio has a denominator from boot.
 */
class PaidEgressMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`.
  private val requests: Counter = Counter.builder()
    .name("kinowo_worker_paid_egress")
    .help("Requests the worker sent through a paid egress provider since boot, by country, provider " +
      "(zyte = per-request residential API; decodo = static-ISP residential proxy) and outcome " +
      "(success / http_401 / http_403 / … / timeout / connection_error / other). A circuit-breaker " +
      "fast-fail sends nothing and is not counted.")
    .labelNames("country", "provider", "outcome")
    .register(registry)

  for (c <- countryCodes; p <- PaidEgressMetrics.Provider.all; o <- HttpOutcome.all)
    requests.labelValues(c, p, o)

  /** The recorder one country's `provider` leg is wrapped with. */
  def recorderFor(country: String, provider: String): HttpOutcomeRecorder =
    (outcome: String) => requests.labelValues(country, provider, outcome).inc()
}

object PaidEgressMetrics {
  /** The `provider` label values — a closed set, so the seed grid, the alert
   *  and the panel agree on the exact strings. */
  object Provider {
    val Zyte   = "zyte"
    val Decodo = "decodo"
    val all: Seq[String] = Seq(Zyte, Decodo)
  }
}
