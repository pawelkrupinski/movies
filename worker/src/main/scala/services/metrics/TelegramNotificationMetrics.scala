package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.alerts.{TelegramAlertKind, TelegramNotificationRecorder, TelegramOutcome}

/**
 * `kinowo_worker_telegram_notifications_total` — one count per in-app Telegram
 * alert the worker tried to send, labelled by `country`, `kind`
 * ([[TelegramAlertKind]]: fallback_enter, gone_venue, filmweb_drop, …) and
 * `outcome` (sent / failed).
 *
 * WHY: a delivered page left no trace at all, and a failed one only a WARN, so
 * neither "how many venues did we page about this week" nor "has the bot been
 * failing since its token was rotated" had an answer. `WorkerTelegramAlertsFailing`
 * (worker-pipeline.rules) reads the failed half.
 *
 * Registered once on the shared [[WorkerMetrics]] registry and seeded to 0 over
 * country × known kind × outcome, so `increase` sees each kind's first attempt.
 */
class TelegramNotificationMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total`.
  private val notifications: Counter = Counter.builder()
    .name("kinowo_worker_telegram_notifications")
    .help("In-app Telegram alerts the worker tried to send since boot, by country, kind (fallback_enter / " +
      "fallback_recovered / fallback_uncovered / gone_venue / venue_closure / filmweb_drop / staging_stuck) " +
      "and outcome (sent / failed).")
    .labelNames("country", "kind", "outcome")
    .register(registry)

  for (c <- countryCodes; k <- TelegramAlertKind.all; o <- TelegramOutcome.all)
    notifications.labelValues(c, k.value, o)

  /** The recorder one country's notifiers count into. */
  def recorderFor(country: String): TelegramNotificationRecorder =
    (kind, outcome) => notifications.labelValues(country, kind.value, outcome).inc()
}
