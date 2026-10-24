package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import tools.TickRecorder

import java.time.Clock
import scala.concurrent.duration.FiniteDuration

/**
 * Every periodic loop's pulse, on one tier's registry ([[ScheduledTickMetrics.worker]] or
 * [[ScheduledTickMetrics.web]]; each family is spelt whole there, so the metric-coverage scans can
 * see it):
 *
 *  - `kinowo_<tier>_scheduled_tick_last_success_timestamp_seconds{country,tick}` — when the loop
 *    last completed a run. SEEDED WITH ITS FIRST RUN'S DUE TIME when the schedule is armed, so a
 *    loop that never completes a run goes stale from when it should have, instead of publishing
 *    nothing -- and a long initial delay is not mistaken for a stall;
 *  - `kinowo_<tier>_scheduled_tick_interval_seconds{country,tick}` — how often it means to run.
 *
 * `ScheduledTickStale` (infra/nix/files/monitoring/rules/scheduled-ticks.rules) compares the two,
 * which is what replaced per-loop Sentry cron check-ins: one rule over every loop, each held to a
 * few of its own periods.
 */
final class ScheduledTickMetrics private (registry: PrometheusRegistry, lastSuccessName: String, intervalName: String) {
  private val lastSuccess: Gauge = Gauge.builder()
    .name(lastSuccessName)
    .help("When this periodic loop last completed a run (epoch seconds); seeded with its first run's due time.")
    .labelNames("country", "tick")
    .register(registry)

  private val interval: Gauge = Gauge.builder()
    .name(intervalName)
    .help("How often this periodic loop is scheduled to run (seconds).")
    .labelNames("country", "tick")
    .register(registry)

  def forCountry(country: String, clock: Clock): TickRecorder = new TickRecorder {
    def started(tick: String, every: FiniteDuration, firstRunIn: FiniteDuration): Unit = stamp(tick, every, firstRunIn)
    def completed(tick: String, every: FiniteDuration): Unit = stamp(tick, every, scala.concurrent.duration.Duration.Zero)

    private def stamp(tick: String, every: FiniteDuration, ahead: FiniteDuration): Unit = {
      interval.labelValues(country, tick).set(every.toMillis / 1000.0)
      lastSuccess.labelValues(country, tick).set((clock.millis() + ahead.toMillis) / 1000.0)
    }
  }
}

object ScheduledTickMetrics {
  def worker(registry: PrometheusRegistry): ScheduledTickMetrics = new ScheduledTickMetrics(registry,
    "kinowo_worker_scheduled_tick_last_success_timestamp_seconds", "kinowo_worker_scheduled_tick_interval_seconds")

  def web(registry: PrometheusRegistry): ScheduledTickMetrics = new ScheduledTickMetrics(registry,
    "kinowo_web_scheduled_tick_last_success_timestamp_seconds", "kinowo_web_scheduled_tick_interval_seconds")
}
