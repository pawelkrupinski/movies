package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * A feature the worker wires only when its environment configuration is present, and the
 * keys whose absence switched it off.
 *
 * WHY THIS EXISTS. Wiring a feature as `for { key <- configuration.someKey } yield …` turns a missing
 * secret into a `None`, and a `None` into a feature that simply is not there — no error, no
 * log line, nothing on a dashboard. When the workers moved from Fly to k3s the Telegram chat
 * ids were left behind, and all three in-app alerters (Filmweb fallback, Filmweb drops,
 * staging stuck) were off for weeks with nothing noticing. Every such feature now reports
 * itself here: a gauge that reads 0 while it is off, and a WARN at boot naming the key.
 */
final case class EnvGatedFeature(name: String, missing: Seq[settings.MissingSetting]) {
  def enabled: Boolean = missing.isEmpty
}

object EnvGatedFeature {

  /** `name`, reporting the settings a resolution said were missing (none when it resolved). */
  def from(name: String, resolved: Either[Seq[settings.MissingSetting], ?]): EnvGatedFeature =
    EnvGatedFeature(name, resolved.left.getOrElse(Nil))

  /** The boot WARN for the disabled ones among `features`, or None when all are on. */
  def disabledWarning(kind: String, features: Seq[EnvGatedFeature]): Option[String] =
    Option(features.filterNot(_.enabled))
      .filter(_.nonEmpty)
      .map(_.map(f => s"$kind ${f.name} is OFF: missing ${f.missing.map(_.value).mkString(", ")}").mkString("; "))
}

/**
 * `kinowo_worker_alerter_enabled{country,alerter}` and `kinowo_worker_integration_enabled{integration}`
 * — 1 while an [[EnvGatedFeature]] is wired, 0 while a missing env var has it off. Always
 * present once recorded, so `WorkerAlerterDisabled` / `WorkerIntegrationDisabled` compare a
 * value rather than test for a sample.
 *
 * The alerters carry `country` because each country's wiring builds its own (the Filmweb
 * ones exist only where Filmweb does); the integrations read process-wide secrets, so a
 * `country` label on them would be made up — the target label names the pod.
 */
final class EnvGatedFeatureMetrics(registry: PrometheusRegistry) {

  private val alerters: Gauge = Gauge.builder()
    .name("kinowo_worker_alerter_enabled")
    .help("1 while the worker's in-app Telegram alerter is wired, 0 while a missing env var has it off " +
      "(the boot log names the key). Off in production means an incident class nobody is told about.")
    .labelNames("country", "alerter")
    .register(registry)

  private val integrations: Gauge = Gauge.builder()
    .name("kinowo_worker_integration_enabled")
    .help("1 while the worker's external integration is wired, 0 while a missing secret has it off " +
      "(the boot log names the key): TMDB, OMDb, the residential proxy, Zyte, Sentry, the Facebook re-scrape.")
    .labelNames("integration")
    .register(registry)

  private def flag(feature: EnvGatedFeature): Double = if (feature.enabled) 1.0 else 0.0

  def recordAlerters(country: String, features: Seq[EnvGatedFeature]): Unit =
    features.foreach(f => alerters.labelValues(country, f.name).set(flag(f)))

  def recordIntegrations(features: Seq[EnvGatedFeature]): Unit =
    features.foreach(f => integrations.labelValues(f.name).set(flag(f)))
}
