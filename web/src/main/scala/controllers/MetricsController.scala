package controllers

import models.Country
import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor.RecentTotals
import services.metrics.WebJvmMetrics

/**
 * Prometheus exposition endpoint (`GET /metrics`), scraped over the pod's
 * NodePort by the fleet's Prometheus (`infra/nix/files/monitoring/scrape-kinowo-apps.yaml`)
 * and charted/alerted on by the Grafana beside it.
 *
 * The in-app /uptime health lives in Mongo (`uptimeBuckets`), invisible to
 * Fly's host-only metrics — so a service that fails silently never alerts. The
 * residential proxy is the canonical case: when it rolls every request to the
 * Zyte fallback, every cinema bar stays green and only the "Residential proxy"
 * row goes red, which nobody is watching (and the Zyte bill quietly climbs).
 * Surfacing the recent per-service success/failure counts as gauges lets
 * Grafana alert on it like any host metric.
 *
 * Also appends [[WebMovieMetrics]] — the per-city count of films the web is
 * actually serving (all future / showing tomorrow) — so Grafana can alert when
 * a city's repertoire suddenly swings (the read-model-outage signal: a city
 * silently dropping to zero).
 *
 * Finally appends [[WebJvmMetrics]] — the process-wide Prometheus registry. That
 * carries the standard `jvm_*` / `process_*` resource collectors, matching what
 * the worker already exports (so the Fly-health dashboard can chart the web
 * JVM's heap against its `-Xmx384m` rather than inferring it from the machine's
 * free RAM); the `kinowo_web_http_*` request rate / latency families that
 * [[modules.HttpMetricsFilter]] records; and the `kinowo_web_host_*` gauges
 * ([[services.metrics.WebHostMetrics]]) reporting the machine's free RAM and
 * free disk. All three replace series that died with Fly's managed-Prometheus
 * token — `fly_app_http_*`, `fly_instance_memory_*`, `fly_volume_*` — and are
 * now the tier's ONLY signal for request rate, latency, and how close the box
 * is to full.
 */
class MetricsController(cc: ControllerComponents, monitor: UptimeMonitor, movieMetrics: WebMovieMetrics,
  jvmMetrics: WebJvmMetrics, country: String = Country.default.code) extends AbstractController(cc) {
  def metrics: Action[AnyContent] = Action {
    // Windowed AND summed by the monitor, not here. Pulling each service's full
    // `history` to sum it in the controller is what OOM-killed `web-us` on a
    // 30-second scrape loop — see `UptimeMonitor.recentTotals`.
    val totals = monitor.recentTotals(System.currentTimeMillis() - MetricsController.RecentWindowMs)
    val body = MetricsController.render(totals, country) +
      movieMetrics.render() + jvmMetrics.render()
    Ok(body).as("text/plain; version=0.0.4; charset=utf-8")
  }
}

object MetricsController {
  /** Only buckets newer than this contribute to the "recent" gauges — a rolling
   *  window over the last two 15-min uptime buckets. A since-boot total would
   *  never recover after an incident; this reflects *current* health, so the
   *  ratio failures/(failures+successes) reads ~1.0 only while a service is
   *  actively failing and falls back to 0 once it recovers. */
  val RecentWindowMs: Long = 30 * 60 * 1000L

  private case class Family(name: String, help: String, value: RecentTotals => Int)
  private val Families = Seq(
    Family("kinowo_uptime_recent_successes", "Successful uptime checks per service in the last 30 minutes.", _.successes),
    Family("kinowo_uptime_recent_failures", "Failed uptime checks per service in the last 30 minutes.", _.failures),
    Family("kinowo_uptime_recent_zeroes", "Parsed-but-empty uptime checks per service in the last 30 minutes.", _.zeroes)
  )

  /** Render the Prometheus text exposition (version 0.0.4) of every service's
   *  recent health. Pure — the controller supplies the already-windowed per-service
   *  totals — so it's unit-tested without an HTTP round-trip. Services are emitted
   *  in name order so the output (and its tests) are deterministic.
   *
   *  It takes TOTALS rather than buckets on purpose: with one service per venue the
   *  US roster is 5,031 rows, and handing this the raw slots to sum meant
   *  materialising them all on every 30-second scrape. */
  def render(totalsByService: Seq[(String, RecentTotals)], country: String): String = {
    val recent = totalsByService.sortBy(_._1)
    val sb = new StringBuilder
    Families.foreach { family =>
      sb.append("# HELP ").append(family.name).append(' ').append(family.help).append('\n')
      sb.append("# TYPE ").append(family.name).append(" gauge\n")
      recent.foreach { case (service, totals) =>
        sb.append(family.name).append("{country=\"").append(country).append("\",service=\"")
          .append(escapeLabel(service)).append("\"} ").append(family.value(totals)).append('\n')
      }
    }
    sb.toString
  }

  /** Escape a label value per the Prometheus text format: backslash, double
   *  quote and newline. Service names are human strings ("Residential proxy",
   *  "img: www.multikino.pl"), so this is defensive but cheap. */
  private def escapeLabel(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")
}
