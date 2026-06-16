package controllers

import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor.BucketSnapshot

/**
 * Prometheus exposition endpoint (`GET /metrics`), scraped by Fly's managed
 * Prometheus via the `[[metrics]]` block in fly.toml and charted/alerted on by
 * the self-hosted Grafana (`fly/grafana`).
 *
 * The in-app /uptime health lives in Mongo (`uptimeBuckets`), invisible to
 * Fly's host-only metrics — so a service that fails silently never alerts. The
 * residential proxy is the canonical case: when it rolls every request to the
 * Zyte fallback, every cinema bar stays green and only the "Residential proxy"
 * row goes red, which nobody is watching (and the Zyte bill quietly climbs).
 * Surfacing the recent per-service success/failure counts as gauges lets
 * Grafana alert on it like any host metric.
 */
class MetricsController(cc: ControllerComponents, monitor: UptimeMonitor) extends AbstractController(cc) {
  def metrics: Action[AnyContent] = Action {
    val snapshots = monitor.services.iterator.map(service => service -> monitor.history(service)).toMap
    Ok(MetricsController.render(snapshots, System.currentTimeMillis()))
      .as("text/plain; version=0.0.4; charset=utf-8")
  }
}

object MetricsController {
  /** Only buckets newer than this contribute to the "recent" gauges — a rolling
   *  window over the last two 15-min uptime buckets. A since-boot total would
   *  never recover after an incident; this reflects *current* health, so the
   *  ratio failures/(failures+successes) reads ~1.0 only while a service is
   *  actively failing and falls back to 0 once it recovers. */
  val RecentWindowMs: Long = 30 * 60 * 1000L

  private case class Family(name: String, help: String, value: BucketSnapshot => Int)
  private val Families = Seq(
    Family("kinowo_uptime_recent_successes", "Successful uptime checks per service in the last 30 minutes.", _.successes),
    Family("kinowo_uptime_recent_failures", "Failed uptime checks per service in the last 30 minutes.", _.failures),
    Family("kinowo_uptime_recent_zeroes", "Parsed-but-empty uptime checks per service in the last 30 minutes.", _.zeroes)
  )

  /** Render the Prometheus text exposition (version 0.0.4) of every service's
   *  recent health. Pure — the controller supplies the `monitor.history`
   *  snapshots and the clock — so it's unit-tested without an HTTP round-trip.
   *  Services are emitted in name order so the output (and its tests) are
   *  deterministic. */
  def render(snapshotsByService: Map[String, Seq[BucketSnapshot]], nowMs: Long): String = {
    val cutoff = nowMs - RecentWindowMs
    val recent = snapshotsByService.view
      .mapValues(_.filter(_.timestamp >= cutoff))
      .toSeq
      .sortBy(_._1)
    val sb = new StringBuilder
    Families.foreach { family =>
      sb.append("# HELP ").append(family.name).append(' ').append(family.help).append('\n')
      sb.append("# TYPE ").append(family.name).append(" gauge\n")
      recent.foreach { case (service, buckets) =>
        val total = buckets.foldLeft(0)((acc, bucket) => acc + family.value(bucket))
        sb.append(family.name).append("{service=\"").append(escapeLabel(service)).append("\"} ").append(total).append('\n')
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
