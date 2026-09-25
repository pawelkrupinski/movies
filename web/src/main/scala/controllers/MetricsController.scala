package controllers

import play.api.mvc._
import services.UptimeMonitor
import services.UptimeMonitor.RecentTotals
import services.fallback.{FallbackState, FallbackStore}
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
 * Also appends the per-scraper-client fallback saturation gauges
 * (`kinowo_fallback_active_venues` / `kinowo_fallback_total_venues`): a cinema
 * quietly failing over to its aggregator fallback (`FallbackStore` /
 * `filmwebFallback`) is invisible the same way the residential proxy is — no
 * cinema bar goes red, only the "Aggregator fallback" section on /uptime does.
 * That was completely unwatched when all 87 UK Cineworld venues broke at once
 * on 2026-09-17 (a site relaunch 404ing the old scrape API): every venue
 * failed over to its Flicks fallback individually, so nothing paged for over a
 * day. Grouped by `client` (the SAME `shared:<Client>` marker
 * `services.cinemas.common.CinemaClientMarkers` computes and the worker pushes
 * to Mongo's `uptimeServiceTags` — read here via `UptimeMonitor.serviceTagsSnapshot()`
 * rather than re-derived) so an alert can ask "what fraction of THIS chain is
 * on fallback right now", not just "is any one venue". `custom:` (bespoke,
 * single-venue) markers are excluded: with exactly one venue their ratio is
 * always 0% or 100% and carries no "is a whole chain down" signal.
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
class MetricsController(cc: ControllerComponents, monitor: UptimeMonitor, fallbackStore: FallbackStore,
  movieMetrics: WebMovieMetrics, jvmMetrics: WebJvmMetrics, country: String,
  clock: java.time.Clock = java.time.Clock.systemUTC()) extends AbstractController(cc) {
  def metrics: Action[AnyContent] = Action {
    // Windowed AND summed by the monitor, not here. Pulling each service's full
    // `history` to sum it in the controller is what OOM-killed `web-us` on a
    // 30-second scrape loop — see `UptimeMonitor.recentTotals`.
    val totals = monitor.recentTotals(clock.millis() - MetricsController.RecentWindowMs)
    // Both already cheap, in-memory reads: `serviceTagsSnapshot()` is the
    // monitor's own polled-every-5-minutes map, and `findAll()` is
    // `FallbackStore`'s in-process mirror (see its own class doc) — neither
    // touches Mongo on the request path.
    val body = MetricsController.render(totals, country) +
      MetricsController.renderFallbackSaturation(monitor.serviceTagsSnapshot(), fallbackStore.findAll(), country) +
      movieMetrics.render() + jvmMetrics.render()
    Ok(body).as("text/plain; version=0.0.4; charset=utf-8")
  }
}

object MetricsController {
  /** Only buckets newer than this contribute to the "recent" gauges — a rolling
   *  window over every 15-min uptime bucket that overlaps the last 30 minutes
   *  (so 30 to 45 minutes of data, never less — see `UptimeMonitor.recentTotals`
   *  for the mid-outage alert flap the shorter reading caused). A since-boot total would
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

  /** Matches the `shared:<Client>` half of the tag `CinemaClientMarkers.tagsFor`
   *  writes — never `custom:<Client>` (see the class doc for why bespoke,
   *  single-venue clients are excluded here rather than filtered downstream). */
  private val SharedClientTag = "^shared:(.+)$".r

  private def sharedClientOf(tags: Set[String]): Option[String] =
    tags.collectFirst { case SharedClientTag(client) => client }

  private case class FallbackFamily(name: String, help: String, value: (Seq[String], Set[String]) => Int)
  private val FallbackFamilies = Seq(
    FallbackFamily("kinowo_fallback_active_venues",
      "Cinemas of a shared scraper client currently riding their aggregator fallback.",
      (cinemas, activeCinemas) => cinemas.count(activeCinemas.contains)),
    FallbackFamily("kinowo_fallback_total_venues",
      "Total cinemas known for a shared scraper client (the ratio's denominator).",
      (cinemas, _) => cinemas.size)
  )

  /** Render the per-shared-client fallback-saturation gauges. Pure, like
   *  [[render]] above, for the same testability reason — the controller
   *  supplies the already-polled tag snapshot and the fallback store's
   *  already-hydrated mirror.
   *
   *  `serviceTags` is `UptimeMonitor.serviceTagsSnapshot()` (cinema name ->
   *  its tags, one of which is the `shared:<Client>`/`custom:<Client>` marker
   *  `CinemaClientMarkers` computed on the worker); `fallbackStates` is
   *  `FallbackStore.findAll()`. Two families rather than one pre-divided
   *  ratio, matching `kinowo_uptime_recent_failures`/`_successes` — the ratio
   *  is computed in PromQL, where a `clamp_min` divide-by-zero guard and a
   *  minimum-venue-count floor belong beside the threshold, not baked in here. */
  def renderFallbackSaturation(serviceTags: Map[String, Set[String]], fallbackStates: Seq[FallbackState], country: String): String = {
    val activeCinemas = fallbackStates.filter(_.active).map(_.cinema).toSet
    val cinemasByClient = serviceTags.toSeq
      .flatMap { case (cinema, tags) => sharedClientOf(tags).map(_ -> cinema) }
      .groupMap(_._1)(_._2)

    val sb = new StringBuilder
    FallbackFamilies.foreach { family =>
      sb.append("# HELP ").append(family.name).append(' ').append(family.help).append('\n')
      sb.append("# TYPE ").append(family.name).append(" gauge\n")
      cinemasByClient.toSeq.sortBy(_._1).foreach { case (client, cinemas) =>
        sb.append(family.name).append("{country=\"").append(country).append("\",client=\"")
          .append(escapeLabel(client)).append("\"} ").append(family.value(cinemas, activeCinemas)).append('\n')
      }
    }
    sb.toString
  }
}
