package controllers

import models.City
import play.api.Logging
import tools.DaemonExecutors

import java.time.{Clock, LocalDateTime}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

/**
 * Samples, once a minute, how many films the web is currently serving per city
 * — in two scopes: every film with a future showing (`all`) and just those with
 * a showing tomorrow (`tomorrow`). Surfaced as Prometheus gauges through
 * [[MetricsController]] (`GET /metrics`), scraped by Fly's managed Prometheus
 * and graphed/alerted on by the self-hosted Grafana (`fly/grafana`).
 *
 * The counts come from the real serving path
 * ([[MovieControllerService.toSchedules]]), so the metric can't drift from what
 * users actually see on `/:city/`. The motivating signal is the read-model
 * outage: a malformed `web_movies` document empties the whole corpus and every
 * city silently drops to zero — invisible to host metrics. A per-city gauge
 * graphs the repertoire size and lets Grafana alert when it swings either way.
 *
 * Every city in [[City.all]] is emitted on every sample, seeded at zero, so a
 * city that drops to zero films reads as a `0` sample rather than a vanished
 * series — the swing/floor alerts need the zero to be present, not absent.
 */
class WebMovieMetrics(
  service: MovieControllerService,
  cities:  Seq[City]  = City.all,
  clock:   Clock      = Clock.systemDefaultZone()
) extends Logging {

  private val latest = new AtomicReference[Seq[WebMovieMetrics.CityCounts]](
    cities.map(c => WebMovieMetrics.CityCounts(c.slug, all = 0, tomorrow = 0))
  )
  private val scheduler = DaemonExecutors.scheduler("web-movie-metrics")

  /** Recompute every city's served-film counts from the live read model. Cheap
   *  (the read model is in-memory), and bounded to once a minute by the
   *  scheduler regardless of how often Fly scrapes `/metrics`. */
  def sample(): Unit = latest.set(cities.map(countsFor))

  private def countsFor(city: City): WebMovieMetrics.CityCounts = {
    val now       = LocalDateTime.now(clock.withZone(city.zoneId))
    val tomorrow  = now.toLocalDate.plusDays(1)
    val schedules = service.toSchedules(city, now)
    WebMovieMetrics.CityCounts(
      city.slug,
      all      = schedules.size,
      tomorrow = schedules.count(_.showings.exists(_._1 == tomorrow))
    )
  }

  /** Prometheus text exposition of the latest sample, appended to `/metrics`. */
  def render(): String = WebMovieMetrics.render(latest.get())

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"web-movie-metrics initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"web-movie-metrics sample tick failed: ${e.getMessage}") },
      WebMovieMetrics.SampleSeconds, WebMovieMetrics.SampleSeconds, TimeUnit.SECONDS)
  }

  def stop(): Unit = scheduler.shutdown()
}

object WebMovieMetrics {
  /** Per-minute sampling, per the metric's contract. Fly may scrape faster; it
   *  just re-reads the same cached sample until the next tick. */
  val SampleSeconds: Long = 60

  private val Name = "kinowo_web_movies_served"

  case class CityCounts(citySlug: String, all: Int, tomorrow: Int)

  /** Pure exposition of the per-city counts (city slugs are lowercase ascii
   *  kebab — no label escaping needed). Cities are emitted in slug order so the
   *  output and its tests are deterministic. */
  def render(counts: Seq[CityCounts]): String = {
    val sb = new StringBuilder
    sb.append("# HELP ").append(Name)
      .append(" Films the web is currently serving per city, by scope: all future showings, or showing tomorrow.\n")
    sb.append("# TYPE ").append(Name).append(" gauge\n")
    counts.sortBy(_.citySlug).foreach { c =>
      appendLine(sb, c.citySlug, "all", c.all)
      appendLine(sb, c.citySlug, "tomorrow", c.tomorrow)
    }
    sb.toString
  }

  private def appendLine(sb: StringBuilder, citySlug: String, scope: String, value: Int): Unit =
    sb.append(Name).append("{city=\"").append(citySlug).append("\",scope=\"").append(scope).append("\"} ")
      .append(value).append('\n')
}
