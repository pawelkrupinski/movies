package controllers

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

import scala.util.Try

import models.{City, Country}
import play.api.Logging
import tools.{ThreadPools, ScheduledTick, TickRecorder}

/**
 * Samples, once a minute, how many films the web is currently serving per city
 * — in two scopes: every film with a future showing (`all`) and just those with
 * a showing tomorrow (`tomorrow`). Surfaced as Prometheus gauges through
 * [[MetricsController]] (`GET /metrics`), scraped by the fleet's Prometheus on
 * monitoring-1 and graphed/alerted on by the Grafana beside it
 * (`infra/nix/modules/roles/grafana.nix`).
 *
 * The counts come from the real serving path
 * ([[MovieControllerService.toSchedules]]), so the metric can't drift from what
 * users actually see on `/:city/`. The motivating signal is the read-model
 * outage: a malformed `web_movies` document empties the whole corpus and every
 * city silently drops to zero — invisible to host metrics. A per-city gauge
 * graphs the repertoire size and lets Grafana alert when it swings either way.
 *
 * Every city of the serving country is emitted on every sample, seeded at zero, so a
 * city that drops to zero films reads as a `0` sample rather than a vanished
 * series — the swing/floor alerts need the zero to be present, not absent.
 */
class WebMovieMetrics(
  service: MovieControllerService,
  // A web deployment serves exactly one country: its cities are the ones sampled, and
  // its code is the constant `country` label that lines the series up with the
  // worker's per-country series on the shared Grafana dashboards.
  servingCountry: Country,
  // A poster host serving fewer of the country's films than this is counted under `other`
  // ([[PosterCensus]]): the long tail of single-venue sites stays one series.
  posterHostFloor: Int = WebMovieMetrics.PosterHostFloor,
  // Where each completed sample is reported (`ScheduledTickStale`): a dead sampler serves its
  // last counts for ever, which the films-served alerts read as a steady country.
  ticks: TickRecorder = TickRecorder.Unrecorded,
  threadPools: ThreadPools
) extends tools.Stoppable with Logging {
  private val cities: Seq[City] = servingCountry.cities
  private val country: String   = servingCountry.code

  private val latest = new AtomicReference[Seq[WebMovieMetrics.CityCounts]](
    cities.map(c => WebMovieMetrics.CityCounts(c.slug, all = 0, tomorrow = 0))
  )
  // None until the first sample: a poster census of zero films would read as every poster lost.
  private val posters = new AtomicReference[Option[PosterCensus]](None)
  private val scheduler = threadPools.scheduler("web-movie-metrics")

  /** Recompute every city's served-film counts from the live read model. Cheap
   *  (the read model is in-memory), and bounded to once a minute by the
   *  scheduler regardless of how often Fly scrapes `/metrics`. */
  def sample(): Unit = {
    val perCity = cities.map(c => c -> service.toSchedules(c, service.nowIn(c)))
    latest.set(perCity.map { case (c, schedules) => countsFor(c, schedules) })
    posters.set(Some(PosterCensus.of(perCity.flatMap(_._2), posterHostFloor)))
  }

  private def countsFor(city: City, schedules: Seq[FilmSchedule]): WebMovieMetrics.CityCounts = {
    val tomorrow = service.nowIn(city).toLocalDate.plusDays(1)
    WebMovieMetrics.CityCounts(
      city.slug,
      all      = schedules.size,
      tomorrow = schedules.count(_.showsOn(tomorrow))
    )
  }

  /** Prometheus text exposition of the latest sample, appended to `/metrics`. */
  def render(): String =
    WebMovieMetrics.render(latest.get(), country) + posters.get().fold("")(_.render(country))

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"web-movie-metrics initial sample failed: ${e.getMessage}") }
    val every = scala.concurrent.duration.Duration(WebMovieMetrics.SampleSeconds, TimeUnit.SECONDS)
    ticks.started("web-movie-metrics", every, every)
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("web-movie-metrics", every, logger, ticks)(sample()),
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

  /** Below this many of a country's films, a poster host is counted under `other`. Twenty keeps
   *  every CDN and chain that a whole-source outage would matter for (kinoheld.de ~2,000 DE films,
   *  tmdb.org, savoysystems.co.uk, cloudfront.net, kinoteka.pl …) and folds Warsaw's thirty-odd
   *  single-venue sites, so the series count stays near a dozen per country. */
  val PosterHostFloor = 20

  /** Pure exposition of the per-city counts (city slugs are lowercase ascii
   *  kebab — no label escaping needed), tagged with the deployment's `country`.
   *  Cities are emitted in slug order so the output and its tests are
   *  deterministic. */
  def render(counts: Seq[CityCounts], country: String): String = {
    val sb = new StringBuilder
    sb.append("# HELP ").append(Name)
      .append(" Films the web is currently serving per country and city, by scope: all future showings, or showing tomorrow.\n")
    sb.append("# TYPE ").append(Name).append(" gauge\n")
    counts.sortBy(_.citySlug).foreach { c =>
      appendLine(sb, country, c.citySlug, "all", c.all)
      appendLine(sb, country, c.citySlug, "tomorrow", c.tomorrow)
    }
    sb.toString
  }

  private def appendLine(sb: StringBuilder, country: String, citySlug: String, scope: String, value: Int): Unit =
    sb.append(Name).append("{country=\"").append(country).append("\",city=\"").append(citySlug)
      .append("\",scope=\"").append(scope).append("\"} ").append(value).append('\n')
}
