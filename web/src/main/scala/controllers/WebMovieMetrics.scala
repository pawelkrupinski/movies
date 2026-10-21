package controllers

import models.{City, Country}
import play.api.Logging
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

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
  // The wiring's clock, which also dates the listing these counts come from.
  clock:   Clock,
  // A poster host serving fewer of the country's films than this is counted under `other`
  // ([[WebMovieMetrics.PosterCensus]]): the long tail of single-venue sites stays one series.
  posterHostFloor: Int = WebMovieMetrics.PosterHostFloor,
) extends services.Stoppable with Logging {
  private val cities: Seq[City] = servingCountry.cities
  private val country: String   = servingCountry.code

  private val latest = new AtomicReference[Seq[WebMovieMetrics.CityCounts]](
    cities.map(c => WebMovieMetrics.CityCounts(c.slug, all = 0, tomorrow = 0))
  )
  // None until the first sample: a poster census of zero films would read as every poster lost.
  private val posters = new AtomicReference[Option[WebMovieMetrics.PosterCensus]](None)
  private val scheduler = DaemonExecutors.scheduler("web-movie-metrics")

  /** Recompute every city's served-film counts from the live read model. Cheap
   *  (the read model is in-memory), and bounded to once a minute by the
   *  scheduler regardless of how often Fly scrapes `/metrics`. */
  def sample(): Unit = {
    val perCity = cities.map(c => c -> service.toSchedules(c, new models.VenueClock(clock).nowIn(c)))
    latest.set(perCity.map { case (c, schedules) => countsFor(c, schedules) })
    posters.set(Some(WebMovieMetrics.PosterCensus.of(perCity.flatMap(_._2), posterHostFloor)))
  }

  private def countsFor(city: City, schedules: Seq[FilmSchedule]): WebMovieMetrics.CityCounts = {
    val tomorrow = new models.VenueClock(clock).nowIn(city).toLocalDate.plusDays(1)
    WebMovieMetrics.CityCounts(
      city.slug,
      all      = schedules.size,
      tomorrow = schedules.count(_.showings.exists(_._1 == tomorrow))
    )
  }

  /** Prometheus text exposition of the latest sample, appended to `/metrics`. */
  def render(): String =
    WebMovieMetrics.render(latest.get(), country) + posters.get().fold("")(_.render(country))

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

  /** Below this many of a country's films, a poster host is counted under `other`. Twenty keeps
   *  every CDN and chain that a whole-source outage would matter for (kinoheld.de ~2,000 DE films,
   *  tmdb.org, savoysystems.co.uk, cloudfront.net, kinoteka.pl …) and folds Warsaw's thirty-odd
   *  single-venue sites, so the series count stays near a dozen per country. */
  val PosterHostFloor = 20

  /** Three or more films sharing ONE poster image is a placeholder, not a poster: a site-wide
   *  og:image (Kinoteka, 2026-10-06), a "coming soon" card (Multikino `wkrotce`), a not-found image
   *  (bilety24), kinoheld's `fallback-light.jpg`. Two is still a real pairing (a film and its
   *  dubbed screening under separate cards). */
  val SharedPosterMin = 3

  private val PosterHostName = "kinowo_web_films_by_poster_host"
  private val SharedPosterName = "kinowo_web_films_sharing_poster"

  /** The country's served films — distinct across its cities — by the registrable domain their
   *  poster is drawn from ([[tools.PosterHost]]), `none` for a film served with no poster (its card
   *  renders the "no poster" placeholder), and per host the films whose poster image is shared with
   *  [[SharedPosterMin]] − 1 or more others.
   *
   *  WHY, AT THE WEB AND NOT THE WORKER: this is what the pages hand a browser, after projection and
   *  the read model's poster ordering — the layer the 2026-10-21 kinoheld outage sat right next to.
   *  It cannot see whether an image LOADS (that is the synthetic poster probe's job,
   *  synthetic-probes.nix), only whose image every card asks for. posters.rules alerts on a host's
   *  share collapsing, on the placeholder share jumping, and on a burst of shared images. */
  final case class PosterCensus(byHost: Map[String, Int], sharing: Map[String, Int]) {
    def render(country: String): String = {
      val sb = new StringBuilder
      sb.append("# HELP ").append(PosterHostName)
        .append(" The country's served films (distinct across its cities) by the registrable domain of the poster their card draws, `none` for no poster (the card's placeholder), `other` for hosts under the floor.\n")
      sb.append("# TYPE ").append(PosterHostName).append(" gauge\n")
      byHost.toSeq.sortBy(_._1).foreach { case (h, n) => line(sb, PosterHostName, country, h, n) }
      sb.append("# HELP ").append(SharedPosterName)
        .append(" The country's served films whose poster is one image three or more films share (a placeholder or a site-wide image, not a poster), by the image's registrable domain.\n")
      sb.append("# TYPE ").append(SharedPosterName).append(" gauge\n")
      sharing.toSeq.sortBy(_._1).foreach { case (h, n) => line(sb, SharedPosterName, country, h, n) }
      sb.toString
    }

    private def line(sb: StringBuilder, name: String, country: String, host: String, n: Int): Unit =
      sb.append(name).append("{country=\"").append(country).append("\",poster_host=\"").append(host)
        .append("\"} ").append(n).append('\n')
  }

  object PosterCensus {
    val NoPoster = "none"
    val Other    = "other"

    def of(schedules: Seq[FilmSchedule], floor: Int): PosterCensus = {
      // One poster per film: a film listed in five cities is one of the country's films.
      val posterByFilm: Map[String, Option[String]] =
        schedules.map(s => s.slug.getOrElse(s.movie.title) -> s.posterHref.map(_.url)).toMap
      val hostByFilm = posterByFilm.view.mapValues(_.flatMap(tools.PosterHost.family)).toMap
      val counts     = hostByFilm.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      def label(host: String): String = if (counts.getOrElse(host, 0) >= floor) host else Other
      val byHost = hostByFilm.values.groupMapReduce(_.fold(NoPoster)(label))(_ => 1)(_ + _)
      val filmsPerImage = posterByFilm.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      val sharing = posterByFilm.toSeq.collect {
        case (film, Some(url)) if filmsPerImage(url) >= SharedPosterMin => hostByFilm(film)
      }.flatten.groupMapReduce(label)(_ => 1)(_ + _)
      PosterCensus(byHost, sharing)
    }
  }

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
