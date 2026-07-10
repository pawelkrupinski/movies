package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.City
import play.api.Logging
import services.movies.MovieRepository
import services.readmodel.ReadModelProjection
import tools.DaemonExecutors

import java.time.{Clock, LocalDateTime}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * Per-city census of how many INDIVIDUAL SHOWTIMES (single dated slots) the source
 * `movies` collection would serve — the volume complement to [[WorkerSourceFilmsMetrics]],
 * which counts distinct FILMS per city. A city can hold a steady film count while its
 * showtime volume swings (a chain adds evening slots, a venue drops a screen), so this
 * gauge tracks the raw slot throughput the read model carries.
 *
 * The general total is `sum(kinowo_worker_showtimes)` in Grafana — the per-city series
 * sum to it exactly, since every slot belongs to exactly one city.
 *
 * Apples-to-apples with the films gauge by construction: each row runs through the REAL
 * projection's screenings path ([[ReadModelProjection.screeningsAll]] — same per-title
 * card split + cinema→city bucketing), gates on the same `readyToProject` predicate the
 * projector writes by (a film still pending TMDB enrichment contributes 0 slots, matching
 * the read model), and counts only UPCOMING slots ([[models.Showtime.isUpcoming]] in the
 * city's own zone) so retained past showings don't inflate the count.
 *
 * Sampled on its own scheduler (default every 5 min), decoupled from the Fly scrape rate,
 * scanning the cursor read-only (`foreachRecord`) so it adds no write load; between samples
 * the gauge re-reads its cached value. Mirrors [[WorkerSourceFilmsMetrics]]' shape.
 */
class WorkerShowtimesMetrics(
  repository:     MovieRepository,
  showtimes:      Gauge,
  countryCode:    String,
  clock:          Clock          = Clock.systemDefaultZone(),
  cities:         Seq[City]      = City.all,
  sampleInterval: FiniteDuration = WorkerShowtimesMetrics.DefaultSampleInterval
) extends Logging {
  import WorkerShowtimesMetrics._

  // Seed every city at 0 so a city that empties reads as an explicit 0, not a
  // vanished series — a drop-to-zero must be a sample, not an absence.
  for (c <- cities) showtimes.labelValues(countryCode, c.slug).set(0.0)

  private val scheduler = DaemonExecutors.scheduler("worker-showtimes-metrics")

  /** Scan the corpus once and publish the per-city showtime counts onto the gauge.
   *  Read-only, paged; bounded to once per `sampleInterval` regardless of scrape rate. */
  def sample(): Unit = {
    val counts = countAll(repository, cities, clock)
    for (c <- cities) showtimes.labelValues(countryCode, c.slug).set(counts.getOrElse(c.slug, 0).toDouble)
  }

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"worker-showtimes-metrics initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"worker-showtimes-metrics sample tick failed: ${e.getMessage}") },
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object WorkerShowtimesMetrics {
  /** Sibling of `kinowo_worker_movies_served` — same worker prefix, same
   *  `country`+`city` labels; a gauge (not a counter), so no `_total` suffix. */
  val Name = "kinowo_worker_showtimes"

  /** Build and register the ONE shared gauge every country's sampler writes into
   *  (leading `country` label, then `city`). Called once when the shared worker
   *  registry is built. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Upcoming individual showtimes (single dated slots) the source `movies` collection would serve per country and city, sampled every 5 min through the REAL projection path and gated on readyToProject. sum() across cities is the country total. The volume complement to kinowo_worker_movies_served (which counts distinct films).")
      .labelNames("country", "city")
      .register(registry)

  /** Once every 5 minutes, mirroring [[WorkerSourceFilmsMetrics.DefaultSampleInterval]]. */
  val DefaultSampleInterval: FiniteDuration = 5.minutes

  /** Tally, per city slug, how many upcoming showtimes the corpus would serve. Pure given
   *  a fixed `clock`, so it's unit-tested directly. Each row is projected exactly as the
   *  read model does (per-title split + cinema→city bucketing) and gated on `readyToProject`;
   *  a row that fails to project is skipped, matching the projector's per-row resilience. */
  def countAll(repository: MovieRepository, cities: Seq[City], clock: Clock): Map[String, Int] = {
    val bySlug = cities.map(c => c.slug -> c).toMap
    val acc    = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    repository.foreachRecord { stored =>
      if (stored.record.readyToProject)
        Try(ReadModelProjection.screeningsAll(stored)) match {
          case Success(cards) =>
            cards.foreach { screenings =>
              // Each CityScreening is (film, city, cinema); sum its upcoming slots into
              // the owning city. A slot belongs to exactly one city, so the per-city
              // series sum to the general total without double-counting.
              screenings.groupBy(_.city).foreach { case (citySlug, scs) =>
                bySlug.get(citySlug).foreach { city =>
                  val now      = LocalDateTime.now(clock.withZone(city.zoneId))
                  val upcoming = scs.flatMap(_.showtimes).count(_.isUpcoming(now))
                  if (upcoming > 0) acc(citySlug) += upcoming
                }
              }
            }
          case Failure(_) => () // a row that won't project simply doesn't count
        }
    }
    acc.toMap
  }
}
