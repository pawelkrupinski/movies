package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCacheReader}
import services.tasks.{RatingSources, RatingTasks}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * Surfaces the rating runs that HAVEN'T happened — the gap the first-attempt
 * histogram can't show. [[services.tasks.RatingHandler]]'s
 * `kinowo_worker_rating_first_attempt_delay_seconds` only times runs that DID
 * happen, so a TMDB-resolved film whose RT/MC has no match, or whose source is
 * starved by the enqueue cap, simply never produces a sample — invisible on the
 * panel.
 *
 * For each rating site (imdb/fw/rt/mc) this exposes, over all resolved films the
 * site is eligible for (same [[RatingSources]] eligibility the enqueuer uses):
 *   - `kinowo_worker_rating_resolved_not_run{site}` — how many have never had this
 *     rating run (no freshness stamp), and
 *   - `kinowo_worker_rating_resolved_not_run_oldest_age_seconds{site}` — how long
 *     the oldest such film has waited since it resolved.
 * A site that never runs keeps a non-zero backlog and an ever-climbing oldest
 * age, so Grafana can alert on it.
 *
 * Sampled off-band (default every 5 min) by walking the in-memory cache and
 * point-looking-up the freshness mirror (both in-memory — no Mongo round-trips),
 * published onto the same registry/endpoint as [[WorkerTaskMetrics]]. Mirrors the
 * sample-and-cache shape of [[WorkerCorpusMetrics]].
 */
class RatingRunCensus(
  cache:          MovieCacheReader,
  freshness:      FreshnessStore,
  registry:       PrometheusRegistry,
  clock:          Clock = Clock.systemUTC(),
  sampleInterval: FiniteDuration = RatingRunCensus.DefaultSampleInterval
) extends Logging {
  import RatingRunCensus._

  private val notRun = Gauge.builder()
    .name(NotRunName)
    .help("TMDB-resolved films eligible for this rating site whose first run hasn't happened yet (no freshness stamp), by site. A site that never runs keeps a non-zero backlog here.")
    .labelNames("site")
    .register(registry)

  private val oldestAge = Gauge.builder()
    .name(OldestAgeName)
    .help("Seconds the OLDEST resolved-but-never-run film has waited since its TMDB resolution, by rating site — the never-run latency the first-attempt histogram can't show (it only times runs that happened). Climbs without bound for a site that never runs.")
    .labelNames("site")
    .register(registry)

  // Materialize each site at 0 so every series exists from boot — no Grafana gaps.
  RatingSources.all.foreach { s =>
    notRun.labelValues(s.kind.label).set(0.0)
    oldestAge.labelValues(s.kind.label).set(0.0)
  }

  private val scheduler = DaemonExecutors.scheduler("rating-run-census")

  /** Walk the corpus once and publish each site's never-run backlog onto the
   *  gauges. Read-only, in-memory; bounded to once per `sampleInterval`. */
  def sample(): Unit = {
    val stats = census(cache.entries, freshness.lastFetchedAt, clock.instant())
    RatingSources.all.foreach { s =>
      val st = stats.getOrElse(s.kind.label, SiteBacklog.empty)
      notRun.labelValues(s.kind.label).set(st.count.toDouble)
      oldestAge.labelValues(s.kind.label).set(st.oldestAgeSeconds)
    }
  }

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"rating-run-census initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"rating-run-census sample tick failed: ${e.getMessage}") },
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object RatingRunCensus {
  val NotRunName    = "kinowo_worker_rating_resolved_not_run"
  val OldestAgeName = "kinowo_worker_rating_resolved_not_run_oldest_age_seconds"

  /** Once every 5 minutes — the backlog drains on the order of the enrichment
   *  reaper's minute-cadence trickle, far slower than the seconds-apart scrape. */
  val DefaultSampleInterval: FiniteDuration = 5.minutes

  case class SiteBacklog(count: Int, oldestAgeSeconds: Double)
  object SiteBacklog { val empty: SiteBacklog = SiteBacklog(0, 0.0) }

  /** Pure census: per site label, how many eligible rows have no rating stamp
   *  (the run never happened), and the oldest `now − tmdbResolvedAt` among those
   *  whose resolve time is known. `lastFetchedAt` is the freshness lookup (the
   *  cache mirror in prod); `now` is the sample instant. */
  private[services] def census(
    entries:       Iterable[(CacheKey, MovieRecord)],
    lastFetchedAt: String => Option[Instant],
    now:           Instant
  ): Map[String, SiteBacklog] = {
    val acc = scala.collection.mutable.Map.empty[String, SiteBacklog]
    entries.foreach { case (key, record) =>
      RatingSources.all.foreach { s =>
        if (s.eligible(record) && !hasRun(lastFetchedAt, s.kind, key, record.tmdbId)) {
          val ageSeconds = record.tmdbId
            .flatMap(id => lastFetchedAt(RatingTasks.tmdbResolvedAtKey(id)))
            .map(resolvedAt => math.max(0.0, (now.toEpochMilli - resolvedAt.toEpochMilli).toDouble / 1000.0))
            .getOrElse(0.0)
          val prev = acc.getOrElse(s.kind.label, SiteBacklog.empty)
          acc(s.kind.label) = SiteBacklog(prev.count + 1, math.max(prev.oldestAgeSeconds, ageSeconds))
        }
      }
    }
    acc.toMap
  }

  /** A site has run for a row iff a freshness stamp exists under its tmdbId-keyed
   *  dedup key (or the legacy title key — mirrors [[services.tasks.RatingEnqueuer]]'s
   *  fallback so the two agree on "already run"). */
  private def hasRun(lastFetchedAt: String => Option[Instant], kind: FreshnessKind, key: CacheKey, tmdbId: Option[Int]): Boolean =
    lastFetchedAt(RatingTasks.dedupKey(kind, key, tmdbId)).isDefined ||
    lastFetchedAt(RatingTasks.dedupKey(kind, key)).isDefined
}
