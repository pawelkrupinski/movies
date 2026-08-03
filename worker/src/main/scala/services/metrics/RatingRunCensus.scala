package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Country, MovieRecord}
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCacheReader}
import services.tasks.RatingSources.RatingSource
import services.tasks.{RatingSources, RatingTasks}

import java.time.{Clock, Instant}
import scala.concurrent.duration._

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
 *     the oldest such film has waited, since it resolved on TMDB or, for a row
 *     with no tmdbId to measure that from, since this census first saw it stuck.
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
  notRun:         Gauge,
  oldestAge:      Gauge,
  country:        Country,
  clock:          Clock = Clock.systemUTC(),
  override protected val sampleInterval: FiniteDuration = RatingRunCensus.DefaultSampleInterval
) extends SampledCensus {
  import RatingRunCensus._

  private val countryCode = country.code
  // Only the sources this country actually wires a handler for (Filmweb is dropped
  // outside Poland) — so the census never seeds/reports a backlog for a rating that
  // can never run here (see RatingSources.forCountry).
  private val sources = RatingSources.forCountry(country)

  // Materialize each site at 0 so this country's series exist from boot — no Grafana gaps.
  sources.foreach { s =>
    notRun.labelValues(countryCode, s.kind.label).set(0.0)
    oldestAge.labelValues(countryCode, s.kind.label).set(0.0)
  }


  // When this census first saw each (site, row) in the backlog — the fallback
  // clock start for a row whose TMDB-resolve stamp can't exist. IMDb eligibility
  // is imdbId-based, not tmdbId-based (see [[RatingSources]]), so an imdbId-only
  // row joins the backlog with nothing tmdbId-keyed to measure from; the age then
  // collapsed to 0 and stayed there, leaving a non-zero count sitting under a flat
  // line on the very panel that exists to show the wait climbing (the UK carried
  // two such films for six hours on 2026-07-28). Only rows still in the backlog
  // are carried forward, so a drained row forgets its sighting and the map can't
  // grow without bound. Volatile: written by the scheduler thread, and by the
  // caller's thread for the initial `start()` sample.
  @volatile private var firstSeen: Map[(String, CacheKey), Instant] = Map.empty

  /** Walk the corpus once and publish each site's never-run backlog onto the
   *  gauges. Read-only, in-memory; bounded to once per `sampleInterval`. */
  def sample(): Unit = {
    val now  = clock.instant()
    val seen = scala.collection.mutable.Map.empty[(String, CacheKey), Instant]
    val sighting: (String, CacheKey) => Instant = (site, key) => {
      val at = firstSeen.getOrElse((site, key), now)
      seen((site, key)) = at
      at
    }
    val stats = census(cache.entries, freshness.lastFetchedAt, now, sources, Some(sighting))
    firstSeen = seen.toMap
    sources.foreach { s =>
      val st = stats.getOrElse(s.kind.label, SiteBacklog.empty)
      notRun.labelValues(countryCode, s.kind.label).set(st.count.toDouble)
      oldestAge.labelValues(countryCode, s.kind.label).set(st.oldestAgeSeconds)
    }
  }

  override protected val censusName: String = "rating-run-census"
}

object RatingRunCensus {
  val NotRunName    = "kinowo_worker_rating_resolved_not_run"
  val OldestAgeName = "kinowo_worker_rating_resolved_not_run_oldest_age_seconds"

  /** Build and register the TWO shared gauges every country's census writes into
   *  (leading `country` label, then `site`) — the never-run backlog count and the
   *  oldest-waiting age. Called once when the shared worker registry is built. */
  def gauges(registry: PrometheusRegistry): (Gauge, Gauge) = {
    val notRun = Gauge.builder()
      .name(NotRunName)
      .help("TMDB-resolved films eligible for this rating site whose first run hasn't happened yet (no freshness stamp), by country and site. A site that never runs keeps a non-zero backlog here.")
      .labelNames("country", "site")
      .register(registry)
    val oldestAge = Gauge.builder()
      .name(OldestAgeName)
      .help("Seconds the OLDEST resolved-but-never-run film has waited, by country and rating site — the never-run latency the first-attempt histogram can't show (it only times runs that happened). Measured from the film's TMDB resolution, or from when this worker first saw the row stuck when it has no tmdbId to date that from (IMDb is eligible on an imdbId alone), so the line climbs for every backlog rather than only the tmdbId-keyed ones. The no-tmdbId fallback restarts at 0 after a worker restart.")
      .labelNames("country", "site")
      .register(registry)
    (notRun, oldestAge)
  }

  /** Once every 5 minutes — the backlog drains on the order of the enrichment
   *  reaper's minute-cadence trickle, far slower than the seconds-apart scrape. */
  val DefaultSampleInterval: FiniteDuration = 5.minutes

  case class SiteBacklog(count: Int, oldestAgeSeconds: Double)
  object SiteBacklog { val empty: SiteBacklog = SiteBacklog(0, 0.0) }

  /** Pure census: per site label, how many eligible rows have no rating stamp
   *  (the run never happened), and the oldest wait among them. `lastFetchedAt` is
   *  the freshness lookup (the cache mirror in prod); `now` is the sample instant.
   *  `sources` are the rating sources to census — the caller passes only those its
   *  country wires a handler for (defaults to all).
   *
   *  A row's wait is measured from its TMDB resolution when that's knowable, and
   *  otherwise from `firstBacklogSighting` — when the caller first observed this
   *  (site, row) in the backlog. The fallback is what makes the gauge honest for a
   *  row eligible WITHOUT a tmdbId (IMDb needs only an imdbId), which has no
   *  tmdbId-keyed resolve stamp to measure from and previously reported a wait of
   *  exactly 0 no matter how long it sat. It's the weaker of the two — the caller's
   *  memory resets on a worker restart, where the stamp is durable — so it's only
   *  ever the fallback. `None` ("this caller keeps no memory of sightings") dates
   *  such a row from `now`, i.e. a wait of 0. */
  private[services] def census(
    entries:              Iterable[(CacheKey, MovieRecord)],
    lastFetchedAt:        String => Option[Instant],
    now:                  Instant,
    sources:              Seq[RatingSource] = RatingSources.all,
    firstBacklogSighting: Option[(String, CacheKey) => Instant] = None
  ): Map[String, SiteBacklog] = {
    val acc = scala.collection.mutable.Map.empty[String, SiteBacklog]
    entries.foreach { case (key, record) =>
      sources.foreach { s =>
        if (s.eligible(record) && !hasRun(lastFetchedAt, s.kind, key, record.tmdbId)) {
          val waitingSince = record.tmdbId
            .flatMap(id => lastFetchedAt(RatingTasks.tmdbResolvedAtKey(id)))
            .orElse(firstBacklogSighting.map(_(s.kind.label, key)))
            .getOrElse(now)
          val ageSeconds = math.max(0.0, (now.toEpochMilli - waitingSince.toEpochMilli).toDouble / 1000.0)
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
