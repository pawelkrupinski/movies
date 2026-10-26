package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, Country}
import services.cinemas.common.CinemaScraper
import services.freshness.FreshnessStore
import services.tasks.{DueWindow, ScrapeCinemaHandler}

import java.time.{Clock, Instant}
import scala.concurrent.duration._

/**
 * Surfaces how STALE a country's most-neglected cinema is — the age of the
 * oldest last-successful scrape across the whole roster.
 *
 * The existing scrape signals are all about the scrapes that DID happen:
 * `kinowo_worker_http_total{phase="scrape"}` counts attempts and outcomes, and
 * the task pipeline times the handlers that ran. None of them can see a cinema
 * that has quietly stopped being scraped — it simply contributes no samples, and
 * a roster where one venue has been dark for two days looks exactly like a
 * healthy one. That gap is what the [[ScrapeReaper]]'s most-overdue-first ordering
 * exists to fight, and this is the metric that says whether it is winning.
 *
 * For each country this exposes:
 *   - `kinowo_worker_cinema_scrape_oldest_age_seconds{country}` — seconds since
 *     the LEAST recently scraped cinema last scraped successfully, and
 *   - `kinowo_worker_cinema_scrape_never_scraped{country}` — how many cinemas
 *     have no successful scrape at all,
 *   - `kinowo_worker_cinema_scrape_age_quantile_seconds{country,quantile}` — the
 *     median (`0.5`) and p90 (`0.9`) of those same ages, and
 *   - `kinowo_worker_cinema_scrape_due_venues{country,mode}` — how many venues
 *     the scrape [[DueWindow]] calls due right now, `mode` = `direct` (scraped in
 *     one task) or `chunked` (fanned out over ScrapeChunk tasks).
 *
 * The maximum alone cannot tell "everything fresh" from "one group going stale":
 * a whole phase band that stops coming due moves the p90 long before the single
 * oldest venue looks unusual, while one broken venue moves the maximum and leaves
 * the quantiles flat. The due count answers the question every stalled-looking
 * panel downstream raises — "is nothing being scraped because nothing is DUE?" —
 * from the same [[DueWindow]] the reaper enqueues with, so it cannot disagree with
 * the scheduler. It counts a chunked venue whose run is still in flight as due
 * (its stamp lands when the run ends), which the reaper itself skips; a due count
 * that stays up while scrape tasks stay at zero is that difference, or a reaper
 * that is not ticking.
 *
 * The two are deliberately separate. A never-scraped cinema has no age, and
 * folding it into the age gauge would mean either pretending it is 0s old (a
 * silent lie that hides the worst case) or picking an arbitrary sentinel that
 * dwarfs the real ages and flattens the chart. Keeping the count beside the age
 * follows [[services.scrapes.ScrapeArchiveRepository]]'s rule that a read which
 * never happened is not data — see the failed-read-is-not-data convention.
 *
 * In steady state the age gauge rides just under the country's scrape window
 * (`KINOWO_SCRAPE_FRESHNESS_MINUTES`, set per country in that worker's k3s
 * overlay — the values are NOT uniform, each is sized so that country's paced
 * sweep fits inside it, so read them off the overlays rather than from here) in a
 * sawtooth: cinemas fall due, get scraped, and the maximum resets. A line that
 * climbs past the window in a straight diagonal means the reaper is not draining
 * the backlog — the credit-throttle spiral, a chunked-scrape starvation, or a
 * cinema whose scrape throws every time (a failure never marks freshness, by
 * design, so it stays stale and its age keeps climbing).
 *
 * Sampled off-band against the in-memory freshness mirror — no Mongo round-trips,
 * a few thousand map lookups per tick — and published onto the same registry and
 * `/metrics` endpoint as [[WorkerTaskMetrics]]. Mirrors the sample-and-publish
 * shape of [[RatingRunCensus]].
 */
class CinemaScrapeCensus(
  scrapers:       Seq[CinemaScraper],
  freshness:      FreshnessStore,
  oldestAge:      Gauge,
  neverScraped:   Gauge,
  ageQuantile:    Gauge,
  dueVenues:      Gauge,
  // The SAME due schedule the ScrapeReaper enqueues with — see `dueVenues`.
  dueWindow:      DueWindow,
  // Whether a venue is scraped through the chunk fan-out — the `mode` split.
  chunked:        Cinema => Boolean,
  country:        Country,
  clock:          Clock,
  override protected val sampleInterval: FiniteDuration = CinemaScrapeCensus.DefaultSampleInterval,
  override protected val ticks: tools.TickRecorder = tools.TickRecorder.Unrecorded
) extends SampledCensus {
  import CinemaScrapeCensus._

  private val countryCode = country.code

  // The freshness keys this country's roster is stamped under, resolved once —
  // the roster is fixed at wiring time, so re-deriving it every tick would be
  // pure waste.
  private val scrapeKeys: Seq[String] = scrapers.map(s => ScrapeCinemaHandler.dedupKey(s.cinema))
  private val chunkedKeys: Set[String] =
    scrapers.map(_.cinema).filter(chunked).map(ScrapeCinemaHandler.dedupKey).toSet

  // Materialize both series at 0 so this country exists on the panel from boot
  // rather than appearing at the first tick — no Grafana gap.
  oldestAge.labelValues(countryCode).set(0.0)
  neverScraped.labelValues(countryCode).set(0.0)
  Quantiles.foreach(q => ageQuantile.labelValues(countryCode, q.label).set(0.0))
  Modes.foreach(mode => dueVenues.labelValues(countryCode, mode).set(0.0))

  /** Read every cinema's last-successful-scrape stamp once and publish the
   *  roster's worst case. Read-only, in-memory. */
  def sample(): Unit = {
    val now       = clock.instant()
    val staleness = census(scrapeKeys, freshness.lastFetchedAt, now)
    oldestAge.labelValues(countryCode).set(staleness.oldestAgeSeconds)
    neverScraped.labelValues(countryCode).set(staleness.neverScraped.toDouble)
    staleness.ageQuantiles.foreach { case (q, seconds) => ageQuantile.labelValues(countryCode, q.label).set(seconds) }
    val due = dueCount(scrapeKeys, chunkedKeys, key => dueWindow.isDue(key, freshness.lastFetchedAt(key), now))
    dueVenues.labelValues(countryCode, DirectMode).set(due.direct.toDouble)
    dueVenues.labelValues(countryCode, ChunkedMode).set(due.chunked.toDouble)
  }

  override protected val censusName: String = "cinema-scrape-census"
}

object CinemaScrapeCensus {
  val OldestAgeName    = "kinowo_worker_cinema_scrape_oldest_age_seconds"
  val NeverScrapedName = "kinowo_worker_cinema_scrape_never_scraped"
  val AgeQuantileName  = "kinowo_worker_cinema_scrape_age_quantile_seconds"
  val DueVenuesName    = "kinowo_worker_cinema_scrape_due_venues"

  /** One published quantile of the scraped cinemas' ages: `label` is its `quantile` value. */
  case class AgeQuantile(fraction: Double, label: String)
  val Quantiles: Seq[AgeQuantile] = Seq(AgeQuantile(0.5, "0.5"), AgeQuantile(0.9, "0.9"))

  val DirectMode  = "direct"
  val ChunkedMode = "chunked"
  val Modes: Seq[String] = Seq(DirectMode, ChunkedMode)

  /** Build and register the two shared gauges every country's census writes into,
   *  each carrying the standard leading `country` label. Called once when the
   *  shared worker registry is built. */
  def gauges(registry: PrometheusRegistry): (Gauge, Gauge) = {
    val oldestAge = Gauge.builder()
      .name(OldestAgeName)
      .help("Seconds since the LEAST recently scraped cinema last scraped successfully, per country — the roster's worst-case staleness. Rides just under the country's scrape window in a sawtooth when healthy; climbs in a straight diagonal when the reaper isn't draining the backlog or a cinema fails every attempt (a failure never marks freshness). Cinemas that have never scraped carry no age and are counted separately by kinowo_worker_cinema_scrape_never_scraped.")
      .labelNames("country")
      .register(registry)
    val neverScraped = Gauge.builder()
      .name(NeverScrapedName)
      .help("Cinemas in this country's roster with no successful scrape at all, per country. Held apart from the oldest-age gauge because a never-scraped cinema has no age — folding it in would either read as 0s (hiding the worst case) or need a sentinel that flattens the chart. Non-zero right after a cinema is added and expected to fall to 0; a stuck non-zero is a cinema that has never once worked.")
      .labelNames("country")
      .register(registry)
    (oldestAge, neverScraped)
  }

  /** The scraped cinemas' median and p90 age, beside the oldest — `country`, `quantile`. */
  def ageQuantileGauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(AgeQuantileName)
      .help("Quantiles (quantile=0.5, 0.9) of the seconds since each cinema in this country's roster last scraped successfully, per country. Read beside kinowo_worker_cinema_scrape_oldest_age_seconds: one broken venue moves the maximum and leaves these flat, while a whole group of venues that stops coming due moves the p90 first. Never-scraped cinemas carry no age and are left out; an empty or never-scraped roster reads 0.")
      .labelNames("country", "quantile")
      .register(registry)

  /** How many venues are due for a scrape right now — `country`, `mode` (direct | chunked). */
  def dueVenuesGauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(DueVenuesName)
      .help("Venues the scrape due schedule (the ScrapeReaper's DueWindow) calls due right now, per country and mode (direct = scraped in one task, chunked = fanned out over ScrapeChunk tasks). Never-scraped venues count as due; so does a chunked venue whose run is still in flight, which the reaper itself skips. Zero for hours means nothing came due — idle scrape tasks then are the schedule, not a stall.")
      .labelNames("country", "mode")
      .register(registry)

  /** Once a minute — the same cadence the [[services.tasks.ScrapeReaper]] ticks
   *  at, so the sawtooth's resets land on the sample grid rather than being
   *  smeared across it. The sample is a few thousand in-memory map lookups. */
  val DefaultSampleInterval: FiniteDuration = 1.minute

  /** The roster's scrape staleness: the largest `now − lastFetchedAt` among the
   *  cinemas that HAVE scraped, and how many have never scraped. */
  case class ScrapeStaleness(oldestAgeSeconds: Double, neverScraped: Int, ageQuantiles: Seq[(AgeQuantile, Double)] = Nil)

  /** How many due venues are direct and how many chunked. */
  case class DueVenues(direct: Int, chunked: Int)

  /** Pure count of the due keys, split by whether each is chunked. */
  private[services] def dueCount(scrapeKeys: Iterable[String], chunkedKeys: Set[String], isDue: String => Boolean): DueVenues = {
    val (chunked, direct) = scrapeKeys.filter(isDue).partition(chunkedKeys)
    DueVenues(direct = direct.size, chunked = chunked.size)
  }

  /** Nearest-rank quantile of `sorted` (ascending, non-empty): the smallest value at
   *  or above `fraction` of the samples, so p90 of ten ages is the ninth. */
  private[services] def nearestRank(sorted: IndexedSeq[Double], fraction: Double): Double =
    sorted(math.min(sorted.size - 1, math.max(0, math.ceil(fraction * sorted.size).toInt - 1)))

  /** Pure census over the freshness stamps. Cinemas with no stamp contribute to
   *  `neverScraped` and are excluded from the age — an unknown age is not a zero
   *  one. An empty roster, or one where nothing has ever scraped, reports age
   *  0.0: there is no oldest scrape to be old. A stamp in the future (a clock
   *  skew between machines) clamps to 0 rather than going negative. */
  private[services] def census(
    scrapeKeys:    Iterable[String],
    lastFetchedAt: String => Option[Instant],
    now:           Instant
  ): ScrapeStaleness = {
    var never = 0
    val ages  = Vector.newBuilder[Double]
    scrapeKeys.foreach { key =>
      lastFetchedAt(key) match {
        case None     => never += 1
        case Some(at) => ages += math.max(0.0, (now.toEpochMilli - at.toEpochMilli).toDouble / 1000.0)
      }
    }
    val sorted = ages.result().sorted
    val quantiles = Quantiles.map(q => q -> (if (sorted.isEmpty) 0.0 else nearestRank(sorted, q.fraction)))
    ScrapeStaleness(sorted.lastOption.getOrElse(0.0), never, quantiles)
  }
}
