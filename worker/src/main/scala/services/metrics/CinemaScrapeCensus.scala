package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.cinemas.common.CinemaScraper
import services.freshness.FreshnessStore
import services.tasks.ScrapeCinemaHandler

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
 *     have no successful scrape at all.
 *
 * The two are deliberately separate. A never-scraped cinema has no age, and
 * folding it into the age gauge would mean either pretending it is 0s old (a
 * silent lie that hides the worst case) or picking an arbitrary sentinel that
 * dwarfs the real ages and flattens the chart. Keeping the count beside the age
 * follows [[services.scrapes.ScrapeArchiveRepository]]'s rule that a read which
 * never happened is not data — see the failed-read-is-not-data convention.
 *
 * In steady state the age gauge rides just under the country's scrape window
 * (`KINOWO_SCRAPE_FRESHNESS_MINUTES`, 60min for PL/UK, 180min for DE) in a
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
  country:        Country,
  clock:          Clock = Clock.systemUTC(),
  override protected val sampleInterval: FiniteDuration = CinemaScrapeCensus.DefaultSampleInterval
) extends SampledCensus {
  import CinemaScrapeCensus._

  private val countryCode = country.code

  // The freshness keys this country's roster is stamped under, resolved once —
  // the roster is fixed at wiring time, so re-deriving it every tick would be
  // pure waste.
  private val scrapeKeys: Seq[String] = scrapers.map(s => ScrapeCinemaHandler.dedupKey(s.cinema))

  // Materialize both series at 0 so this country exists on the panel from boot
  // rather than appearing at the first tick — no Grafana gap.
  oldestAge.labelValues(countryCode).set(0.0)
  neverScraped.labelValues(countryCode).set(0.0)


  /** Read every cinema's last-successful-scrape stamp once and publish the
   *  roster's worst case. Read-only, in-memory. */
  def sample(): Unit = {
    val staleness = census(scrapeKeys, freshness.lastFetchedAt, clock.instant())
    oldestAge.labelValues(countryCode).set(staleness.oldestAgeSeconds)
    neverScraped.labelValues(countryCode).set(staleness.neverScraped.toDouble)
  }

  override protected val censusName: String = "cinema-scrape-census"
}

object CinemaScrapeCensus {
  val OldestAgeName    = "kinowo_worker_cinema_scrape_oldest_age_seconds"
  val NeverScrapedName = "kinowo_worker_cinema_scrape_never_scraped"

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

  /** Once a minute — the same cadence the [[services.tasks.ScrapeReaper]] ticks
   *  at, so the sawtooth's resets land on the sample grid rather than being
   *  smeared across it. The sample is a few thousand in-memory map lookups. */
  val DefaultSampleInterval: FiniteDuration = 1.minute

  /** The roster's scrape staleness: the largest `now − lastFetchedAt` among the
   *  cinemas that HAVE scraped, and how many have never scraped. */
  case class ScrapeStaleness(oldestAgeSeconds: Double, neverScraped: Int)

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
    var oldest = 0.0
    var never  = 0
    scrapeKeys.foreach { key =>
      lastFetchedAt(key) match {
        case None => never += 1
        case Some(at) =>
          val ageSeconds = math.max(0.0, (now.toEpochMilli - at.toEpochMilli).toDouble / 1000.0)
          if (ageSeconds > oldest) oldest = ageSeconds
      }
    }
    ScrapeStaleness(oldest, never)
  }
}
