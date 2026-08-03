package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.cinemas.common.CinemaScraper
import services.scrapes.ScrapeArchiveRepository

import java.time.{Clock, Instant}
import scala.concurrent.duration._

/**
 * Surfaces cinemas that SCRAPE fine and produce nothing — the failure mode no
 * other signal can see.
 *
 * [[CinemaScrapeCensus]] measures whether a cinema is still being scraped, and a
 * venue whose parser has quietly stopped matching passes that test perfectly: the
 * fetch succeeds, the scrape is marked fresh, its age resets, nothing errors.
 * `/uptime` renders it white ("zero results"), which is the SAME thing a cinema
 * that is genuinely closed for the summer renders, so no alert can fire on it
 * without firing on every dormant arthouse in August.
 *
 * That gap is not hypothetical. On 2026-08-03, 15 Polish cinemas had never once
 * recorded a content-bearing scrape. Twelve were legitimately shut — their pages
 * said so in as many words ("przerwa wakacyjna", "Brak wydarzeń"). Two had drifted:
 * Kino Sfinks's listing table had been replaced by a different one, and Kino
 * Kuźnica matched none of its client's three known skins. Nothing distinguished
 * them from the dormant twelve, and nothing would have, indefinitely.
 *
 * What separates the two is TIME. A summer break ends; a broken selector doesn't.
 * So this reports, per country:
 *   - `kinowo_worker_cinema_content_oldest_age_seconds{country}` — how long the
 *     longest-quiet cinema has gone without producing a single film, and
 *   - `kinowo_worker_cinema_never_content{country}` — how many have never produced
 *     one at all.
 *
 * Deliberately the same shape as [[CinemaScrapeCensus]]'s pair, and split for the
 * same reason: a cinema with no content has no age, so folding it into the age
 * gauge would either read as 0s (hiding the worst case) or need a sentinel that
 * flattens the chart.
 *
 * Neither number is an alert on its own — a genuine winter-long closure will climb
 * too. They are the number that makes a drifting parser ASKABLE, instead of
 * invisible: a venue quiet far longer than a season is worth opening the site for.
 *
 * Sampled from the scrape archive rather than the corpus, because the archive is
 * the only place that remembers the last scrape which HAD content — the read model
 * simply has no row for a film nobody is showing. That is a Mongo read, so it runs
 * on its own slow cadence ([[CinemaContentCensus.DefaultSampleInterval]]): the
 * thing being measured moves in days, and there is no sense paying a query a minute
 * to watch it.
 */
class CinemaContentCensus(
  scrapers:     Seq[CinemaScraper],
  archive:      ScrapeArchiveRepository,
  oldestAge:    Gauge,
  neverContent: Gauge,
  country:      Country,
  clock:        Clock = Clock.systemUTC(),
  override protected val sampleInterval: FiniteDuration = CinemaContentCensus.DefaultSampleInterval
) extends SampledCensus {
  import CinemaContentCensus._

  override protected val censusName: String = "cinema-content-census"

  private val countryCode = country.code

  // This country's roster, resolved once — fixed at wiring time. The archive is
  // per-country too (each worker writes its own database), but scoping to the
  // roster keeps a cinema that has been dropped from the catalogue out of the
  // count instead of pinning it high forever.
  private val roster: Seq[String] = scrapers.map(_.cinema.displayName).distinct

  oldestAge.labelValues(countryCode).set(0.0)
  neverContent.labelValues(countryCode).set(0.0)

  def sample(): Unit = {
    val stamps = archive.lastContentAt()
    // Empty means the read failed (or there is no archive at all) — NOT that every
    // cinema has gone quiet. Publishing that would turn a Mongo blip into a
    // roster-wide outage on the panel, which is the exact inversion this metric
    // exists to avoid. Hold the previous reading instead.
    if (stamps.isEmpty && roster.nonEmpty) return

    val census = quiet(roster, stamps, clock.instant())
    oldestAge.labelValues(countryCode).set(census.oldestAgeSeconds)
    neverContent.labelValues(countryCode).set(census.neverContent.toDouble)
  }
}

object CinemaContentCensus {
  val OldestAgeName    = "kinowo_worker_cinema_content_oldest_age_seconds"
  val NeverContentName = "kinowo_worker_cinema_never_content"

  /** How long the quietest cinema has gone without content, and how many have
   *  never had any. */
  case class ContentCensus(oldestAgeSeconds: Double, neverContent: Int)

  /** Pure: fold the roster against its archive stamps. A cinema missing from the
   *  archive counts the same as one archived with no content — in both cases we
   *  have never seen it produce a film. */
  def quiet(roster: Seq[String], stamps: Map[String, Option[Instant]], now: Instant): ContentCensus = {
    val ages = roster.map(cinema => stamps.getOrElse(cinema, None))
    ContentCensus(
      oldestAgeSeconds = ages.flatten.map(at => (now.toEpochMilli - at.toEpochMilli) / 1000.0).maxOption.getOrElse(0.0),
      neverContent     = ages.count(_.isEmpty)
    )
  }

  def gauges(registry: PrometheusRegistry): (Gauge, Gauge) = {
    val oldestAge = Gauge.builder()
      .name(OldestAgeName)
      .help("Seconds since the longest-quiet cinema last produced ANY films, per country. Distinct from kinowo_worker_cinema_scrape_oldest_age_seconds, which only says whether a cinema is still being SCRAPED: a venue whose parser has stopped matching scrapes perfectly and returns nothing, so it stays fresh there while climbing here. A season-long closure climbs too, so this is a prompt to look, not an outage on its own.")
      .labelNames("country")
      .register(registry)
    val neverContent = Gauge.builder()
      .name(NeverContentName)
      .help("Cinemas in this country's roster that have never once recorded a content-bearing scrape. Held apart from the oldest-age gauge because a cinema with no content has no age to report. Expected non-zero — dormant venues live here too — so watch it for STEPS: one more cinema falling silent while the rest of the roster keeps producing is what a drifted selector looks like.")
      .labelNames("country")
      .register(registry)
    (oldestAge, neverContent)
  }

  /** Every 30 minutes. The measured thing moves in days, and unlike its in-memory
   *  sibling each reading is a Mongo query — a per-minute cadence would buy no
   *  resolution and add a query a minute for the life of the worker. */
  val DefaultSampleInterval: FiniteDuration = 30.minutes
}
