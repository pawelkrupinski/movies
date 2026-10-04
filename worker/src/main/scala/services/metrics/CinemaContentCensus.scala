package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.cinemas.common.CinemaScraper
import services.scrapes.{BarrenAttempt, ContentStamp, ForwardingScrapeArchive, ScrapeArchiveRepository, SuccessfulScrape}

import java.time.{Clock, Instant}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

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
 *     one at all, and
 *   - `kinowo_worker_cinema_content_stale_venues{country}` — how many last produced
 *     one more than [[CinemaContentCensus.StaleAfter]] ago. The oldest age above is
 *     ONE venue, the roster's worst, so it cannot say whether that venue is alone:
 *     DE sat at 1,383h and the US at 600h on 2026-09-24 with nothing reading either.
 *     A count is what shows a SECOND venue falling silent, which is the shape a
 *     client-wide parser break takes (one skin change, every venue on it). A venue
 *     whose newest scrape was its own source AFFIRMATIVELY listing no schedule (a
 *     page that parsed and says it has nothing on — see
 *     `CinemaScraper.noScheduleListed`) is left out: 17 US drive-ins closing for
 *     the season held `CinemaContentStaleVenuesGrowing` firing for 44 hours
 *     (2026-09-27..29). A parser break still counts, because it throws or comes
 *     back empty with nothing vouching for it.
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
 * Kept from the scrape archive rather than the corpus, because the archive is the
 * only place that remembers the last scrape which HAD content — the read model simply
 * has no row for a film nobody is showing. Its stamps are read ONCE, at the first
 * reading, and then kept as each scrape is archived ([[watching]]): the archive is
 * written nowhere else. A daily re-read ([[CinemaContentCensus.RereadInterval]])
 * catches what that misses — a write that failed after it was counted. Until a read
 * has succeeded, nothing is published. This read every venue's stamp every 30
 * minutes until 2026-10-04: 5,000 documents a reading on the US.
 */
class CinemaContentCensus(
  scrapers:     Seq[CinemaScraper],
  archive:      ScrapeArchiveRepository,
  oldestAge:    Gauge,
  neverContent: Gauge,
  staleVenues:  Gauge,
  country:      Country,
  clock:        Clock,
  override protected val sampleInterval: FiniteDuration = CinemaContentCensus.DefaultSampleInterval,
  rereadInterval: FiniteDuration = CinemaContentCensus.RereadInterval
) extends SampledCensus {
  import CinemaContentCensus._

  // Each venue's stamp, as the last read gave it and every scrape archived since moved it.
  private val stamps = new java.util.concurrent.ConcurrentHashMap[String, ContentStamp]()
  // The venues a scrape moved since the last read began: a read cannot know whether it saw their newest stamp.
  private val moved  = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
  // When the archive was last read whole; None until it has been.
  @volatile private var lastRead: Option[Instant] = None

  override protected val censusName: String = "cinema-content-census"

  private val countryCode = country.code

  // This country's roster, resolved once — fixed at wiring time. The archive is
  // per-country too (each worker writes its own database), but scoping to the
  // roster keeps a cinema that has been dropped from the catalogue out of the
  // count instead of pinning it high forever.
  private val roster: Seq[String] = scrapers.map(_.cinema.displayName).distinct

  oldestAge.labelValues(countryCode).set(0.0)
  neverContent.labelValues(countryCode).set(0.0)
  // NOT zeroed at construction, unlike the two above: its alert compares today's count with
  // yesterday's lowest, and a boot-time zero would read as "yesterday there were none".
  // `start()` takes the first reading at once, so the series appears within the boot.

  def sample(): Unit = {
    val now = clock.instant()
    if (lastRead.forall(at => !now.isBefore(at.plusMillis(rereadInterval.toMillis)))) read(now)
    // Never read whole: nothing to stand behind, so the gauges hold whatever they held.
    if (lastRead.isDefined) {
      val census = quiet(roster, stamps.asScala.toMap, now)
      oldestAge.labelValues(countryCode).set(census.oldestAgeSeconds)
      neverContent.labelValues(countryCode).set(census.neverContent.toDouble)
      staleVenues.labelValues(countryCode).set(census.staleVenues.toDouble)
    }
  }

  private def read(now: Instant): Unit = {
    moved.clear()
    val read = archive.contentStamps()
    // Empty means the read failed (or there is no archive at all) — NOT that every
    // cinema has gone quiet. Taken, that would turn a Mongo blip into a roster-wide
    // outage on the panel, the exact inversion this metric exists to avoid. The next
    // reading reads again.
    if (read.nonEmpty || roster.isEmpty) {
      read.foreach { case (venue, stamp) => if (!moved.contains(venue)) stamps.put(venue, stamp) }
      lastRead = Some(now)
    }
  }

  /** `underlying`, every scrape filed in it also moving that venue's stamp here — as the archive's own rules move it
   *  ([[ScrapeArchiveRepository.record]]): a listing with films is the venue's newest content; an attempt that
   *  produced none, unless older than that content, says only whether the source vouched for its silence. */
  def watching(underlying: ScrapeArchiveRepository): ScrapeArchiveRepository = new ForwardingScrapeArchive(underlying) {
    override protected def storeSuccess(cinema: models.Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = {
      super.storeSuccess(cinema, city, scrape)
      moved.add(cinema.displayName)
      stamps.put(cinema.displayName, ContentStamp(Some(scrape.at)))
      ()
    }
    override protected def storeBarren(cinema: models.Cinema, city: Option[String], attempt: BarrenAttempt): Unit = {
      super.storeBarren(cinema, city, attempt)
      // A venue not held yet is left to the read: its content stamp is the archive's to say.
      stamps.computeIfPresent(cinema.displayName, (_, held) => {
        moved.add(cinema.displayName)
        if (held.lastContentAt.exists(_.isAfter(attempt.at))) held else held.copy(noScheduleListed = attempt.noScheduleListed)
      })
      ()
    }
  }
}

object CinemaContentCensus {
  val OldestAgeName    = "kinowo_worker_cinema_content_oldest_age_seconds"
  val NeverContentName = "kinowo_worker_cinema_never_content"
  val StaleVenuesName  = "kinowo_worker_cinema_content_stale_venues"

  /** A venue quiet this long is STALE. A week, because a repertory venue can legitimately go
   *  a few days between programmes (a Monday-to-Wednesday dark run, a festival gap), and every
   *  horizon a scraper looks ahead covers at least a week — so a week with nothing at all is
   *  past anything a working parser of an open cinema produces. Only a source that SAYS it has
   *  no schedule marks a cinema closed (`ContentStamp.noScheduleListed`); most cannot, which is
   *  why the alert on this gauge watches it GROW rather than its level. */
  val StaleAfter: FiniteDuration = 7.days

  /** How long the quietest cinema has gone without content, how many have never had any, and
   *  how many last had some more than [[StaleAfter]] ago (never-content venues are NOT among
   *  them — they have no age, and their own gauge counts them — and neither is a venue whose
   *  source last said it has no schedule). */
  case class ContentCensus(oldestAgeSeconds: Double, neverContent: Int, staleVenues: Int)

  /** Pure: fold the roster against its archive stamps. A cinema missing from the
   *  archive counts the same as one archived with no content — in both cases we
   *  have never seen it produce a film. */
  def quiet(roster: Seq[String], stamps: Map[String, ContentStamp], now: Instant): ContentCensus = {
    val rows = roster.map(cinema => stamps.getOrElse(cinema, ContentStamp(None)))
    def agesSeconds(of: Seq[ContentStamp]): Seq[Double] =
      of.flatMap(_.lastContentAt).map(at => (now.toEpochMilli - at.toEpochMilli) / 1000.0)
    ContentCensus(
      oldestAgeSeconds = agesSeconds(rows).maxOption.getOrElse(0.0),
      neverContent     = rows.count(_.lastContentAt.isEmpty),
      staleVenues      = agesSeconds(rows.filterNot(_.noScheduleListed)).count(_ > StaleAfter.toSeconds)
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

  def staleVenuesGauge(registry: PrometheusRegistry): Gauge = Gauge.builder()
    .name(StaleVenuesName)
    .help("Cinemas in this country's roster whose last content-bearing scrape is more than 7 days old (never-content venues excluded: kinowo_worker_cinema_never_content counts those; so are venues whose own page last said they have no schedule). Most sources cannot say a venue is closed, so a season's dormant venues still sit here: watch it GROW day over day, which is what a parser break across a client's venues looks like.")
    .labelNames("country")
    .register(registry)

  /** Every 30 minutes: the measured thing moves in days. A reading reads nothing but the stamps held here. */
  val DefaultSampleInterval: FiniteDuration = 30.minutes

  /** How often the stamps are read from the archive again, to catch what the scrapes filed here missed. */
  val RereadInterval: FiniteDuration = 1.day
}
