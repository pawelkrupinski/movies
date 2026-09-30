package services.cinemas.roster

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaMovie, Country}
import play.api.Logging
import services.scrapes.{ArchivedScrape, ScrapeArchiveRepository}

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
 * Roster venues whose scraped feed is ANOTHER venue's: an aggregator filing one venue's
 * programme under a second venue's name (Flicks gave the Syracuse IN Pickwick the Park Ridge IL
 * one's, 216 of 216 showtimes, booked through Park Ridge's own Veezi sessions), or one screen
 * wired to two roster names.
 *
 * Told by BOOKING SESSIONS, not by programme: a booking link names one venue's one session, so
 * two venues listing the same sessions list one feed — where two venues showing the same films at
 * the same times are, far more often, a chain running one grid (Cineworld, Odeon), each booking
 * through its own site. Each venue listing is reduced to one fingerprint, the smallest hash among
 * its sessions (a MinHash: two listings with the same sessions have the same fingerprint, and
 * chain-mates' own sessions give them different ones), so the whole roster costs one entry per
 * listing — ~108k on the US, a few MB — instead of one per showtime.
 *
 * Only for the venues whose scraper reads an upstream KNOWN to do this ([[KnownToCopy]], by client):
 * both venues of a copied pair are read through it, so the rest of the roster is neither checked
 * nor indexed, and a country with none of them runs no detector at all.
 *
 * Checked as each venue's scrape lands ([[CopiedFeedArchive]]), and seeded once per boot from the
 * archive's latest scrape of every venue ([[seed]]), so a copy that predates the restart is found
 * again. Until 2026-09-30 this was `DuplicateVenueCensus`: every venue's whole programme compared
 * every 5 minutes, off a scan decoding every showtime — about a quarter of a core on the US worker
 * for a condition that changes only when a venue is scraped. What the census saw and this cannot:
 * a screen listed twice through sources with no booking links at all.
 */
final class CopiedFeedDetector(pairs: Gauge, country: Country, watched: Set[Cinema]) extends Logging {
  import CopiedFeedDetector._

  private val countryCode = country.code
  private val owners      = mutable.LongMap.empty[Cinema]
  private val held        = mutable.HashMap.empty[Cinema, Array[Long]]
  // Each venue's latest scrape: the venues whose sessions it lists. A pair lasts until the venue
  // that lists the other's sessions is scraped clean.
  private val copying     = mutable.HashMap.empty[Cinema, Set[Cinema]]
  @volatile private var seeded = false

  /** A venue's scrape landed with `films`: its listings now. */
  def venueScraped(cinema: Cinema, films: Seq[CinemaMovie]): Unit = synchronized(land(cinema, films))

  /** Every venue's latest archived scrape, as it stands — each a landing, but never over a venue a
   *  fresher scrape already landed for since boot. Publishes the gauge from here on. */
  def seed(scrapes: Seq[ArchivedScrape]): Unit = synchronized {
    scrapes.foreach(scrape => if (!held.contains(scrape.cinema)) scrape.lastSuccess.foreach(s => land(scrape.cinema, s.films)))
  }

  /** The archive has been read whole: from now on the gauge says what the roster holds. */
  def seedComplete(): Unit = synchronized { seeded = true; publish() }

  /** [[seed]] from every venue's latest archived scrape, a page at a time, [[seedComplete]] once the
   *  archive was read whole. A partial read publishes nothing: a copy it missed would read as none. */
  def seedFrom(archive: ScrapeArchiveRepository): Unit =
    if (archive.scan(seed)) seedComplete()
    else logger.warn(s"copied feed ($countryCode): the scrape archive could not be read whole — the gauge stays " +
      "unpublished until the next boot's seed; landings are still checked.")

  /** Seed after `delay` on a thread of its own: past a boot's heavy stretch, which it would only slow. */
  def start(archive: ScrapeArchiveRepository, delay: scala.concurrent.duration.FiniteDuration): Unit = {
    val scheduler = tools.DaemonExecutors.scheduler(s"copied-feed-seed-$countryCode")
    scheduler.schedule((() => try seedFrom(archive) finally scheduler.shutdown()): Runnable, delay.toSeconds,
      java.util.concurrent.TimeUnit.SECONDS)
    ()
  }

  /** The pairs whose one venue lists the other's sessions, each once. */
  def copiedPairs: Set[(String, String)] = synchronized(pairsNow)

  private def land(cinema: Cinema, films: Seq[CinemaMovie]): Unit = if (watched(cinema)) {
    val fingerprints = fingerprintsOf(films)
    val shared       = fingerprints.iterator.flatMap(owners.get).filter(other => other != cinema && watched(other))
      .toSeq.groupMapReduce(identity)(_ => 1)(_ + _)
    val copied = shared.collect {
      case (other, n) if n >= MinListings && n >= MinShare * fingerprints.length && !DistinctVenuePairs.contains(cinema, other) => other
    }.toSet
    held.remove(cinema).foreach(_.foreach(fp => if (owners.get(fp).contains(cinema)) owners.remove(fp)))
    fingerprints.foreach(owners.update(_, cinema))
    held.update(cinema, fingerprints)
    val before = copying.getOrElse(cinema, Set.empty)
    if (copied != before) {
      if (copied.isEmpty) copying.remove(cinema) else copying.update(cinema, copied)
      (copied -- before).foreach(other => logger.warn(s"copied feed ($countryCode): '${cinema.displayName}' lists " +
        s"${shared(other)} of its ${fingerprints.length} listings through '${other.displayName}'s own booking sessions — " +
        "one venue's feed under another's name? Check both in CinemaScraperCatalog; two venues genuinely sharing " +
        "sessions go on DistinctVenuePairs."))
      (before -- copied).foreach(other => logger.info(s"copied feed ($countryCode): '${cinema.displayName}' no longer " +
        s"lists '${other.displayName}'s sessions."))
      publish()
    }
  }

  private def pairsNow: Set[(String, String)] =
    copying.iterator.flatMap { case (a, others) => others.iterator.map(b =>
      if (a.displayName <= b.displayName) (a.displayName, b.displayName) else (b.displayName, a.displayName)) }.toSet

  private def publish(): Unit = if (seeded) { pairs.labelValues(countryCode).set(pairsNow.size.toDouble); () }
}

object CopiedFeedDetector {
  val Name = "kinowo_worker_copied_feed_pairs"

  /** The scraper clients whose upstream has filed one venue's feed under another's name — each
   *  entry names the cases that put it here, all found by the programme census's first prod run
   *  (2026-09-23) and each booking the other venue's very sessions:
   *   - Flicks: the Syracuse IN Pickwick listed the Park Ridge IL one's 216 showtimes, through Park
   *     Ridge's Veezi sessions;
   *   - a bilety24 organiser page: Koło's Kino nad Wartą repeated Konin's, and Braniewo's Baszta
   *     Środa Wielkopolska's Baszta — one organiser page read for two venues.
   *  Everything else that census found (2026-09-23..30) was chain-mates running one grid. */
  val KnownToCopy: Set[String] = Set(
    classOf[services.cinemas.common.FlicksClient].getSimpleName,
    classOf[services.cinemas.pl.Bilety24OrganizerClient].getSimpleName)

  /** The venues among `scrapers` read through a client [[KnownToCopy]]. */
  def watchedVenues(scrapers: Seq[services.cinemas.common.CinemaScraper]): Set[Cinema] =
    scrapers.filter(s => KnownToCopy(services.cinemas.common.CinemaClientMarkers.clientOf(s))).map(_.cinema).toSet

  /** How long after boot the seed reads the archive: past the take-up that settles a US boot. */
  val SeedDelay: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")

  /** A listing counts only with this many distinct booking sessions: one generic link a venue
   *  prints on every showtime (a film page, a box-office home) is shared by every venue using it. */
  val MinSessions: Int = 3

  /** A pair counts when the venue lists at least this many of its listings, and this share of
   *  them, through the other's sessions — one shared listing can be a cross-listed event. */
  val MinListings: Int  = 3
  val MinShare: Double  = 0.5

  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Pairs of roster venues where one venue's latest scrape lists the other's own booking sessions — one venue's feed under another's name (an aggregator mis-filing a feed, or one screen wired to two roster names). Checked as each venue's scrape lands, seeded from the scrape archive at boot; absent until that seed completes. Pairs on DistinctVenuePairs are cleared. Zero is healthy; the worker's WARN line \"copied feed (<country>)\" names the pair. Alerted by DuplicateVenueListing.")
      .labelNames("country")
      .register(registry)

  /** Each listing's fingerprint — the smallest hash among its distinct booking sessions — for the
   *  listings with at least [[MinSessions]] of them. */
  private[roster] def fingerprintsOf(films: Seq[CinemaMovie]): Array[Long] =
    films.iterator.flatMap { film =>
      val sessions = film.showtimes.iterator.flatMap(_.bookingUrl).map(sessionOf).toSet
      Option.when(sessions.size >= MinSessions)(sessions.iterator.map(hash64).min)
    }.toArray.distinct

  /** The part of a booking link naming the venue and the session: its path and query. Scheme and
   *  host are dropped — one ticketing backend serves one session from regional mirrors (Veezi's
   *  ticketing.us. and ticketing.useast.). */
  private[roster] def sessionOf(url: String): String = {
    val afterScheme = url.indexOf("://")
    if (afterScheme < 0) url
    else url.indexOf('/', afterScheme + 3) match {
      case -1 => ""
      case i  => url.substring(i)
    }
  }

  private def hash64(session: String): Long =
    (MurmurHash3.stringHash(session, 0x9747b28c).toLong << 32) | (MurmurHash3.stringHash(session, 0x5bd1e995) & 0xffffffffL)
}
