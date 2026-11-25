package services.tasks

import java.time.{Clock, Instant}

import scala.concurrent.duration.*

import models.Cinema
import play.api.Logging
import services.cinemas.common.{CinemaScraper, ScrapeErrors}
import services.scrapes.{GoneUpstream, ScrapeArchiveReader}

/**
 * What the scrape archive says about a venue's page — gone upstream, or read lately — and so whether a
 * scrape of it is worth running, and how loudly a failure to read it is reported.
 *
 * @param archive the same archive the scrape runner writes: it is what says whether a venue is merely
 *                failing or has 404'd for over a day. The no-op archive says nothing is ever gone.
 */
final class VenuePageHistory(archive: ScrapeArchiveReader, clock: Clock) extends Logging {
  import VenuePageHistory.BrokenFor

  /** Whether the scrape of `scraper`'s venue is skipped this time: its page — the page its scraper reads
   *  NOW, not one it was read from before a re-sourcing — has 404'd for over a day, so it is re-probed once
   *  a day, not once a window. Nothing about the roster can bring it back (both aggregators keep dead venues
   *  in the sitemaps we harvest from), so every attempt in between is a request that cannot succeed. */
  def skipsScrape(scraper: CinemaScraper): Boolean = {
    val skip = archive.find(scraper.cinema).exists(row => GoneUpstream.skipScrape(row, scraper.sourceKey, clock.instant()))
    if (skip) logger.info(s"Skipping ${scraper.cinema.displayName}: its page has been 404ing for over " +
      s"${GoneUpstream.MinimumAge.toHours}h; next probe in ${GoneUpstream.RecheckInterval.toHours}h.")
    skip
  }

  /** The venue's page has ALREADY stood gone for [[GoneUpstream.MinimumAge]] ([[GoneUpstream.isGone]]). */
  def knownGone(cinema: Cinema): Boolean = archive.find(cinema).exists(GoneUpstream.isGone(_, clock.instant()))

  /** A plain scrape of `cinema` started at `startedAt` failed after `elapsedMillis`: a WARN when the failure is
   *  transient or the venue was read within [[BrokenFor]] before it started, an ERROR with its stack (a Sentry
   *  issue) when it is broken past that. */
  def reportScrapeFailure(cinema: Cinema, failure: Exception, startedAt: Instant, elapsedMillis: Long): Unit =
    if (ScrapeErrors.isTransientHttpError(failure) || readWithin(cinema, BrokenFor, startedAt))
      logger.warn(s"Failed to refresh ${cinema.displayName} after ${elapsedMillis}ms: ${failure.getMessage}")
    else
      logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsedMillis}ms", failure)

  /** A chunked plan of `cinema` failed. A seasonal venue whose page is gone OFF its season is
   *  the season, not a fault: Filmstarts takes open-air pages down for the winter (all 28 German 404s in
   *  2026-10 were seasonal), and the closure sweep leaves such a venue alone until its season opens. And a
   *  page the quarantine already holds gone is its daily re-probe answering as expected: 50 Filmstarts and
   *  16 SensaCine pages warned once a probe for weeks (2026-10-29..11-12). Both say so at INFO. */
  def reportPlanFailure(cinema: Cinema, failure: Exception): Unit = {
    // Read off the text the archive records the failure as: a message may be null, that text never is.
    val said = tools.FailureText.of(failure)
    val gone = GoneUpstream.saysPageIsGone(said)
    if (gone && cinema.opening.offSeasonAt(clock.instant()))
      logger.info(s"chunked plan for ${cinema.displayName} found its page gone off-season: $said")
    else if (gone && knownGone(cinema))
      logger.info(s"chunked plan for ${cinema.displayName} found its quarantined page still gone: $said")
    else logger.warn(s"chunked plan for ${cinema.displayName} failed: $said")
  }

  /** Did the venue's scraper read it successfully within `window` before `now`? */
  private def readWithin(cinema: Cinema, window: FiniteDuration, now: Instant): Boolean =
    archive.find(cinema).flatMap(_.lastSuccess).exists(_.at.isAfter(now.minusMillis(window.toMillis)))
}

object VenuePageHistory {
  /** How long a venue's scraper must have failed to read it before a non-transient failure is an ERROR
   *  (a Sentry issue). Phoenix Leicester, 2026-10-12: three "the Savoy template changed" failures in three
   *  minutes filed one, and the next scrape was fine and stayed fine. Read within the day, it is a WARN;
   *  broken past it, or never read at all, it files an ERROR with its stack. */
  val BrokenFor: FiniteDuration = 24.hours
}
