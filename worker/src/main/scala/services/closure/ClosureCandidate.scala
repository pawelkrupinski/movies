package services.closure

import java.time.Instant

import models.{ArchivedScrape, Cinema}
import services.fallback.FallbackState
import services.scrapes.VenueClosure.SourceDrop
import services.scrapes.{GoneUpstream, VenueClosure}

/** A rostered venue the sweep judges, whether it has a fallback feed to corroborate it, and
 *  the page its primary scrapes (`CinemaScraper.sourceUrl`) — the page whose 404 is the
 *  source-drop evidence, so the one the retirement workflow re-checks. `pagedWhenGone`: the
 *  gone-venue alert (`GoneVenueAlertingArchive`) owns this venue's page-gone spells — no
 *  fallback's UNCOVERED or Filmweb-drop alert pages them instead. */
final case class ClosureCandidate(cinema: Cinema, hasFallback: Boolean, page: Option[String] = None,
                                  pagedWhenGone: Boolean = false) {

  /** The venue's name, as the ledger and the pages know it. */
  def name: String = cinema.displayName

  /** Whether its sources dropped it, by its archived scrape `row` and its fallback's state ([[VenueClosure]]). */
  def dropped(row: ArchivedScrape, fallback: Option[FallbackState], now: Instant): Either[String, SourceDrop] =
    VenueClosure.sourceDropped(row, fallback, hasFallback, cinema.opening, now)

  /** Whether the gone-venue alert has already paged the page-gone spell `row` is in ([[GoneUpstream.pagedAsGone]]). */
  def pagedAsGone(row: ArchivedScrape): Boolean = pagedWhenGone && row.lastBarren.exists(GoneUpstream.pagedAsGone)

  /** Where it sits in a data-driven roster: none for a venue retired by hand. */
  def rosterEntry: Option[RosterEntry] = RosterEntry.of(cinema)
}
