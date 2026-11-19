package services.closure

import models.Cinema

/** A rostered venue the sweep judges, whether it has a fallback feed to corroborate it, and
 *  the page its primary scrapes (`CinemaScraper.sourceUrl`) — the page whose 404 is the
 *  source-drop evidence, so the one the retirement workflow re-checks. `pagedWhenGone`: the
 *  gone-venue alert (`GoneVenueAlertingArchive`) owns this venue's page-gone spells — no
 *  fallback's UNCOVERED or Filmweb-drop alert pages them instead. */
final case class ClosureCandidate(cinema: Cinema, hasFallback: Boolean, page: Option[String] = None,
                                  pagedWhenGone: Boolean = false)
