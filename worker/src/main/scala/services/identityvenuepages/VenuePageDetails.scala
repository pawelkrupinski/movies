package services.identityvenuepages

import services.lookupfill.{LookupGaps, VenueDetails}
import services.cinemas.common.DetailEnricher
import services.identity.vocabulary.ObservationReads

/** A listing's venue detail as venue_pages holds it ([[VenuePageIndex]]): each enricher read through the index, never
 *  fetched ([[VenuePageDetailEnricher]]) -- a page not read yet is a gap, counted by one [[LookupGaps]]. */
object VenuePageDetails {
  def of(enrichers: Seq[DetailEnricher], index: VenuePageIndex, reads: ObservationReads = ObservationReads.Untracked): VenueDetails = {
    val gaps = new LookupGaps
    new VenueDetails(enrichers.map(new VenuePageDetailEnricher(_, index, gaps, reads)), gaps)
  }
}
