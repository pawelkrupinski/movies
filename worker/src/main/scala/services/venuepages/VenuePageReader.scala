package services.venuepages

import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}
import services.events.VenueDetailRead
import services.freshness.{FreshnessKind, FreshnessStore}
import services.tasks.EnrichDetailsTasks

import java.time.Clock

/**
 * Reads a venue detail page into `venue_pages`, the one place its facts are written: fetch it, store
 * what it said — its detail, or that it is gone — stamp the page (`pageRead` / `pageGone`, keyed by the
 * page so the answer moves with a listing when rows regroup) and announce it (`VenueDetailRead`), so
 * the identity model re-asks exactly that page. A fetch that failed for now stores nothing: the page
 * keeps whatever it last said, and is asked again on the caller's next tick. So is an answer the store
 * did not take ([[VenuePageRead.unfiled]]): the model reads a page only from the store, and only once
 * its read is announced, so a page read but not filed was not read at all.
 *
 * Every page fetch goes through here — the film rows' enrichment, staging's, a cut-over listing's —
 * each caller keeping only what is its own: when a page is due, and what it does with the detail.
 */
final class VenuePageReader(store: VenuePageStore, freshness: FreshnessStore, announce: VenueDetailRead => Unit, clock: Clock) {

  def read(enricher: DetailEnricher, page: String): VenuePageRead = {
    val outcome = enricher.fetchDetail(page)
    val key     = VenuePageKey(enricher.detailGroup, page)
    val now     = clock.instant()
    val filed = outcome match {
      case DetailFetchOutcome.Fetched(detail) =>
        store.put(VenuePage(key, VenuePage.Read(detail), now)) && stamp(EnrichDetailsTasks.pageRead(key.detailGroup, page))
      case DetailFetchOutcome.Gone(code) =>
        store.put(VenuePage(key, VenuePage.Gone(code), now)) && stamp(EnrichDetailsTasks.pageGone(key.detailGroup, page))
      case DetailFetchOutcome.Failed => false
    }
    // After the stamps, so a reader of the store sees the answer this announces.
    if (filed) announce(VenueDetailRead(key.detailGroup, page))
    VenuePageRead(outcome, filed)
  }

  private def stamp(freshnessKey: String): Boolean = {
    freshness.markFresh(freshnessKey, FreshnessKind.DetailEnrich, clock.instant()); true
  }
}

/** What reading a page did: what the venue answered, and whether `venue_pages` holds that answer now. */
final case class VenuePageRead(outcome: DetailFetchOutcome, filed: Boolean) {
  /** The venue answered and the store did not take it — a write Mongo applied and never acknowledged within its
   *  wait, on a loaded server. Nothing is stamped or announced, so the caller must ask for the page again: done
   *  with, its film is decided without the page's year and director until the page is next due. */
  def unfiled: Boolean = outcome != DetailFetchOutcome.Failed && !filed
}
