package services.identity

import services.cinemas.common.DetailEnricher
import services.tasks.TaskQueue
import services.venuepages.ReadVenuePageTasks

import scala.concurrent.duration.FiniteDuration

/** A cut-over country's [[PageWait]]: a listing at a venue with a detail page waits while that page has
 *  no answer in venue_pages (`VenuePageIndex`), its page read by a `ReadVenuePage` task. A page that
 *  carries the listing's identity (`defersTmdbResolution`) is waited for until read or gone; a
 *  display-only one only until it has been tried — staging likewise lets a display-only page fail. */
final class VenuePageWait(enrichers: Seq[DetailEnricher], index: VenuePageIndex, queue: TaskQueue,
                          freshness: services.freshness.FreshnessStore, val limit: FiniteDuration, clock: java.time.Clock)
    extends PageWait {
  private val enricherOf = enrichers.map(e => e.cinema -> e).toMap

  private def pageOf(listing: Listing): Option[(DetailEnricher, String)] =
    for { page <- listing.page; enricher <- enricherOf.get(listing.cinema) } yield enricher -> page

  def awaiting(listing: Listing): Boolean = pageOf(listing).exists { case (enricher, page) =>
    index.answer(enricher, page).isEmpty &&
      (enricher.defersTmdbResolution || freshness.lastFetchedAt(services.tasks.EnrichDetailsTasks.pageAttempted(enricher.detailGroup, page)).isEmpty)
  }
  def request(listing: Listing): Unit = pageOf(listing).foreach { case (enricher, page) => ReadVenuePageTasks.enqueue(queue, enricher, page, clock.instant()) }
}
