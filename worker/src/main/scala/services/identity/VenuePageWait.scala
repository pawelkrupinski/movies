package services.identity

import services.cinemas.common.DetailEnricher
import services.tasks.TaskQueue
import services.venuepages.ReadVenuePageTasks

import scala.concurrent.duration.FiniteDuration

/** A cut-over country's [[PageWait]]: a listing at a venue with a detail page waits while that page has
 *  no answer in venue_pages (`VenuePageIndex`), and its page is read by a `ReadVenuePage` task. */
final class VenuePageWait(enrichers: Seq[DetailEnricher], index: VenuePageIndex, queue: TaskQueue, val limit: FiniteDuration)
    extends PageWait {
  private val enricherOf = enrichers.map(e => e.cinema -> e).toMap

  private def pageOf(listing: Listing): Option[(DetailEnricher, String)] =
    for { page <- listing.page; enricher <- enricherOf.get(listing.cinema) } yield enricher -> page

  def awaiting(listing: Listing): Boolean = pageOf(listing).exists { case (enricher, page) => index.answer(enricher, page).isEmpty }
  def request(listing: Listing): Unit = pageOf(listing).foreach { case (enricher, page) => ReadVenuePageTasks.enqueue(queue, enricher, page) }
}
