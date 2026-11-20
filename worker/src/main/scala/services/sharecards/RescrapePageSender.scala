package services.sharecards

import java.time.Clock

import services.facebook.FacebookGraph

/** A country's due pages in the fleet's re-scrape queue sent to Facebook -- ONE a tick, when the fleet's next slot has
 *  come round, one Facebook holds a preview of before any it never fetched. What each answer does to the page and to
 *  the fleet's pace is [[RescrapeReplies]]'. */
final class RescrapePageSender(store: FacebookRescrapeStore, graph: FacebookGraph, replies: RescrapeReplies, country: String,
                               metrics: ShareCardMetrics, clock: Clock) {
  import RescrapePacing.*

  /** Send the page due, if the fleet's slot has come round, then report how many of the country's pages wait. */
  def sendDue(): Unit = {
    sendPage()
    metrics.rescrapesWaiting(store.waitingPages(country))
  }

  private def sendPage(): Unit = {
    val now = clock.instant()
    // The pages Facebook holds a preview of first; the ones it never fetched with what is left.
    for {
      kind <- RescrapeKind.Pages.find(store.hasDue(country, _, now)) if store.takeSlot(now, Spacing, TickEvery)
      page <- store.claim(country, kind, now, Lease)
    } replies.answered(page, graph.scrape(page.url))
  }
}
