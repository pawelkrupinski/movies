package services.sharecards

import play.api.Logging
import services.facebook.{AppUsage, FacebookGraph, FacebookScrape, FacebookUsageBrake}

import java.time.Clock

/** A country's due pages in the fleet's re-scrape queue sent to Facebook -- ONE a tick, when the fleet's next slot has
 *  come round, one Facebook holds a preview of before any it never fetched. Sent: gone, and when Facebook says the
 *  app's hourly allowance is nearly spent the fleet waits ([[FacebookUsageBrake]]) before the next. Rate-limited: the
 *  whole fleet's quota waits [[RescrapePacing.RateLimitHold]] and the page goes back without spending an attempt.
 *  Unavailable (unreachable, a 5xx): the same for [[RescrapePacing.UnavailableHold]]. Refused: that page alone retries,
 *  backing off, and is dropped after [[RescrapePacing.MaxAttempts]]. */
final class RescrapePageSender(store: FacebookRescrapeStore, graph: FacebookGraph, brake: FacebookUsageBrake, country: String,
                               metrics: ShareCardMetrics, clock: Clock) extends Logging {
  import RescrapePacing.*

  /** Send the page due, if the fleet's slot has come round, then report how many of the country's pages wait. */
  def sendDue(): Unit = {
    sendPage()
    metrics.rescrapesWaiting(store.waitingPages(country))
  }

  private def urlOf(page: RescrapeEntry): String = page.target match { case RescrapeTarget.Page(u, _) => u; case other => other.key }

  private def sendPage(): Unit = {
    val now = clock.instant()
    // The pages Facebook holds a preview of first; the ones it never fetched with what is left.
    for {
      kind <- RescrapeKind.Pages.find(store.hasDue(country, _, now)) if store.takeSlot(now, Spacing, TickEvery)
      page <- store.claim(country, kind, now, Lease)
    } {
      val url   = urlOf(page)
      val reply = graph.scrape(url)
      reply.outcome match {
        case FacebookScrape.Accepted =>
          metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Sent)
          store.complete(page)
        case FacebookScrape.RateLimited(why) =>
          metrics.rescrape(ShareCardMetrics.RescrapeOutcome.RateLimited)
          val until = clock.instant().plusMillis(RateLimitHold.toMillis)
          logger.warn(s"share card: Facebook rate-limited the re-scrape of $url ($why) — every country waits until $until")
          store.holdSlots(until)
          store.retry(page, until, countAttempt = false)
        case FacebookScrape.Unavailable(why) =>
          metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
          val until = clock.instant().plusMillis(UnavailableHold.toMillis)
          logger.info(s"share card: Facebook unavailable for the re-scrape of $url ($why) — every country waits until $until")
          store.holdSlots(until)
          store.retry(page, until, countAttempt = false)
        case FacebookScrape.Refused(why) if page.attempts >= MaxAttempts =>
          metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
          logger.warn(s"share card: Facebook re-scrape of $url dropped after ${page.attempts} attempts: $why")
          store.complete(page)
        case FacebookScrape.Refused(why) =>
          metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
          logger.info(s"share card: Facebook re-scrape of $url failed: $why")
          store.retry(page, clock.instant().plusMillis(backoff(page.attempts).toMillis), countAttempt = true)
      }
      // After the page's own outcome is stored: a hold the fleet database refuses fails the tick,
      // and must not leave a page Facebook accepted claimed, to be sent again.
      reply.usage.foreach(paceOn)
    }
  }

  /** Facebook's word on how much of the app's hourly allowance is spent, acted on by the fleet's
   *  [[FacebookUsageBrake]] — the same one the web's Facebook sign-ins report to. */
  private def paceOn(usage: AppUsage): Unit = {
    metrics.appUsage(usage.percent)
    brake.observe(usage).foreach(until =>
      logger.info(s"share card: Facebook says ${usage.percent}% of the app's hourly allowance is spent — every country waits until $until"))
  }
}
