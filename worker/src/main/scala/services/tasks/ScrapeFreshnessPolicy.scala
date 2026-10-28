package services.tasks

import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}

import scala.concurrent.duration.FiniteDuration

import java.time.Clock
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * Decides whether a finished scrape attempt advances the cinema's due schedule —
 * i.e. when to stamp [[services.freshness.FreshnessStore]] so [[DueWindow]] stops
 * calling the cinema due. One place, because the same rule must hold for the plain
 * scrape ([[ScrapeCinemaHandler]]) and for both terminal outcomes of a chunked one
 * ([[ChunkScrapePlanner]]'s empty/failed plan, [[ScrapeChunkReduceHandler]]'s
 * reduce) — they drifted apart, and the drift was an outage.
 *
 * The rule:
 *
 *  - **Success — including a legitimately EMPTY listing — always stamps.** A venue
 *    that parsed cleanly and simply has nothing on is up to date; it is not a
 *    failure and must wait its normal window like any other. The plain path already
 *    got this right (an empty `fetch()` returns normally, so the handler stamps);
 *    the chunked planner did not, and that asymmetry is what broke Germany.
 *  - **Failure leaves the cinema stale — but only for `immediateRetries` ticks.**
 *    Staying stale is what gives a transient 5xx a fast retry at the reaper's
 *    cadence instead of waiting a whole freshness window. Past the budget the
 *    failure is no longer plausibly transient, so we stamp anyway and let the venue
 *    come round again on the normal window.
 *
 * WHY the budget exists (prod, 2026-07-24 → 07-27, `kinowo_de` + `kinowo_uk`): a
 * never-stamped cinema has `lastFetchedAt = None`, and [[ScrapeReaper]] orders the
 * due set oldest-first with never-fetched ahead of every timestamp. So permanently
 * broken venues sort to the FRONT of every single tick and consume the whole
 * [[ScrapeCadence.MaxEnqueuePerTick]] budget (40) forever, while every healthy
 * cinema behind them is never enqueued at all. Germany had ~40 venues advertising
 * zero showtime days and the UK ~40 returning 404: both countries scraped the same
 * broken 40 once a minute for days and none of their ~1000 working cinemas even
 * once. Bounding the retries turns a permanently broken venue from a permanent
 * queue-jumper into an ordinary hourly one.
 *
 * The streak counter is in-memory on purpose: it only shortens a retry sequence, so
 * losing it on restart costs at most `immediateRetries` extra attempts per venue —
 * not worth a Mongo round-trip per scrape.
 */
class ScrapeFreshnessPolicy(
  freshness:        FreshnessStore,
  // Consecutive failures a cinema may retry at the reaper's tick cadence before it
  // is parked back onto the normal freshness window. Small: the scrapers already
  // retry within one attempt (`CinemaScraper.maxFetchAttempts`), so this is the
  // second, coarser layer — enough to ride out a blip, not enough to camp the queue.
  immediateRetries: Int   = 2,
  clock:            Clock,
  // Where a landed scrape's remaining-showtime runway feeds a shortened next
  // interval for THIN venues (see `VenueScrapeCadence`) — `None` (the default)
  // leaves cadence untouched, so every existing caller/test that doesn't pass a
  // `remainingHorizon` to `succeeded` is unaffected. Production wires the same
  // instance the worker's `scrapeDueWindow` reads.
  venueCadence:     Option[VenueCadenceStore] = None,
  // Where a venue whose scrape failed on the residential-proxy leg is parked while the proxy is down, and released
  // from the moment it is back (see `ProxyParking`). None where there is no proxy: every failure is the venue's own.
  proxyParking:     Option[ProxyParking] = None
) extends Logging {

  private val consecutiveFailures = new ConcurrentHashMap[String, Integer]()

  /** The scrape produced a listing (possibly empty) — stamp it and forget any
   *  failure streak. `remainingHorizon`, when the caller has one, records how
   *  much showtime runway that listing leaves so a THIN venue's next interval
   *  can shorten below the country default — see `VenueScrapeCadence`. `None`
   *  (a GoneUpstream skip, a call site that hasn't been taught to measure it)
   *  leaves whatever cadence this venue already had untouched, never resets it
   *  back to the country default. */
  def succeeded(key: String, remainingHorizon: Option[FiniteDuration] = None): Unit = {
    consecutiveFailures.remove(key)
    proxyParking.foreach(_.released(key))
    freshness.markFresh(key, FreshnessKind.CinemaScrape, clock.instant())
    for (store <- venueCadence; horizon <- remainingHorizon) store.record(key, horizon)
  }

  /** The scrape was deliberately SKIPPED rather than run — the venue's page is
   *  gone (see [[services.scrapes.GoneUpstream]]). Stamps exactly like a success,
   *  because the scheduling consequence is identical: this cinema is not due again
   *  until its next window. Named apart so a call site that ran no scrape at all
   *  does not have to claim one succeeded. */
  def skipped(key: String): Unit = succeeded(key)

  /** The scrape threw. Leave the cinema stale so the reaper retries next tick,
   *  until the streak passes `immediateRetries` — then stamp anyway so it stops
   *  jumping the queue ahead of healthy cinemas. When `error` failed on the
   *  residential-proxy leg and the proxy's probe says it is down, the venue is
   *  ALSO parked on that cause, so [[releaseRecoveredProxy]] makes it due again the
   *  moment the proxy is back instead of a whole freshness window later. The stamp
   *  stays either way: should the probe never pass, the venue still comes round on
   *  its normal window, as before. */
  def failed(key: String, error: Option[Throwable] = None): Unit = {
    val streak = consecutiveFailures.merge(key, 1, (a, b) => a + b)
    if (streak > immediateRetries) {
      val onProxy = error.exists(tools.AllBackendsFailedException.legFailed(_, tools.ProxyChain.ProxyLeg)) &&
        proxyParking.exists(_.parkIfProxyDown(key))
      if (onProxy)
        logger.warn(s"$key failed $streak consecutive scrapes on the residential proxy, which its probe says is down; " +
          "parking it on the proxy's recovery (and on the normal freshness window, should that come first).")
      else
        logger.warn(s"$key failed $streak consecutive scrapes; parking it on the normal " +
          "freshness window so it stops consuming the reaper's per-tick budget.")
      freshness.markFresh(key, FreshnessKind.CinemaScrape, clock.instant())
    }
  }

  /** The recovery check (`ScrapeWiring.proxyParkingRecovery`, every `ProxyParking.ProbeInterval`): when the
   *  proxy's probe passes again, every venue parked on it is made due NOW — its stamp dropped, so the reaper takes
   *  it first, at its usual per-tick cap and spread (the drain is paced by the reaper, not stampeded) — with a fresh
   *  retry budget. Returns how many were released. */
  def releaseRecoveredProxy(): Int = proxyParking.fold(0) { parking =>
    val released = parking.releaseIfRecovered()
    released.foreach { key =>
      consecutiveFailures.remove(key)
      freshness.invalidate(key)
    }
    if (released.nonEmpty)
      logger.warn(s"residential proxy recovered: un-parked ${released.size} cinema(s), due for scraping now " +
        s"(${released.take(5).mkString(", ")}${if (released.size > 5) ", ..." else ""})")
    released.size
  }

  /** Venues parked right now, by cause (`ProxyParking.Cause`): on the proxy, or on their own failure streak. */
  def parkedByCause: Map[String, Int] = {
    val own = consecutiveFailures.asScala.count { case (key, streak) =>
      streak > immediateRetries && !proxyParking.exists(_.isParked(key))
    }
    Map(ProxyParking.Cause.ResidentialProxyDown -> proxyParking.fold(0)(_.size), ProxyParking.Cause.OwnFailures -> own)
  }
}
