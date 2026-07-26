package services.tasks

import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}

import java.time.Clock
import java.util.concurrent.ConcurrentHashMap

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
  clock:            Clock = Clock.systemUTC()
) extends Logging {

  private val consecutiveFailures = new ConcurrentHashMap[String, Integer]()

  /** The scrape produced a listing (possibly empty) — stamp it and forget any
   *  failure streak. */
  def succeeded(key: String): Unit = {
    consecutiveFailures.remove(key)
    freshness.markFresh(key, FreshnessKind.CinemaScrape, clock.instant())
  }

  /** The scrape threw. Leave the cinema stale so the reaper retries next tick,
   *  until the streak passes `immediateRetries` — then stamp anyway so it stops
   *  jumping the queue ahead of healthy cinemas. */
  def failed(key: String): Unit = {
    val streak = consecutiveFailures.merge(key, 1, (a, b) => a + b)
    if (streak > immediateRetries) {
      logger.warn(s"$key failed $streak consecutive scrapes; parking it on the normal " +
        "freshness window so it stops consuming the reaper's per-tick budget.")
      freshness.markFresh(key, FreshnessKind.CinemaScrape, clock.instant())
    }
  }
}
