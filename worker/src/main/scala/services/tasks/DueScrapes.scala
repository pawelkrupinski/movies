package services.tasks

import services.freshness.{FreshnessKind, FreshnessStore}
import services.cinemas.common.CinemaScraper

import java.time.Instant
import scala.concurrent.Future

/**
 * The roster's venues due a scrape, most overdue first — what a [[ScrapeReaper]] tick admits from.
 *
 * @param dueWindow the shared due schedule (each cinema scraped once per its freshness window,
 *                  phase-spread across it so all ~300 don't fall due together). The SAME instance
 *                  must back `ScrapeCinemaHandler` so this enqueue gate and that pickup re-gate
 *                  agree on what's due — see [[DueWindow]].
 * @param inFlight  cinemas whose scrape is already running are left OUT of the due set. A chunked
 *                  venue is not stamped until its run terminates (deliberately — see
 *                  [[ScrapeInFlight]]), so without this it stays most-overdue for its own duration
 *                  and the reaper re-admits it every tick into a no-op against the run mutex.
 */
final class DueScrapes(
  scrapers:  Seq[CinemaScraper],
  freshness: FreshnessStore,
  dueWindow: DueWindow,
  inFlight:  ScrapeInFlight = ScrapeInFlight.Never
) {

  /** How many venues the roster holds. */
  def rosterSize: Int = scrapers.size

  /** Completes once the scrape freshness stamps are loaded: before then every venue reads as never scraped. */
  def whenReady: Future[Unit] = freshness.whenReady(FreshnessKind.CinemaScrape)

  /** Whether the venue keyed `key` is due at `now`: what the `ScrapeCinema` handler re-checks a task by. */
  def isDue(key: String, now: Instant): Boolean = dueWindow.isDue(key, freshness.lastFetchedAt(key), now)

  /** Every venue due at `now` and not mid-scrape, as (dedup key, display name), oldest-fetched first.
   *
   *  `scrapers` is a fixed city→catalogue order, so serving it in that order would re-enqueue the front
   *  cinemas every tick under a cap and STARVE the tail while a backlog drains. Ordered by how long each
   *  has waited — oldest `lastFetchedAt` (never-fetched = oldest) first — the longest-overdue venue is
   *  always served next. Ties break on the dedup key, keeping the order deterministic (no clock/random in
   *  the ordering — the order must not depend on arrival). */
  def at(now: Instant): Vector[(String, String)] = {
    // Which venues are mid-scrape, read once for the tick rather than once per due venue.
    val running = inFlight.snapshot()
    scrapers.iterator
      .map(s => (ScrapeFreshnessPolicy.scrapeKey(s.cinema), s.cinema.displayName))
      .filter { case (key, _) => isDue(key, now) }
      .filterNot { case (_, cinemaName) => running(cinemaName) }
      .toVector
      // Oldest-fetched first; never-fetched (None) sorts ahead of any timestamp.
      .sortBy { case (key, _) =>
        (freshness.lastFetchedAt(key).map(_.toEpochMilli).getOrElse(Long.MinValue), key)
      }
  }
}
