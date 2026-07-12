package services.enrichment

import play.api.Logging
import services.movies.{CacheKey, MovieCache}

/**
 * Common skeleton for the four `*Ratings` services (`ImdbRatings`,
 * `FilmwebRatings`, `MetascoreRatings`, `RottenTomatoesRatings`). Each one
 * needs:
 *
 *   1. A per-row refresh entry point (`refreshOneSync`) — the queue's
 *      `RatingHandler` calls this for one row, and scripts/tests call it too.
 *   2. A full-corpus refresh (`refreshAllNow`) — the operator-triggered
 *      `/tasks` bulk-refresh button (the worker's `BulkRefreshHandler`).
 *
 * Per CLAUDE.md threshold-2 rule, these were extracted once the second copy
 * appeared. Subclasses provide the service-specific bits: the per-row
 * `refreshOne` and the full-corpus `refreshAll`.
 *
 * No lifecycle of its own: rating refresh is driven by the queue
 * (`RatingHandler` per row, the `EnrichmentReaper` as the periodic backstop),
 * so the refresh runs synchronously on the caller's thread (the `TaskWorker`
 * pool in production) — no EC or scheduler to own here.
 */
abstract class CacheRefresher(
  protected val cache: MovieCache,
  // Sink for a displayed-value change OBSERVED BY THE FULL-CORPUS WALK. The
  // per-row path records into the adaptive cadence via `RatingHandler` (which
  // carries the task's dedup key); the bulk walk has no task, so the composition
  // root injects a recorder that builds the `(source, film)` cadence key from the
  // row's `CacheKey` + tmdbId. Without it an operator's corpus refresh would move
  // a rating without telling the cadence, leaving a gap a later per-row refresh
  // mis-reads as a fresh change. No-op by default (scripts/tests).
  recordBulkChange: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends Logging {

  /** Record a displayed-value change the full-corpus walk just made, so the
   *  adaptive cadence's change history stays complete across an operator bulk
   *  refresh. Subclasses call this from `refreshAll` at each write site; the
   *  cadence itself dedups a value equal to the last recorded one. */
  protected def recordCadenceChange(key: CacheKey, tmdbId: Option[Int], displayValue: Option[String]): Unit =
    recordBulkChange(key, tmdbId, displayValue)

  /** Short name of the source this refresher owns (`"IMDb"`, `"Metacritic"`,
   *  `"RT"`, `"Filmweb"`) — used to prefix the per-step enrichment logs so a
   *  film's whole journey through the cascade is greppable by source. */
  protected def sourceName: String

  /** Synchronous per-row refresh by `CacheKey` — the queue `RatingHandler`,
   *  scripts, and tests call this. Logs the step boundary at INFO so every
   *  per-film rating resolution is visible; the subclass logs the outcome.
   *  Returns `Some(newDisplayValue)` when the DISPLAYED value moved this refresh
   *  (the badge text it became), else `None` — the signal the adaptive
   *  [[services.cadence.RatingCadence]] backs off on. */
  private[services] def refreshOneSync(key: CacheKey): Option[String] = {
    logger.info(s"$sourceName: resolving '${key.cleanTitle}' (${key.year.getOrElse("?")})")
    refreshOne(key)
  }

  /** Synchronous refresh by `(title, year)` — public entry point for the
   *  `RatingHandler` and scripts. Returns the new displayed value if it moved. */
  def refreshOneSync(title: String, year: Option[Int]): Option[String] =
    refreshOneSync(cache.keyOf(title, year))

  /** Subclass hook: the per-row work. Returns the new displayed value if it moved. */
  protected def refreshOne(key: CacheKey): Option[String]

  /** Subclass hook: walk every cached row and apply per-row refresh. */
  private[services] def refreshAll(): Unit

  /** Public entry point to run a full refresh now — the operator-triggered
   *  `/tasks` button path (the worker's `BulkRefreshHandler` calls this). Wraps
   *  the `private[services]` walk so callers outside the `services` package
   *  (the `modules` composition root) can kick one off. */
  def refreshAllNow(): Unit = refreshAll()

  /** Per-source concurrency cap for the parallel `refreshAll` walk (see
   *  [[tools.BoundedParallel]]). Default 8; override lower for an upstream that
   *  soft-blocks under load (Filmweb). */
  protected def refreshConcurrency: Int = 8
}
