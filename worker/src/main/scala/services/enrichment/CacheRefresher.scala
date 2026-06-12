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
abstract class CacheRefresher(protected val cache: MovieCache) extends Logging {

  /** Synchronous per-row refresh by `CacheKey` — the queue `RatingHandler`,
   *  scripts, and tests call this. */
  private[services] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous refresh by `(title, year)` — public entry point for the
   *  `RatingHandler` and scripts. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  /** Subclass hook: the per-row work. */
  protected def refreshOne(key: CacheKey): Unit

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
