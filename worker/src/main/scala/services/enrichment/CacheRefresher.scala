package services.enrichment

import scala.util.{Failure, Success, Try}

import models.CacheKey
import models.MovieRecord
import play.api.Logging
import services.movies.MovieCache
import services.tasks.BulkRefreshResult

/**
 * What every rating source's refresher answers, whatever the source: [[ImdbRatings]] (the IMDb id another stage
 * resolves) and [[SiteRatings]] (every URL-keyed rating site, over its port, [[SiteRatings.Site]]).
 *
 *   1. A per-row refresh entry point (`refreshOneSync`) — the queue's
 *      `RatingHandler` calls this for one row, and scripts/tests call it too.
 *   2. A full-corpus refresh (`refreshAllNow`) — the operator-triggered
 *      `/tasks` bulk-refresh button (the worker's `BulkRefreshHandler`).
 *   3. The re-check of given rows (`recheck`) — `services.refresh.RatingRefreshSource`.
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

  /** The re-check an operator asks for, for given rows, outside the queue (`services.refresh.RatingRefreshSource`):
   *  what the full-corpus walk ([[refreshAll]]) does to each row, for these rows alone. This default is
   *  [[refreshOneSync]] per row, a displayed value it moved reported to the adaptive cadence under the row's
   *  `tmdbId` as the full-corpus walk reports one (the queue's `RatingHandler` reports through its task; this path
   *  has none, and a move the cadence never heard of is the gap a later per-row refresh mis-reads as a fresh
   *  change), and a row whose refresh throws (the site down for it, a page that will not parse) counted as failed
   *  while the rest are still asked. A URL-keyed source walks the rows as its full-corpus walk does
   *  ([[SiteRatings]]): its per-row refresh trusts the stored URL, so a re-check through it could never correct a
   *  wrong one. */
  private[services] def recheck(rows: Seq[(CacheKey, MovieRecord)]): BulkRefreshResult = {
    val outcomes = rows.map { case (key, record) =>
      Try(refreshOneSync(key)) match {
        case Success(moved) =>
          moved.foreach(value => recordCadenceChange(key, record.tmdbId, Some(value)))
          Some(moved.isDefined)
        case Failure(exception) =>
          logger.warn(s"$sourceName: re-check of '${key.cleanTitle}' (${key.year.getOrElse("?")}) failed: ${exception.getMessage}")
          None
      }
    }
    val (changed, failed) = (outcomes.count(_.contains(true)), outcomes.count(_.isEmpty))
    BulkRefreshResult.counts(walked = rows.size, changed = changed, discovered = 0, failed = failed,
      message = s"$changed of ${rows.size} row(s) changed, $failed failed")
  }

  /** Subclass hook: the per-row work. Returns the new displayed value if it moved. */
  protected def refreshOne(key: CacheKey): Option[String]

  /** Subclass hook: walk every cached row and apply per-row refresh, returning a
   *  summary of what the walk did (walked / changed / discovered / failed +
   *  message) so the operator-triggered path can persist and display it. */
  private[services] def refreshAll(): BulkRefreshResult

  /** Public entry point to run a full refresh now — the operator-triggered
   *  `/tasks` button path (the worker's `BulkRefreshHandler` calls this). Wraps
   *  the `private[services]` walk so callers outside the `services` package
   *  (the `modules` composition root) can kick one off. */
  def refreshAllNow(): BulkRefreshResult = refreshAll()

  /** Per-source concurrency cap for a parallel full-corpus walk (see [[tools.BoundedParallel]]). Default 8; override
   *  lower for an upstream that soft-blocks under load (Filmweb). */
  protected def refreshConcurrency: Int = 8

  /** `step` over every one of `rows`, [[refreshConcurrency]] at a time on threads named after `pool`
   *  ([[tools.BoundedParallel]]): how every source's walk runs. A step that throws loses the whole batch, so each
   *  source's step catches its own row's failure. */
  protected def walkEach[A](pool: String, rows: Seq[A])(step: A => Unit): Unit =
    tools.BoundedParallel.foreach(pool, rows, refreshConcurrency)(step)
}
