package services.enrichment

import models.MovieRecord
import play.api.Logging
import services.movies.{CacheKey, MovieCache}
import services.tasks.BulkRefreshResult
import tools.BoundedParallel

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try}

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

  /** Subclass hook: walk every cached row and apply per-row refresh, returning a
   *  summary of what the walk did (walked / changed / discovered / failed +
   *  message) so the operator-triggered path can persist and display it. */
  private[services] def refreshAll(): BulkRefreshResult

  /** Public entry point to run a full refresh now — the operator-triggered
   *  `/tasks` button path (the worker's `BulkRefreshHandler` calls this). Wraps
   *  the `private[services]` walk so callers outside the `services` package
   *  (the `modules` composition root) can kick one off. */
  def refreshAllNow(): BulkRefreshResult = refreshAll()

  /** Per-row write of a score just read off the row's URL: persist it when it
   *  differs from what the row holds and return the badge it became — the
   *  displayed-value change the cadence keys on — else `None`. The updater
   *  receives the LIVE cached row: that's the merge point for both a URL just
   *  written and any other listener's concurrent update. */
  protected def persistIfMoved[A](
    key: CacheKey, url: String, noun: String, stored: Option[A], fresh: A,
    withScore: (MovieRecord, Option[A]) => MovieRecord, badge: A => String
  ): Option[String] = {
    val commit = !stored.contains(fresh)
    logger.info(s"$sourceName: '${key.cleanTitle}' (${key.year.getOrElse("?")}) $url → $noun $fresh" +
      (if (commit) s" (was ${stored.getOrElse("—")})" else " (unchanged)"))
    if (commit) { cache.putIfPresent(key, withScore(_, Some(fresh))); Some(badge(fresh)) }
    else None
  }

  /** Per-source concurrency cap for the parallel `refreshAll` walk (see
   *  [[tools.BoundedParallel]]). Default 8; override lower for an upstream that
   *  soft-blocks under load (Filmweb). */
  protected def refreshConcurrency: Int = 8

  /** The full-corpus walk of a URL-keyed source (Metacritic, RT, Filmweb): ONE
   *  pass, two steps per row — re-derive the row's URL, then re-read the score
   *  off whatever URL the row NOW holds.
   *
   *  It used to be two passes split on whether the row already had a URL, which
   *  made a stored URL permanently authoritative: the operator's button could
   *  only re-scrape the score off whatever was there, so a WRONG URL was never
   *  corrected and the run reported "0 changed" while films sat on another
   *  film's page.
   *
   *  Both steps run, deliberately. Re-resolving ALONE would be a regression: a
   *  row whose re-resolution fails (transient, or the site genuinely has no
   *  page) would stop refreshing its score at all — which is what two specs
   *  caught.
   *
   *  Each score write goes through `cache.putIfPresent` (the per-title lock),
   *  and a moved score is reported to the adaptive cadence under the SNAPSHOT
   *  row's tmdbId. A failed step — a `rediscoverUrl` that answers `Failure`, a
   *  `fetchScore` that throws — counts the row as failed and leaves what the
   *  row holds alone; a row whose re-resolution failed still refreshes its
   *  score off the URL it already had.
   *
   *  @param walkLabel     log prefix ("RT refresh"); also names the pool.
   *  @param urlOf         the row's stored URL for this source.
   *  @param scoreOf       the row's stored score for this source.
   *  @param rediscoverUrl step 1 — re-derive and persist the URL. Runs only for
   *                       rows with a tmdbId; `Success(true)` when a URL was
   *                       found.
   *  @param fetchScore    step 2 — read the score off a URL.
   *  @param withScore     write a fresh score onto the live row.
   *  @param badge         the displayed value a fresh score becomes — what the
   *                       cadence is told.
   *  @param changedNoun   what the summary line counts as changed.
   */
  protected def refreshAllUrlThenScore[A](
    walkLabel:     String,
    urlOf:         MovieRecord => Option[String],
    scoreOf:       MovieRecord => Option[A],
    rediscoverUrl: (CacheKey, MovieRecord) => Try[Boolean],
    fetchScore:    String => Option[A],
    withScore:     (MovieRecord, Option[A]) => MovieRecord,
    badge:         A => String,
    changedNoun:   String = "score(s)"
  ): BulkRefreshResult = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val resolvable = snapshot.count { case (_, e) => e.tmdbId.isDefined }
    logger.info(s"$walkLabel: starting tick over ${snapshot.size} cached row(s) " +
                s"($resolvable re-resolving their URL first).")
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)

    BoundedParallel.foreach(walkLabel.replace(' ', '-'), snapshot, refreshConcurrency) { case (key, enrichment) =>
      // 1. Re-derive the URL when the row has a tmdbId to derive it from. A
      //    better match replaces the stored one; a failure leaves it be.
      if (enrichment.tmdbId.isDefined) rediscoverUrl(key, enrichment) match {
        case Success(true)  => urlDiscovered.incrementAndGet()
        case Success(false) => ()
        case Failure(exception) =>
          failed.incrementAndGet()
          logger.debug(s"$walkLabel: ${key.cleanTitle} lookup failed: ${exception.getMessage}")
      }

      // 2. Refresh the score off whatever URL the row NOW holds — possibly the
      //    one just re-resolved, possibly the pre-existing one.
      val current = cache.get(key).getOrElse(enrichment)
      urlOf(current).foreach { url =>
        Try(fetchScore(url)) match {
          case Success(fresh) if fresh != scoreOf(current) =>
            logger.debug(s"$walkLabel: ${key.cleanTitle} $url ${scoreOf(current).getOrElse("—")} → ${fresh.getOrElse("—")}")
            cache.putIfPresent(key, withScore(_, fresh))
            fresh.foreach(s => recordCadenceChange(key, enrichment.tmdbId, Some(badge(s))))
            changed.incrementAndGet()
          case Success(_) => ()
          case Failure(exception) =>
            failed.incrementAndGet()
            logger.debug(s"$walkLabel: $url lookup failed: ${exception.getMessage}")
        }
      }
    }

    val took = System.currentTimeMillis() - startedAt
    val message = s"tick done in ${took}ms — ${changed.get} $changedNoun changed, " +
                  s"${urlDiscovered.get} URL(s) newly discovered, ${failed.get} failed."
    logger.info(s"$walkLabel: $message")
    BulkRefreshResult.counts(walked = snapshot.size, changed = changed.get, discovered = urlDiscovered.get, failed = failed.get, message = message)
  }
}
