package services.enrichment

import play.api.Logging
import services.movies.{CacheKey, MovieCache}

import java.util.concurrent.ConcurrentHashMap

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
  recordBulkChange: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => (),
  // Effective confirmation deadband for a film's rating RIGHT NOW: how many
  // consecutive refreshes a NEW displayed value must be reported by before it's
  // committed to the row — the mechanism that absorbs the A→B→A rounding-boundary
  // flap (see [[RatingDeadband]]). Returns 1 ([[RatingDeadband.Off]]) to commit on
  // first sight. The worker wiring returns 2 ONLY while the film is on the base
  // (~2h) cadence — where the flap concentrates — and 1 once it has backed off, so
  // a genuine change on a slow-cadence film isn't held for days. Default off
  // (scripts/tests).
  deadbandConfirmationsFor: (CacheKey, Option[Int]) => Int = (_, _) => RatingDeadband.Off
) extends Logging {

  // Per-film pending rating candidate awaiting confirmation. In-memory: a lost
  // entry (worker restart, or a key whose refreshes hop machines) just means the
  // next new value re-starts its confirmation count — never a wrong or extra
  // write, only a possibly-missed suppression. Scoped to this refresher, so
  // `sourceName` needn't be part of the key.
  private val pendingRatings = new ConcurrentHashMap[String, RatingDeadband.Pending]()

  private def deadbandKey(key: CacheKey): String = s"${key.cleanTitle}|${key.year.getOrElse("")}"

  /** Deadband gate for a freshly-fetched DISPLAYED rating. Returns true iff the
   *  fresh value should be written to the row now; false when it's unchanged or
   *  held pending confirmation. Updates the per-film pending state as a side
   *  effect, so call it for EVERY fetched value (an unchanged one clears any
   *  outstanding candidate). `stored`/`fresh` are badge strings, so the
   *  comparison is exactly what the user sees; `tmdbId` locates the film's
   *  cadence so the deadband can engage only at the base interval. */
  protected def ratingSettled(key: CacheKey, tmdbId: Option[Int], stored: Option[String], fresh: Option[String]): Boolean = {
    val k        = deadbandKey(key)
    val confirmations = deadbandConfirmationsFor(key, tmdbId)
    val decision = RatingDeadband.decide(stored, fresh, Option(pendingRatings.get(k)), confirmations)
    decision.pending match {
      case Some(p) =>
        pendingRatings.put(k, p)
        logger.info(s"$sourceName: '${key.cleanTitle}' (${key.year.getOrElse("?")}) rating ${p.value} " +
                    s"held pending confirmation (${p.seen}/$confirmations)")
      case None => pendingRatings.remove(k)
    }
    decision.commit
  }

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
