package services.enrichment

import play.api.Logging
import services.movies.{CacheKey, MovieCache}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Common skeleton for the four `*Ratings` services (`ImdbRatings`,
 * `FilmwebRatings`, `MetascoreRatings`, `RottenTomatoesRatings`). Each one
 * needs:
 *
 *   1. A bounded worker pool draining bus-event-driven per-row refreshes.
 *   2. A single-thread scheduler firing a periodic `refreshAll` over the
 *      whole cache.
 *   3. `schedule(key)` / `refreshOneSync(key)` / `refreshOneSync(title,
 *      year)` entry points.
 *   4. `start()` / `stop()` lifecycle hooks owned by `AppLoader`.
 *
 * Per CLAUDE.md threshold-2 rule, these were extracted once the second copy
 * appeared (and lived as four copies for a while). Subclasses provide the
 * service-specific bits: the per-row `refreshOne` and the per-tick
 * `refreshAll`.
 *
 * Lifecycle is owned by the caller (`AppLoader` calls `start()` and
 * registers `stop()` as a shutdown hook). The base class never
 * self-subscribes or self-schedules.
 */
abstract class PeriodicCacheRefresher(
  name:                String,
  workers:             Int,
  startupDelaySeconds: Long,
  refreshHours:        Long,
  protected val cache: MovieCache
) extends Logging {

  private val worker           = DaemonExecutors.fixedPool(s"$name-stage", workers)
  private val refreshScheduler = DaemonExecutors.scheduler(s"$name-refresh")

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the worker pool. No-op when the row no
   *  longer exists or can't be resolved by the subclass. */
  private[services] def schedule(key: CacheKey): Unit =
    worker.execute(() => refreshOne(key))

  /** Synchronous version of `schedule` — used by scripts and tests. */
  private[services] def refreshOneSync(key: CacheKey): Unit = refreshOne(key)

  /** Synchronous refresh by `(title, year)` — public entry point for scripts. */
  def refreshOneSync(title: String, year: Option[Int]): Unit =
    refreshOne(cache.keyOf(title, year))

  /** Subclass hook: the per-row work. */
  protected def refreshOne(key: CacheKey): Unit

  /** Subclass hook: walk every cached row and apply per-row refresh. */
  private[services] def refreshAll(): Unit

  // ── Lifecycle ──────────────────────────────────────────────────────────────

  /** Schedule the periodic hourly refresh. Called from `AppLoader`. */
  def start(): Unit = {
    logger.info(s"$name refresh scheduled every ${refreshHours}h (first run in ${startupDelaySeconds}s).")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(refreshAll()).recover {
        case ex => logger.warn(s"$name refresh tick failed: ${ex.getMessage}")
      },
      startupDelaySeconds, refreshHours * 3600, TimeUnit.SECONDS
    )
  }

  /** Drain the worker pool so in-flight upserts hit Mongo before the repo
   *  closes its client. */
  def stop(): Unit = {
    worker.shutdown()
    refreshScheduler.shutdown()
    worker.awaitTermination(15, TimeUnit.SECONDS)
  }
}
