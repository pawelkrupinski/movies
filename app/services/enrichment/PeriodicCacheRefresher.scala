package services.enrichment

import play.api.Logging
import services.Stoppable
import services.movies.{CacheKey, MovieCache}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.Try

/**
 * Common skeleton for the four `*Ratings` services (`ImdbRatings`,
 * `FilmwebRatings`, `MetascoreRatings`, `RottenTomatoesRatings`). Each one
 * needs:
 *
 *   1. A virtual-thread `ExecutionContext` draining bus-event-driven per-row
 *      refreshes (`schedule(key)` dispatches `refreshOne` on the EC).
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
  startupDelaySeconds: Long,
  refreshHours:        Long,
  protected val cache: MovieCache
) extends Stoppable with Logging {

  private val ec: ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC(s"$name-stage")
  private val refreshScheduler                    = DaemonExecutors.scheduler(s"$name-refresh")

  // ── Per-row refresh ────────────────────────────────────────────────────────

  /** Dispatch a single-row refresh on the virtual-thread EC. No-op when the
   *  row no longer exists or can't be resolved by the subclass. */
  private[services] def schedule(key: CacheKey): Unit = {
    Future(refreshOne(key))(using ec)
    ()
  }

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

  /** Drain in-flight per-row refreshes so URL discovery + rating scrapes
   *  finish before the caller moves on. Waits for the EC to terminate
   *  rather than capping at a fixed window — the bounded cap was
   *  returning before real-network per-row work finished, so recording
   *  scripts saw no MC / RT / FW fixtures captured. Play's lifecycle
   *  has its own deadline on top, so production's worst-case shutdown
   *  is still bounded. */
  def stop(): Unit = {
    ec.shutdown()
    refreshScheduler.shutdown()
    while (!ec.isTerminated) ec.awaitTermination(1, TimeUnit.HOURS)
  }
}
