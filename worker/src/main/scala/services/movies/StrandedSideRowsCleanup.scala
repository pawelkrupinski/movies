package services.movies

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.util.Try

/**
 * Daily tick: drop the `screenings` / `movie_slots` rows whose film has no `movies`
 * document any more — [[MovieRepository.deleteStrandedSideRows]], on a schedule.
 *
 * The rows are the leftovers of deletes and merges from before the cascade and
 * [[SideCollectionMove]] carried side rows, and nothing else ever clears them: the worker's
 * served-films census counts them as films, the web can serve none of them, and the
 * invariant `FilmIdentityInvariantsSpec` proves in memory (side rows ⊆ live ids) did not
 * hold in the store. On 2026-09-07 the UK corpus held 376 such `screenings` rows under 18
 * dead ids, 169 of them with future showtimes. See [[StrandedSideRows]] for the rule and
 * for what a sweep refuses to do.
 *
 * Same shape as [[UnscreenedCleanup]]: once shortly after boot, then every 24h, the
 * hour-of-day drifting with each restart. Lifecycle owned by the wiring (`start()`
 * schedules the tick; `stop()` runs at shutdown) — the class never self-schedules. The
 * scheduler is injected so a spec can hold the tick and run it by hand.
 */
class StrandedSideRowsCleanup(
  repository: MovieRepository,
  scheduler:  ScheduledExecutorService = DaemonExecutors.scheduler("stranded-side-rows-cleanup")
) extends Stoppable with Logging {

  // Off the boot window: the cache hydrate and the projector's state seed own the first
  // couple of minutes, and rows that have sat stranded for weeks can wait two more.
  private val StartupDelaySeconds = 120L
  private val RunEveryHours       = 24L

  /** One sweep. Public so a script or a spec can run it on demand; the daily tick calls
   *  the same method. */
  def removeStranded(): StrandedSideRows = repository.deleteStrandedSideRows()

  def start(): Unit = {
    logger.info(s"Stranded side-row cleanup scheduled every ${RunEveryHours}h (first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => Try(removeStranded()).recover {
        case exception => logger.warn(s"Stranded side-row cleanup tick failed: ${exception.getMessage}")
      },
      StartupDelaySeconds, RunEveryHours * 3600, TimeUnit.SECONDS
    )
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}
