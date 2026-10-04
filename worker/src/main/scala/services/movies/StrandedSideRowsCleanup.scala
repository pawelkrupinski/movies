package services.movies

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import services.schedule.{OccurrenceKey, ScheduledRunStore}

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Daily tick: the retired-venue sweep ([[RetiredVenueRows]]) every day, and once a WEEK the
 * backstop that drops `screenings` / `movie_slots` rows whose film has no `movies`
 * document any more — [[MovieRepository.deleteStrandedSideRows]].
 *
 * Stranded rows are an EVENT's to remove: every delete of a film carries its side rows with it
 * (`MovieRepository.delete` cascades), and every film removal goes through it — the identity
 * projection retiring a film (`MovieCache.retireProjected`, a merge's loser or a film no venue
 * lists any more), `UnscreenedCleanup`'s delete, and another process's delete, which cascaded
 * in that process. A cascade that fails is retried by the next projection, which still holds the
 * film it could not retire. What no event covers, and the weekly backstop is for: a process that
 * died between deleting the `movies` document and its side rows, a cascade that failed on a path
 * nothing retries (`MovieCache.invalidate`), and a `screenings` row whose `movie_slots` twin went
 * alone. Each is rare, none serves anything (the web serves no stranded row), and a week is short
 * next to how long they used to sit — the sweep's own history is the 376 UK rows under 18 dead ids
 * that the deletes from before the cascade left behind (2026-09-07). See [[StrandedSideRows]] for the
 * rule and for what a sweep refuses to do.
 *
 * The retired-VENUE sweep stays daily: its rows are filed under a live film, so no film delete
 * removes them; dropping a venue from the roster is a deploy, and the sweep's 24h grace (a rolling
 * deploy's old pod must not delete the rows a new pod's venue writes) means a boot cannot act on
 * one either. `RetiredVenueRowsLingering` alerts after 50h — its grace plus a daily tick. Each
 * sweep is guarded separately, so one failing never skips the other. Then `afterSweeps` runs (the
 * wiring hands it the retired-venue census's `sample`), so the watchdog reads what the sweeps left
 * rather than holding its pre-sweep boot reading for up to an hour.
 *
 * Once shortly after boot, then every 24h, the hour-of-day drifting with each restart; the
 * stranded backstop runs on the tick `strandedDue` says ([[StrandedSideRowsCleanup.weekly]]).
 * Lifecycle owned by the wiring (`start()` schedules the tick; `stop()` runs at shutdown) — the
 * class never self-schedules. The scheduler is injected so a spec can hold the tick and run it.
 */
class StrandedSideRowsCleanup(
  repository:    MovieRepository,
  retiredVenues: () => RetiredVenueRows,
  afterSweeps:   () => Unit,
  strandedDue:   () => Boolean,
  scheduler:     ScheduledExecutorService = DaemonExecutors.scheduler("stranded-side-rows-cleanup")
) extends Stoppable with Logging {

  private val RunEveryHours       = 24L
  // Off the boot window, in its own slot past the other whole-collection readers: rows that have sat
  // stranded for weeks can wait a few minutes more.
  private val StartupDelaySeconds = services.metrics.SampledCensus.firstDelay(services.metrics.SampledCensus.Slots.StrandedSideRows,
    scala.concurrent.duration.Duration(RunEveryHours, "hours")).toSeconds

  /** One sweep. Public so a script or a spec can run it on demand; the tick calls the same
   *  method when `strandedDue`. */
  def removeStranded(): StrandedSideRows = repository.deleteStrandedSideRows()

  /** One retired-venue sweep, on demand — the daily tick runs it after [[removeStranded]]'s turn. */
  def removeRetiredVenues(): RetiredVenueRows = retiredVenues()

  def start(): Unit = {
    logger.info(s"Side-row cleanup scheduled every ${RunEveryHours}h, its stranded-film backstop weekly " +
      s"(first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => {
        Try(if (strandedDue()) { removeStranded(); () }).recover {
          case exception => logger.warn(s"Stranded side-row cleanup tick failed: ${exception.getMessage}")
        }
        Try(removeRetiredVenues()).recover {
          case exception => logger.warn(s"Retired-venue side-row cleanup tick failed: ${exception.getMessage}")
        }
        Try(afterSweeps()).recover {
          case exception => logger.warn(s"Side-row cleanup's after-sweep report failed: ${exception.getMessage}")
        }
        ()
      },
      StartupDelaySeconds, RunEveryHours * 3600, TimeUnit.SECONDS
    )
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object StrandedSideRowsCleanup {
  private val Day = 1.day

  /** Whether the stranded backstop is due on a tick at `clock`'s now: on one UTC day in seven (a Thursday,
   *  day 0 of the epoch's weeks), on the first tick that day to win `runStore`'s claim. The daily tick runs
   *  every 24h from each boot, so it lands on every calendar day the worker is up; a day-keyed claim,
   *  where a week-keyed one would outlive the claim store's 48h retention and be won again. */
  def weekly(runStore: ScheduledRunStore, clock: Clock): () => Boolean = () => {
    val now = clock.millis()
    Math.floorMod(Math.floorDiv(now, Day.toMillis), 7L) == 0L &&
      runStore.claim(OccurrenceKey.at("stranded-side-rows-backstop", now, Day, Duration.Zero))
  }
}
