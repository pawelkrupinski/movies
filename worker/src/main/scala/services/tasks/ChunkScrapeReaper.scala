package services.tasks

import play.api.Logging
import tools.Stoppable
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.{ScheduledTick, TickRecorder}

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._

/**
 * The time-based half of a chunked scrape. The [[ChunkScrapeCoordinator]] reduces a run
 * the moment its last chunk finishes; what is left is one deadline per run:
 *
 *  - At `createdAt + staleAfter` a run that is still active is either COMPLETE (its last
 *    `TaskFinished` was lost — a restart, a dropped event) → its reduce is enqueued via the
 *    coordinator; or ABANDONED (a chunk that keeps failing, a lease that never came back)
 *    → a PARTIAL reduce over whatever landed, so one dead chunk degrades to a partial
 *    listing instead of losing the cinema. The next `ScrapeCinema` supersedes the run.
 *
 * Each deadline is a one-shot timer ([[ChunkRunDeadlines]]), armed by the planner as it starts the run
 * ([[ChunkRunDeadlines.arm]]); at that point one run doc is read. A timer lives in this process
 * only, so a SWEEP of every run doc recovers the ones whose timer died with a process (or
 * that another replica started): once at boot, by every process, and then hourly, by the
 * replica that claims the hour. It reduces the runs already complete or past their
 * deadline and arms the rest. This replaced a sweep every minute, which read every `scrape_runs` doc
 * sixty times an hour to act on a handful.
 *
 * No double reduce: a timer and a sweep (or two replicas) may both enqueue a run's reduce;
 * the queue drops the second by its dedup key, and the reduce handler skips a run that is
 * no longer active.
 */
class ChunkScrapeReaper(
  deadlines:    ChunkRunDeadlines,
  interval:     ChunkScrapeReaper.SweepInterval = ChunkScrapeReaper.SweepInterval(1.hour),
  initialDelay: ChunkScrapeReaper.InitialDelay = ChunkScrapeReaper.InitialDelay(45.seconds),
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock,
  scheduler:    ScheduledExecutorService,
  ticks:        TickRecorder = TickRecorder.Unrecorded
) extends Stoppable with Logging {

  def start(): Unit = {
    scheduler.schedule((() => ScheduledTick.logged("ChunkScrapeReaper boot sweep", logger)(sweep())): Runnable,
      initialDelay.value.toMillis, TimeUnit.MILLISECONDS)
    ticks.started("chunk-scrape-reaper", interval.value, initialDelay.value + interval.value)
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.recorded("chunk-scrape-reaper", interval.value, logger, ticks)(sweepIfClaimed().run),
      (initialDelay.value + interval.value).toMillis, interval.value.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ChunkScrapeReaper started — a deadline per chunked run, a sweep in ${initialDelay.value.toSeconds}s " +
      s"and then every ${interval.value.toMinutes}min.")
  }

  private[tasks] def sweepIfClaimed(): Enqueued = {
    val key = OccurrenceKey.at("chunk-scrape", clock.millis(), interval.value, 0.seconds)
    if (runStore.claim(key)) sweep() else Enqueued.Zero
  }

  /** [[ChunkRunDeadlines.sweep]]. Public so tests drive it directly. */
  def sweep(): Enqueued = deadlines.sweep()

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ChunkScrapeReaper {
  /** How often the backstop sweeps every run for ones whose deadline no live process holds. */
  final case class SweepInterval(value: FiniteDuration) extends AnyVal
  /** How long after `start()` the boot sweep runs. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal
}
