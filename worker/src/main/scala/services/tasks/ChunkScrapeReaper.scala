package services.tasks

import play.api.Logging
import services.Stoppable
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.{DaemonExecutors, ScheduledTick}

import java.time.{Clock, Instant}
import java.util.concurrent.{ConcurrentHashMap, RejectedExecutionException, ScheduledExecutorService, TimeUnit}
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
 * Each deadline is a one-shot timer, armed by the planner as it starts the run
 * ([[armDeadline]]); at that point one run doc is read. A timer lives in this process
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
  store:        ChunkScrapeStore,
  queue:        TaskQueue,
  coordinator:  ChunkScrapeCoordinator,
  interval:     ChunkScrapeReaper.SweepInterval = ChunkScrapeReaper.SweepInterval(1.hour),
  initialDelay: ChunkScrapeReaper.InitialDelay = ChunkScrapeReaper.InitialDelay(45.seconds),
  staleAfter:   ChunkScrapePlanner.RunTimeout = ChunkScrapePlanner.RunTimeout(ChunkScrapePlanner.DefaultRunTimeout),
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock,
  scheduler:    ScheduledExecutorService = DaemonExecutors.scheduler("chunk-scrape-reaper")
) extends Stoppable with Logging {

  /** runIds with a deadline pending in this process, so a sweep does not arm a second. */
  private val armed = ConcurrentHashMap.newKeySet[String]()

  def start(): Unit = {
    // The boot sweep is unclaimed: this process's timers are gone, whoever swept this hour.
    scheduler.schedule((() => ScheduledTick.logged("ChunkScrapeReaper boot sweep", logger)(sweep())): Runnable,
      initialDelay.value.toMillis, TimeUnit.MILLISECONDS)
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.logged("ChunkScrapeReaper", logger)(sweepIfClaimed()),
      (initialDelay.value + interval.value).toMillis, interval.value.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ChunkScrapeReaper started — a deadline per chunked run, a sweep in ${initialDelay.value.toSeconds}s " +
      s"and then every ${interval.value.toMinutes}min.")
  }

  /** Arm `run`'s deadline in this process, unless it already has one here. */
  def armDeadline(run: ChunkRun): Unit =
    if (armed.add(run.runId)) {
      // `isStale` is strict, so the first stale instant is a millisecond past the deadline.
      val delay = math.max(0L, deadlineOf(run).toEpochMilli - clock.millis() + 1)
      try {
        scheduler.schedule((() => ScheduledTick.logged(s"ChunkScrapeReaper deadline ${run.cinema}", logger) {
          armed.remove(run.runId)
          atDeadline(run.cinema, run.runId)
        }): Runnable, delay, TimeUnit.MILLISECONDS)
        ()
      } catch { case _: RejectedExecutionException => armed.remove(run.runId); () } // stopping
    }

  /** The run's deadline has passed: reduce it if it is still the cinema's active run. */
  private def atDeadline(cinema: String, runId: String): Boolean =
    store.activeRun(cinema).filter(_.runId == runId).exists(reduceIfDue(_, clock.instant()))

  private[tasks] def sweepIfClaimed(): Int = {
    val key = OccurrenceKey.at("chunk-scrape", clock.millis(), interval.value, 0.seconds)
    if (runStore.claim(key)) sweep() else 0
  }

  /** Reduce every active run that is complete or past its deadline, and arm the deadline of
   *  every other. Returns
   *  how many reduces were enqueued. Public so tests drive it directly. */
  def sweep(): Int = {
    val now = clock.instant()
    val n = store.activeRuns().count(reduceIfDue(_, now))
    if (n > 0) logger.info(s"ChunkScrapeReaper enqueued $n reduce task(s).")
    n
  }

  /** The full reduce when every chunk landed (its last completion was lost), the partial
   *  one once past its deadline — and otherwise the deadline, armed. */
  private def reduceIfDue(run: ChunkRun, now: Instant): Boolean = {
    val stored = store.storedKeys(run.cinema, run.runId)
    if (run.expectedKeys.toSet.subsetOf(stored)) coordinator.maybeReduce(run.cinema, run.runId)
    else if (run.isStale(now, staleAfter.value)) {
      logger.warn(s"${run.cinema} run ${run.runId} abandoned (${stored.size}/${run.expectedKeys.size} chunks) — partial reduce")
      queue.enqueue(TaskType.ScrapeChunkReduce, ChunkScrapeKeys.reduceDedup(run.cinema, run.runId),
        ChunkScrapeKeys.reducePayload(run.cinema, run.runId)) == EnqueueResult.Added
    } else { armDeadline(run); false }
  }

  private def deadlineOf(run: ChunkRun): Instant = run.createdAt.plusMillis(staleAfter.value.toMillis)

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ChunkScrapeReaper {
  /** How often the backstop sweeps every run for ones whose deadline no live process holds. */
  final case class SweepInterval(value: FiniteDuration) extends AnyVal
  /** How long after `start()` the boot sweep runs. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal
}
