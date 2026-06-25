package services.tasks

import play.api.Logging
import services.Stoppable
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.Clock
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodic backstop for chunked scrapes, mirroring `StagingReaper`'s tick. The
 * coordinator (event-driven) handles the happy path; this recovers the edges:
 *
 *  - A run that is COMPLETE but whose `TaskFinished` was lost (worker restart,
 *    dropped event) → re-checks and enqueues the reduce via the coordinator.
 *  - A run ABANDONED past `staleAfter` (a chunk that keeps failing, a lease that
 *    never came back) → enqueues a PARTIAL reduce over whatever landed, so one
 *    dead chunk degrades to a partial listing instead of losing the cinema. The
 *    next `ScrapeCinema` supersedes the run.
 */
class ChunkScrapeReaper(
  store:        ChunkScrapeStore,
  queue:        TaskQueue,
  coordinator:  ChunkScrapeCoordinator,
  interval:     FiniteDuration    = 1.minute,
  initialDelay: FiniteDuration    = 45.seconds,
  staleAfter:   FiniteDuration    = ChunkScrapePlanner.DefaultRunTimeout,
  runStore:     ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:        Clock             = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("chunk-scrape-reaper")

  def start(): Unit = {
    scheduler.scheduleWithFixedDelay(() => Try(tickIfClaimed()),
      initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    logger.info(s"ChunkScrapeReaper started — sweeping chunked-scrape runs every ${interval.toSeconds}s (first in ${initialDelay.toSeconds}s).")
  }

  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("chunk-scrape", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Enqueue a reduce for every complete-or-abandoned active run. Returns how
   *  many reduces were enqueued. Public so tests drive it directly. */
  def tick(): Int = {
    val now = clock.instant()
    val n = store.activeRuns().count { run =>
      val complete = run.expectedKeys.toSet.subsetOf(store.storedKeys(run.cinema, run.runId))
      if (complete) coordinator.maybeReduce(run.cinema, run.runId)
      else if (run.isStale(now, staleAfter)) {
        logger.warn(s"${run.cinema} run ${run.runId} abandoned (${store.storedKeys(run.cinema, run.runId).size}/${run.expectedKeys.size} chunks) — partial reduce")
        queue.enqueue(TaskType.ScrapeChunkReduce, ChunkScrapeKeys.reduceDedup(run.cinema, run.runId),
          ChunkScrapeKeys.reducePayload(run.cinema, run.runId)) == EnqueueResult.Added
      } else false
    }
    if (n > 0) logger.info(s"ChunkScrapeReaper enqueued $n reduce task(s).")
    n
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
