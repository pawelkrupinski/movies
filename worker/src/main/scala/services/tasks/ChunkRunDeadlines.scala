package services.tasks

import java.time.{Clock, Instant}
import java.util.concurrent.{ConcurrentHashMap, RejectedExecutionException, ScheduledExecutorService, TimeUnit}

import play.api.Logging
import tools.ScheduledTick

/**
 * One deadline per chunked run, at `createdAt + staleAfter`, held in this process as a one-shot timer:
 * at it, a run still active is either COMPLETE (its last `TaskFinished` was lost) or ABANDONED, and is
 * reduced through the [[ChunkScrapeCoordinator]] — in full or in part. A [[sweep]] of every run doc
 * recovers the runs whose timer died with a process ([[ChunkScrapeReaper]] runs it).
 */
final class ChunkRunDeadlines(
  store:       ChunkScrapeStore,
  coordinator: ChunkScrapeCoordinator,
  staleAfter:  ChunkScrapePlanner.RunTimeout = ChunkScrapePlanner.RunTimeout(ChunkScrapePlanner.DefaultRunTimeout),
  clock:       Clock,
  scheduler:   ScheduledExecutorService
) extends Logging {

  /** runIds with a deadline pending in this process, so a sweep does not arm a second. */
  private val armed = ConcurrentHashMap.newKeySet[String]()

  /** Arm `run`'s deadline in this process, unless it already has one here. */
  def arm(run: ChunkRun): Unit =
    if (armed.add(run.runId)) {
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
  private def atDeadline(cinema: String, runId: String): Unit =
    store.activeRun(cinema).filter(_.runId == runId).foreach(reduceIfDue(_, clock.instant()))

  /** Reduce every active run that is complete or past its deadline, and arm the deadline of
   *  every other. Returns how many reduces were enqueued, and how many enqueues failed (a
   *  sweep with any is not a completed run). */
  def sweep(): Enqueued = {
    val now = clock.instant()
    val n = store.activeRuns().foldLeft(Enqueued.Zero)(_ + reduceIfDue(_, now))
    if (n.added > 0) logger.info(s"ChunkScrapeReaper enqueued ${n.added} reduce task(s).")
    if (n.failed > 0) logger.warn(s"ChunkScrapeReaper: ${n.failed} reduce enqueue(s) failed; the next sweep tries again.")
    n
  }

  /** The full reduce when every chunk landed (its last completion was lost), the partial
   *  one once past its deadline — and otherwise the deadline, armed. */
  private def reduceIfDue(run: ChunkRun, now: Instant): Enqueued = {
    val stored = store.storedKeys(run.cinema, run.runId)
    if (run.expectedKeys.toSet.subsetOf(stored)) coordinator.maybeReduce(run.cinema, run.runId)
    else if (run.isStale(now, staleAfter.value)) {
      logger.warn(s"${run.cinema} run ${run.runId} abandoned (${stored.size}/${run.expectedKeys.size} chunks) — partial reduce")
      coordinator.reducePartial(run, now)
    } else { arm(run); Enqueued.Zero }
  }

  private def deadlineOf(run: ChunkRun): Instant = run.createdAt.plusMillis(staleAfter.value.toMillis)
}
