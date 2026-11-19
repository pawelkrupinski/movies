package services.tasks

import services.tasktypes.TaskType

import play.api.Logging
import services.events.{DomainEvent, TaskFinished}

/**
 * The fan-IN of a chunked scrape: subscribes to `TaskFinished(ScrapeChunk)` (an
 * event-driven task-finished hook) and, once every
 * expected chunk of the run has landed, enqueues the single `ScrapeChunkReduce`.
 * Idempotent via the reduce dedup key, so the `ChunkScrapeReaper` (a run's
 * deadline, or its boot/hourly sweep) can call it too without double-enqueuing.
 */
class ChunkScrapeCoordinator(store: ChunkScrapeStore, queue: TaskProducer, clock: java.time.Clock) extends Logging {

  def onTaskFinished: PartialFunction[DomainEvent, Unit] = {
    case TaskFinished(TaskType.ScrapeChunk, _, payload) =>
      maybeReduce(payload.getOrElse(ChunkScrapeKeys.CinemaKey, ""),
                  payload.getOrElse(ChunkScrapeKeys.RunIdKey, ""))
      ()
  }

  /** Enqueue the reduce iff `runId` is still the cinema's active run AND every
   *  expected key has stored. Returns what the enqueue came to: [[Enqueued.Zero]] when
   *  the run is not ready, or its reduce is already queued. Package-private so the
   *  backstop reaper and tests can drive it. */
  private[tasks] def maybeReduce(cinema: String, runId: String): Enqueued =
    store.activeRun(cinema).filter(_.runId == runId)
      .filter(run => run.expectedKeys.toSet.subsetOf(store.storedKeys(cinema, runId)))
      .fold(Enqueued.Zero)(_ => queue.enqueued(TaskType.ScrapeChunkReduce,
        ChunkScrapeKeys.reduceDedup(cinema, runId),
        ChunkScrapeKeys.reducePayload(cinema, runId), submittedAt = clock.instant()))

  /** Enqueue `run`'s PARTIAL reduce over whatever chunks have landed: an abandoned run (a chunk that keeps
   *  failing, a lease that never came back) degrades to a partial listing instead of losing the cinema. */
  private[tasks] def reducePartial(run: ChunkRun, now: java.time.Instant): Enqueued =
    queue.enqueued(TaskType.ScrapeChunkReduce, ChunkScrapeKeys.reduceDedup(run.cinema, run.runId),
      ChunkScrapeKeys.reducePayload(run.cinema, run.runId), submittedAt = now)
}
