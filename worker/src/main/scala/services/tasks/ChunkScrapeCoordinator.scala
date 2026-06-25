package services.tasks

import play.api.Logging
import services.events.{DomainEvent, TaskFinished}

/**
 * The fan-IN of a chunked scrape: subscribes to `TaskFinished(ScrapeChunk)` (the
 * same event-driven pattern as `StagingReaper.onTaskFinished`) and, once every
 * expected chunk of the run has landed, enqueues the single `ScrapeChunkReduce`.
 * Idempotent via the reduce dedup key, so the periodic `ChunkScrapeReaper`
 * backstop can call it too without double-enqueuing.
 */
class ChunkScrapeCoordinator(store: ChunkScrapeStore, queue: TaskQueue) extends Logging {

  def onTaskFinished: PartialFunction[DomainEvent, Unit] = {
    case TaskFinished(TaskType.ScrapeChunk, _, payload) =>
      maybeReduce(payload.getOrElse(ChunkScrapeKeys.CinemaKey, ""),
                  payload.getOrElse(ChunkScrapeKeys.RunIdKey, ""))
      ()
  }

  /** Enqueue the reduce iff `runId` is still the cinema's active run AND every
   *  expected key has stored. Returns true when it actually enqueued (a fresh
   *  task). Package-private so the backstop reaper and tests can drive it. */
  private[tasks] def maybeReduce(cinema: String, runId: String): Boolean =
    store.activeRun(cinema).filter(_.runId == runId).exists { run =>
      val complete = run.expectedKeys.toSet.subsetOf(store.storedKeys(cinema, runId))
      complete && queue.enqueue(TaskType.ScrapeChunkReduce,
        ChunkScrapeKeys.reduceDedup(cinema, runId),
        ChunkScrapeKeys.reducePayload(cinema, runId)) == EnqueueResult.Added
    }
}
