package services.tasks

import java.time.Clock

import models.CacheKey
import services.freshness.FreshnessStore
import services.tasktypes.TaskType

/**
 * Puts one retriggered task on the queue. A rating task's dedup and freshness are keyed on the stable tmdbId, so the
 * periodic reaper would otherwise skip a re-enqueue as still-fresh (the stamp predates the merge): the stamp is
 * INVALIDATED first, so the handler's freshness re-gate sees the work as due and actually re-fetches with the merge's
 * corrected inputs. The IMDb-id resolve isn't freshness-gated (the queue's per-dedupKey idempotency is its guard), so it
 * is a plain enqueue.
 */
final class RetriggerEnqueuer(queue: TaskProducer, freshness: FreshnessStore,
                              // Stamps each retriggered task's `submittedAt`.
                              clock: Clock) {

  /** Enqueue `task` for the row at `key` (its TMDB id `tmdbId`); the IMDb-id resolve searches `searchTitle`. */
  def enqueue(task: RetriggerTask, key: CacheKey, tmdbId: Option[Int], searchTitle: => String): Unit = task match {
    case RetriggerTask.Rating(taskType, kind) =>
      val dedup = RatingTasks.dedupKey(kind, key, tmdbId)
      freshness.invalidate(dedup)
      queue.enqueue(taskType, dedup, RatingTasks.payload(key), submittedAt = clock.instant())
      ()
    case RetriggerTask.ResolveImdbId =>
      queue.enqueue(
        TaskType.ResolveImdbId,
        EnrichTaskKeys.resolveImdbIdDedup(key.cleanTitle, key.year),
        EnrichTaskKeys.resolveImdbIdPayload(key.cleanTitle, key.year, searchTitle),
        submittedAt = clock.instant())
      ()
  }
}
