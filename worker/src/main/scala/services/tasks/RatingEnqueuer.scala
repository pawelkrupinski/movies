package services.tasks

import models.MovieRecord
import services.freshness.FreshnessStore
import services.movies.CacheKey

import java.time.Instant

/**
 * Enqueues the due rating-refresh tasks for a SINGLE resolved row. Shared by the
 * two callers that need that decision so they can't drift:
 *
 *   - [[EnrichmentReaper]]'s per-tick corpus walk (capped + phase-spread); and
 *   - the newcomer-fold path (`MovieService.announceResolvedNewMovie`), which kicks
 *     a freshly-promoted film's ratings the instant it resolves rather than waiting
 *     for the reaper's next tick. That's a trickle (a handful of promotions a day),
 *     not the corpus-wide cohort the old `TmdbResolved` fan-out was — so it doesn't
 *     reintroduce the enqueue burst the reaper's cap exists to smooth.
 *
 * Centralises the per-row decision: which sources the row is eligible for (IMDb
 * needs an imdbId; the others a resolved tmdbId), the tmdbId-keyed dedup key with
 * the legacy title-key fallback, the [[DueWindow]] gate, and the queue's own dedup.
 */
class RatingEnqueuer(
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // The shared due schedule. The SAME instance must back the [[RatingHandler]] so
  // this enqueue gate and that execution gate agree on what counts as due — see
  // [[DueWindow]].
  dueWindow: DueWindow
) {
  // The eligibility rule lives in RatingSources so the metrics census can't drift
  // from what this actually enqueues (see RatingSources).
  private val sources = RatingSources.all

  /** How many rating sources exist (for the reaper's start-up log). */
  val sourceCount: Int = sources.size

  /** Enqueue up to `limit` of `record`'s eligible, now-due rating sources, returning
   *  how many were NEWLY added (a not-due or already-queued source doesn't count).
   *  `limit` lets the reaper honour its per-tick cap across rows; the newcomer path
   *  leaves it unbounded — one row enqueues at most the four sources. */
  def enqueueDueFor(key: CacheKey, record: MovieRecord, now: Instant, limit: Int = Int.MaxValue): Int = {
    var enqueued = 0
    val it = sources.iterator
    while (it.hasNext && enqueued < limit) {
      val s = it.next()
      if (s.eligible(record)) {
        val dedupKey = RatingTasks.dedupKey(s.kind, key, record.tmdbId)
        // Honour a stamp left under the legacy title-based key so switching to
        // tmdbId-keyed freshness doesn't re-queue a row that's fresh under its old
        // key. (Drop the fallback once no legacy stamps remain — see EnrichmentReaper.)
        val lastFetched = freshness.lastFetchedAt(dedupKey)
          .orElse(freshness.lastFetchedAt(RatingTasks.dedupKey(s.kind, key)))
        if (dueWindow.isDue(dedupKey, lastFetched, now) &&
            queue.enqueue(s.taskType, dedupKey, RatingTasks.payload(key)) == EnqueueResult.Added)
          enqueued += 1
      }
    }
    enqueued
  }
}
