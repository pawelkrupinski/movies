package services.tasks

import models.{Country, MovieRecord}
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
  dueWindow: DueWindow,
  // The country this enqueuer serves — selects which rating sources apply. Defaults
  // to Poland so tests and single-country paths keep the historical (Filmweb-on)
  // behaviour; the worker wiring passes the actual per-country value so a non-Filmweb
  // country (UK) never enqueues a handler-less FilmwebRating task (see RatingSources).
  country: Country = Country.default
) {
  // The eligibility rule lives in RatingSources so the metrics census can't drift
  // from what this actually enqueues (see RatingSources). `forCountry` also drops
  // sources this country doesn't wire a handler for (Filmweb outside Poland).
  private val sources = RatingSources.forCountry(country)

  /** How many rating sources exist (for the reaper's start-up log). */
  val sourceCount: Int = sources.size

  /** Enqueue up to `limit` of `record`'s eligible, now-due rating sources, returning
   *  how many were NEWLY added (a not-due or already-queued source doesn't count).
   *  `limit` lets the reaper honour its per-tick cap across rows; the newcomer path
   *  leaves it unbounded — one row enqueues at most the four sources.
   *
   *  `force` re-fetches EVERY eligible source regardless of the adaptive cadence:
   *  it drops the source's freshness stamps first, so a stamp that would otherwise
   *  read "recently checked" no longer gates the enqueue. A (re)resolve uses this —
   *  `resetToScrapedData` strips the row's scores, but the stamps survive, so
   *  without forcing the reaper judges each source fresh and never re-fetches,
   *  leaving the film rating-less. */
  def enqueueDueFor(key: CacheKey, record: MovieRecord, now: Instant, limit: Int = Int.MaxValue, force: Boolean = false): Int = {
    var enqueued = 0
    val it = sources.iterator
    while (it.hasNext && enqueued < limit) {
      val s = it.next()
      if (s.eligible(record)) {
        val dedupKey  = RatingTasks.dedupKey(s.kind, key, record.tmdbId)
        val legacyKey = RatingTasks.dedupKey(s.kind, key)
        // A forced refresh clears both stamps so the source reads as never-fetched
        // and re-fetches now (the re-resolve strip healer).
        if (force) { freshness.invalidate(dedupKey); freshness.invalidate(legacyKey) }
        // Honour a stamp left under the legacy title-based key so switching to
        // tmdbId-keyed freshness doesn't re-queue a row that's fresh under its old
        // key. (Drop the fallback once no legacy stamps remain — see EnrichmentReaper.)
        val lastFetched = freshness.lastFetchedAt(dedupKey).orElse(freshness.lastFetchedAt(legacyKey))
        if (dueWindow.isDue(dedupKey, lastFetched, now) &&
            queue.enqueue(s.taskType, dedupKey, RatingTasks.payload(key)) == EnqueueResult.Added)
          enqueued += 1
      }
    }
    enqueued
  }
}
