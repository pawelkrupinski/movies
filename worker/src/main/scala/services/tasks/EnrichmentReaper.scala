package services.tasks

import java.time.{Clock, Instant}

import scala.concurrent.duration.*

import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.MovieCacheReader
import settings.EnrichmentMaxEnqueuePerTick

/**
 * Periodically enqueues rating-refresh tasks for stale rows — the queue-based
 * replacement for the `*Ratings` classes' 4h cache walks.
 *
 * Each rating source refreshes every eligible row once per the [[DueWindow]]
 * period (4h, matching the rating TTL in [[services.freshness.Freshness]]). But
 * instead of walking the whole corpus in one burst — which used to drop ~750 tasks
 * at once, four times every 4h, and pin the shared-CPU credit (the midday `steal`
 * spikes) — the walk is SMEARED across the period: each `(row, source)` gets a
 * deterministic phase offset in `[0, period)` from a hash of its dedup key,
 * and a frequent tick (`tickInterval`, default 1min) enqueues only the rows
 * whose personal period boundary has passed since their last refresh. Over a
 * period the same ~750 tasks per source go out, but ~`N · tickInterval /
 * period` per tick (≈3 for 750 rows at 1min/4h) — a flat per-minute trickle, not
 * the ~5min-wide burst a coarser cadence dumps in one tick. The four sources
 * self-decorrelate because the dedup key embeds the
 * source label, so a film's phase differs per source — no manual stagger needed.
 *
 * The spread is self-sustaining and self-correcting: a row refreshed at its
 * boundary is next due exactly one period later (phases stay fixed, so the
 * spread persists), and a missed tick (GC pause, restart) is caught by the next
 * tick because due-ness is computed from the persisted last-refresh stamp, not
 * an in-memory timer. A never-refreshed row is due immediately, so rows hydrated
 * from Mongo but never enriched are queued on the first tick after boot without
 * a separate boot sweep — bounded by `maxEnqueuePerTick` so a cold corpus
 * doesn't enqueue everything at once (the leftover stays due and drains over the
 * next few ticks).
 *
 * A sweep only enqueues rows the source can act on (IMDb needs an imdbId; the
 * others need a resolved tmdbId), matching the old walks' scope. Enqueue is
 * deduped by the queue's unique index and re-gated by [[RatingHandler]] at
 * pickup using the SAME [[DueWindow]], so the handler skips a task iff this
 * reaper would no longer enqueue it (a race where another machine refreshed it
 * first) — never a task that is still due. On a multi-machine worker each tick is gated by a cluster-wide
 * occurrence claim keyed by the tick window, so one machine walks the corpus per tick, rotating — not every machine
 * every tick: the schedule and the claim are the [[ClaimedEnqueueReaper]] the composition root runs this on
 * ([[EnrichmentReaper.Occurrence]], recorded as [[EnrichmentReaper.Tick]], held until [[ready]]).
 */
class EnrichmentReaper(
  cache:     MovieCacheReader,
  freshness: FreshnessStore,
  // The per-row enqueue decision (eligible sources, tmdbId-keyed dedup, due gate), shared with the newcomer-fold path so
  // the two can't drift: the wiring hands `MovieService` the SAME instance for the newcomer kick. Its due schedule must
  // be the instance `RatingHandler` re-gates on — see [[DueWindow]].
  enqueuer:  RatingEnqueuer,
  // Cap on enqueues per tick. The phase spread keeps steady-state ticks small,
  // but a cold or long-down corpus has every row due at once; capping bounds that
  // recovery burst the same way `ScrapeReaper` does — the leftover stays due and
  // drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap comfortably above the steady-state.
  // BY-NAME: read live each tick, so an `/admin/config` flip applies mid-flight.
  maxEnqueuePerTick: => EnrichmentMaxEnqueuePerTick = EnrichmentMaxEnqueuePerTick(Int.MaxValue),
  clock:    Clock
) extends Logging {
  /** Whether a tick may run: the rating stamps have hydrated. They load in the rest phase, so until they land an empty
   *  mirror reads as "every row's rating stale" and re-enqueues up to maxEnqueuePerTick rating tasks (×4 sources) on
   *  every deploy. All rating kinds share the one rest-hydrate signal, so any one of them answers "are the rating stamps
   *  loaded"; an in-memory store is ready now. Same boot-hydrate gate as DetailReaper's. */
  def ready: Boolean = freshness.isReady(FreshnessKind.ImdbRating)

  /** Enqueue every eligible, now-due `(row, source)`, up to `maxEnqueuePerTick`: how many were added, and how many
   *  enqueues failed (logged by the queue; a run with any is not a completed one). An injectable `nowMillis`, so tests
   *  can drive time. */
  def tick(nowMillis: Long = clock.millis()): Enqueued = {
    val cap      = maxEnqueuePerTick.value
    val now      = Instant.ofEpochMilli(nowMillis)
    var enqueued = Enqueued.Zero
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued.added < cap) {
      val (key, record) = rows.next()
      enqueued += enqueuer.enqueueDueFor(key, record, now, cap - enqueued.added)
    }
    if (enqueued.added > 0) logger.info(s"EnrichmentReaper enqueued ${enqueued.added} due rating-refresh task(s).")
    if (enqueued.failed > 0) logger.warn(s"EnrichmentReaper: ${enqueued.failed} rating-refresh enqueue(s) failed this tick.")
    enqueued
  }
}

object EnrichmentReaper {

  /** The tick window's occurrence claim, one machine a window. */
  val Occurrence = "enrich-sweep"

  /** What each completed run is recorded as (`ScheduledTickStale`) and its scheduler named. */
  val Tick = "enrichment-reaper"

  /** How often the reaper wakes to enqueue the now-due slice of the corpus. At
   *  1min over a 4h period the corpus spreads across ~240 ticks, so each tick
   *  enqueues only ~1/240 of each source — a flat per-minute trickle rather than
   *  the ~5min-wide bursts a coarser cadence dumps in one tick (the residual
   *  `kinowo_worker_tasks` rating spikes the panel still showed). The walk is a
   *  cheap in-memory corpus scan, so the finer cadence costs little. */
  val DefaultTickInterval: FiniteDuration = 1.minute
}
