package services.staging

import services.tasks.{HandlerOutcome, StagingTaskKeys, Task, TaskHandler, TaskType}

/**
 * The four staging-incubation task handlers — thin wrappers that parse a task's
 * payload (via [[StagingTaskKeys]]), run the matching [[StagingSteps]] step, and
 * map its result to a [[HandlerOutcome]]. All the business logic lives in
 * `StagingSteps`; what comes next is decided by `StagingReaper` off the
 * `TaskFinished` event, so these handlers never enqueue follow-ups themselves.
 */

/** STEP 1: fetch one cinema's per-film detail. Reschedules (→ backoff retry)
 *  while a deferred fetch hasn't landed; the reaper enqueues the resolve step
 *  once every cinema's detail is ready. Once `MaxDetailAttempts` claims have gone
 *  by without it landing the fetch is treated as permanently failing (a dead
 *  event page, or a Filmweb-fallback row whose filmUrl the cinema can't parse) —
 *  we give up and let the film graduate on listing-only data rather than
 *  rescheduling forever. */
class StagingDetailHandler(steps: StagingSteps) extends TaskHandler {
  // Anchor task payloads the way the staging pipeline anchors its rows.
  private val normalizer: services.movies.TitleNormalizer = steps.normalizer

  val taskType: TaskType = TaskType.StagingDetail
  def handle(task: Task): HandlerOutcome =
    StagingTaskKeys.cinemaOf(task.payload) match {
      case None         => HandlerOutcome.Skipped   // unknown/renamed cinema — drop the orphaned task
      case Some(cinema) =>
        val giveUp = task.attempts >= StagingDetailHandler.MaxDetailAttempts
        if (steps.fetchDetailFor(cinema, StagingTaskKeys.anchorOf(task.payload, steps.normalizer), giveUp)) HandlerOutcome.Done
        else HandlerOutcome.Reschedule(Some(s"staging detail not ready for ${cinema.displayName}"))
    }
}

object StagingDetailHandler {
  /** After this many claims (with `TaskWorker`'s exponential backoff that's
   *  ≈ 5+10+20+40+80s ≈ 2.5min of retries) a deferred detail fetch that keeps
   *  failing is given up on, so the staging chain advances instead of hot-looping
   *  on a fetch that can never succeed. `task.attempts` is incremented on each
   *  claim, so the first run is attempt 1. */
  private[staging] val MaxDetailAttempts = 6
}

/** STEP 2: resolve the film against TMDB once and stamp the outcome. A failed lookup
 *  (`TransientFailure`) reschedules with the queue's exponential backoff, and past the
 *  queue's own attempts the reaper's backstop re-enqueues it: it is NEVER concluded as a
 *  no-match. A definitive TMDB answer — a miss, or its "not found" — already comes back
 *  concluded from `MovieService.resolveStagingRecord`; what fails here is an outage or a
 *  rate limit, which says nothing about the film. (A six-claim give-up budget used to
 *  conclude it, so a ~2.5-minute TMDB blip hid new films as unresolved until the daily
 *  re-try.) A film that keeps failing stays in staging, where StagingStuckAlerter names it.
 *
 *  Detail still outstanding is NOT a failure, so it completes (`Skipped`) rather
 *  than rescheduling: this task simply isn't due yet, and `StagingReaper` — the
 *  single owner of the chain — re-enqueues it the moment every cinema's detail
 *  has landed. Rescheduling instead parked it under the same exponential backoff
 *  a failure gets, and `TaskQueue.enqueue` is insert-only, so a chain that later
 *  wanted to run the step could not pull the waiting task's `nextEligibleAt`
 *  forward. A UK film showing at ten Cineworld venues (whose per-venue detail
 *  fetches pace out over the best part of an hour) therefore burned ten claims
 *  climbing to the 30-minute backoff cap, then idled at the cap AFTER its last
 *  detail landed — the "staging detail not ready" rows of 2026-07-27. */
class StagingResolveTmdbHandler(steps: StagingSteps) extends TaskHandler {
  // Anchor task payloads the way the staging pipeline anchors its rows.
  private val normalizer: services.movies.TitleNormalizer = steps.normalizer

  val taskType: TaskType = TaskType.StagingResolveTmdb
  def handle(task: Task): HandlerOutcome = {
    steps.resolveAndStamp(StagingTaskKeys.anchorOf(task.payload, steps.normalizer)) match {
      case StagingSteps.Resolved | StagingSteps.AlreadyDone => HandlerOutcome.Done
      case StagingSteps.DetailNotReady                      => HandlerOutcome.Skipped
      case StagingSteps.TransientFailure                    => HandlerOutcome.Reschedule(Some("staging tmdb resolve transient miss"))
    }
  }
}


/** STEP 3: recover a missing IMDb id (best-effort — gives up gracefully). */
class StagingResolveImdbIdHandler(steps: StagingSteps) extends TaskHandler {
  // Anchor task payloads the way the staging pipeline anchors its rows.
  private val normalizer: services.movies.TitleNormalizer = steps.normalizer

  val taskType: TaskType = TaskType.StagingResolveImdbId
  def handle(task: Task): HandlerOutcome = {
    steps.recoverImdbFor(StagingTaskKeys.anchorOf(task.payload, steps.normalizer))
    HandlerOutcome.Done
  }
}

/** STEP 4: fold the concluded film's whole sanitize group into `movies`. `fold`
 *  is wired to publish `StagingFilmEnriched`, which drives the existing
 *  transactional, group-scoped `StagingFolder.foldGroup` (settles as it folds);
 *  a thrown fold reschedules, and the reaper's periodic scan re-enqueues any film
 *  still unfolded. */
class StagingFoldHandler(fold: String => Unit) extends TaskHandler {
  val taskType: TaskType = TaskType.StagingFold
  def handle(task: Task): HandlerOutcome = {
    fold(StagingTaskKeys.titleOf(task.payload))
    HandlerOutcome.Done
  }
}
