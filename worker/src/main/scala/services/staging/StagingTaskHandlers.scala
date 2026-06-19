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
  val taskType: TaskType = TaskType.StagingDetail
  def handle(task: Task): HandlerOutcome =
    StagingTaskKeys.cinemaOf(task.payload) match {
      case None         => HandlerOutcome.Skipped   // unknown/renamed cinema — drop the orphaned task
      case Some(cinema) =>
        val giveUp = task.attempts >= StagingDetailHandler.MaxDetailAttempts
        if (steps.fetchDetailFor(cinema, StagingTaskKeys.anchorOf(task.payload), giveUp)) HandlerOutcome.Done
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

/** STEP 2: resolve the film against TMDB once and stamp the outcome. A transient
 *  TMDB miss (`None`) reschedules with the queue's exponential backoff — the
 *  durability the 120s promoter tick lacked. Once `MaxResolveAttempts` claims
 *  have gone by still failing, the lookup is treated as permanently failing (a
 *  decorated/foreign title TMDB can't search, or a lookup that keeps throwing)
 *  and the film concludes as a no-match so it folds (un-enriched) instead of
 *  re-resolving forever — the staging accumulation this guards against. */
class StagingResolveTmdbHandler(steps: StagingSteps) extends TaskHandler {
  val taskType: TaskType = TaskType.StagingResolveTmdb
  def handle(task: Task): HandlerOutcome = {
    val giveUp = task.attempts >= StagingResolveTmdbHandler.MaxResolveAttempts
    steps.resolveAndStamp(StagingTaskKeys.anchorOf(task.payload), giveUp) match {
      case StagingSteps.Resolved | StagingSteps.AlreadyDone => HandlerOutcome.Done
      case StagingSteps.DetailNotReady                      => HandlerOutcome.Reschedule(Some("staging detail not ready"))
      case StagingSteps.TransientFailure                    => HandlerOutcome.Reschedule(Some("staging tmdb resolve transient miss"))
    }
  }
}

object StagingResolveTmdbHandler {
  /** After this many claims (the queue's exponential backoff, ≈ the detail
   *  handler's ≈2.5min) a TMDB resolve that keeps failing is given up on, so the
   *  film concludes as a no-match and folds instead of hot-looping. `task.attempts`
   *  is incremented per claim, so the first run is attempt 1. */
  private[staging] val MaxResolveAttempts = 6
}

/** STEP 3: recover a missing IMDb id (best-effort — gives up gracefully). */
class StagingResolveImdbIdHandler(steps: StagingSteps) extends TaskHandler {
  val taskType: TaskType = TaskType.StagingResolveImdbId
  def handle(task: Task): HandlerOutcome = {
    steps.recoverImdbFor(StagingTaskKeys.anchorOf(task.payload))
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
