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
 *  once every cinema's detail is ready. */
class StagingDetailHandler(steps: StagingSteps) extends TaskHandler {
  val taskType: TaskType = TaskType.StagingDetail
  def handle(task: Task): HandlerOutcome =
    StagingTaskKeys.cinemaOf(task.payload) match {
      case None         => HandlerOutcome.Skipped   // unknown/renamed cinema — drop the orphaned task
      case Some(cinema) =>
        if (steps.fetchDetailFor(cinema, StagingTaskKeys.anchorOf(task.payload))) HandlerOutcome.Done
        else HandlerOutcome.Reschedule(Some(s"staging detail not ready for ${cinema.displayName}"))
    }
}

/** STEP 2: resolve the film against TMDB once and stamp the outcome. A transient
 *  TMDB miss (`None`) reschedules with the queue's exponential backoff — the
 *  durability the 120s promoter tick lacked. */
class StagingResolveTmdbHandler(steps: StagingSteps) extends TaskHandler {
  val taskType: TaskType = TaskType.StagingResolveTmdb
  def handle(task: Task): HandlerOutcome = steps.resolveAndStamp(StagingTaskKeys.anchorOf(task.payload)) match {
    case StagingSteps.Resolved | StagingSteps.AlreadyDone => HandlerOutcome.Done
    case StagingSteps.DetailNotReady                      => HandlerOutcome.Reschedule(Some("staging detail not ready"))
    case StagingSteps.TransientFailure                    => HandlerOutcome.Reschedule(Some("staging tmdb resolve transient miss"))
  }
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
