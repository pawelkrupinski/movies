package services.sharecards

import services.events.{DomainEvent, TaskFinished}
import services.tasks.{HandlerOutcome, Task, TaskHandler, TaskType}

/** `RenderShareCard`: draw the card its payload describes. A card whose every poster failed is
 *  retried (a cinema origin's outage passes), then given up — the web keeps the fallback. */
class RenderShareCardHandler(service: ShareCardService, maxAttempts: Int = 3) extends TaskHandler {
  val taskType: TaskType = TaskType.RenderShareCard
  def handle(task: Task): HandlerOutcome =
    ShareCardInputs.fromPayload(task.payload).fold[HandlerOutcome](HandlerOutcome.Skipped) { inputs =>
      service.renderIfLatest(inputs, ShareCardService.reasons(task.payload), first = task.payload.get(ShareCardService.FirstKey).contains("true"),
        retryPoster = task.payload.get(ShareCardService.RetryPosterKey).contains("true")) match {
        case ShareCardMetrics.Outcome.Failed if task.attempts < maxAttempts => HandlerOutcome.Reschedule(Some("the card could not be drawn"))
        case ShareCardMetrics.Outcome.Superseded => HandlerOutcome.Skipped
        case _ => HandlerOutcome.Done
      }
    }
}

/** `ShareCardBackfill`: one backfill tick. */
class ShareCardBackfillHandler(backfill: ShareCardBackfill) extends TaskHandler {
  val taskType: TaskType = TaskType.ShareCardBackfill
  def handle(task: Task): HandlerOutcome = { backfill.tick(); HandlerOutcome.Done }
}

/** `PruneShareCards`: the daily prune, or (`mode=budget`) the budget alone. */
class PruneShareCardsHandler(janitor: ShareCardJanitor) extends TaskHandler {
  val taskType: TaskType = TaskType.PruneShareCards
  def handle(task: Task): HandlerOutcome = {
    if (task.payload.get(PruneShareCardsHandler.ModeKey).contains(PruneShareCardsHandler.Budget)) janitor.enforceBudget()
    else janitor.prune()
    HandlerOutcome.Done
  }
}

object PruneShareCardsHandler {
  val ModeKey = "mode"
  val Daily   = "daily"
  val Budget  = "budget"
}

/** `ReleaseShareCardHold`: a first-publish hold has run out — publish whatever it still holds. */
class ReleaseShareCardHoldHandler(releaseExpiredHolds: () => Unit) extends TaskHandler {
  val taskType: TaskType = TaskType.ReleaseShareCardHold
  def handle(task: Task): HandlerOutcome = { releaseExpiredHolds(); HandlerOutcome.Done }
}

/** `RescrapeShareCard`: ask Facebook to look at a film's pages again. */
class RescrapeShareCardHandler(rescraper: ShareCardRescraper, maxAttempts: Int = 3) extends TaskHandler {
  val taskType: TaskType = TaskType.RescrapeShareCard
  def handle(task: Task): HandlerOutcome =
    task.payload.get("filmId").fold[HandlerOutcome](HandlerOutcome.Skipped) { filmId =>
      if (rescraper.rescrape(filmId) || task.attempts >= maxAttempts) HandlerOutcome.Done
      else HandlerOutcome.Reschedule(Some("Facebook re-scrape failed"))
    }
}

/**
 * What a finished render sets off, on the task framework's completion event: re-project the film,
 * so its `web_movies` document points at the new card — and, for a card the first-publish gate is
 * holding, publishes the film. A first card that could not be made at all ends its hold at once —
 * unless a newer request superseded it (`superseded`): that request's render ends the hold.
 */
class ShareCardFollowUp(store: ShareCardStore, superseded: ShareCardInputs => Boolean,
                        refresh: String => Unit, releaseHold: String => Unit) {
  def onTaskFinished: PartialFunction[DomainEvent, Unit] = {
    case TaskFinished(TaskType.RenderShareCard, _, payload) =>
      ShareCardInputs.fromPayload(payload).foreach { inputs =>
        if (store.version(store.cardPath(inputs.filmId)).exists(inputs.acceptableVersions.contains)) refresh(inputs.filmId)
        else if (payload.get(ShareCardService.FirstKey).contains("true") && !superseded(inputs)) releaseHold(inputs.filmId)
      }
  }
}
