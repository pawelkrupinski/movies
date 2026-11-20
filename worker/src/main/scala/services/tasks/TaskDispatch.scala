package services.tasks

import scala.util.Try

import services.tasktypes.TaskType

/**
 * Runs a claimed task through the handler registered for its type — its outbound requests asked for on
 * the task type's HTTP path ([[TaskDispatch.httpPathOf]]), its wall-clock timed — and settles what it
 * answered ([[TaskSettlement]]).
 */
final class TaskDispatch(handlers: Seq[TaskHandler], settlement: TaskSettlement, stopwatch: tools.Stopwatch) {
  import TaskWorker.PollResult

  private val byType: Map[TaskType, TaskHandler] = handlers.map(h => h.taskType -> h).toMap

  /** How many task types have a handler. */
  def handlerCount: Int = byType.size

  /** Run `task`, claimed by `workerId`. `stopping` says whether the pool is shutting down: the stop's
   *  interrupt reaching a handler then hands its task back rather than leaving it leased. */
  def run(task: Task, workerId: String, stopping: => Boolean): PollResult =
    try runHandler(task, workerId, stopping)
    catch { case e: InterruptedException => if (stopping) settlement.handBack(task, workerId, 0L) else throw e }

  // `Try` lets an InterruptedException through (it is not NonFatal): the stop's interrupt reaching a handler
  // lands in `run`, and its task is handed back rather than left leased.
  private def runHandler(task: Task, workerId: String, stopping: => Boolean): PollResult = byType.get(task.taskType) match {
    case None => settlement.noHandler(task, workerId)
    case Some(h) =>
      val handled = stopwatch.timed(Try(tools.HttpPath.within(TaskDispatch.httpPathOf(task.taskType))(h.handle(task))))
      settlement.settle(task, workerId, handled.value, handled.millis, stopping)
  }
}

object TaskDispatch {

  /** Who a task's outbound requests are asked for ([[tools.HttpPath]], the `path` label of `kinowo_worker_http_total`):
   *  the scrapes the scraping pipeline's, the ratings and IMDb-id resolution the pipeline's enrichment, a venue page read
   *  `venue_page` (also what a film row's `EnrichDetails` reads, through `VenuePageReader`), the agreement's questions
   *  `agreement`. The rest send none of their own. */
  def httpPathOf(taskType: TaskType): String = taskType match {
    case TaskType.ScrapeCinema | TaskType.ScrapeChunk | TaskType.ScrapeChunkReduce => tools.HttpPath.Scrape
    case TaskType.EnrichDetails | TaskType.ResolveImdbId | TaskType.ImdbRating | TaskType.FilmwebRating |
        TaskType.RtRating | TaskType.McRating | TaskType.RefreshAllImdb | TaskType.RefreshAllFilmweb |
        TaskType.RefreshAllMetacritic | TaskType.RefreshAllRt | TaskType.RefreshAllOmdb => tools.HttpPath.Enrich
    case TaskType.ReadVenuePage => tools.HttpPath.VenuePage
    case TaskType.RefreshSource => tools.HttpPath.Refresh
    case TaskType.AskQuestion | TaskType.AgreementQuestion | TaskType.AgreementFind | TaskType.AgreementPoster | TaskType.AgreementCatalogue =>
      tools.HttpPath.Agreement
    case TaskType.SettleNow | TaskType.RenderShareCard | TaskType.PruneShareCards | TaskType.ReleaseShareCardHold |
        TaskType.RenderCityCard | TaskType.AuditReadModelContent | TaskType.AuditShareCards => tools.HttpPath.Other
  }
}
