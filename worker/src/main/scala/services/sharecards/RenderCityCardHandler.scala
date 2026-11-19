package services.sharecards

import services.tasks.{HandlerOutcome, Task, TaskHandler}
import services.tasktypes.TaskType

/** `RenderCityCard`: render the city's card when it is due. A failure is retried, then given up —
 *  the city keeps its old card, or the country's home card while it has none. */
class RenderCityCardHandler(service: CityCardService, maxAttempts: Int = 3) extends TaskHandler {
  val taskType: TaskType = TaskType.RenderCityCard
  def handle(task: Task): HandlerOutcome =
    task.payload.get(CityCardService.CityKey).flatMap(service.renderIfDue) match {
      case Some(ShareCardMetrics.CityOutcome.Failed) if task.attempts < maxAttempts => HandlerOutcome.Reschedule(Some("the city card could not be drawn"))
      case Some(_) => HandlerOutcome.Done
      case None    => HandlerOutcome.Skipped
    }
}
