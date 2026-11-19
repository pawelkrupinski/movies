package services.tasks

import services.tasktypes.TaskType

/** One country's task-pipeline metrics -- enqueue, claim and outcome, the first-rating delay and the chunk-page memo --
 *  each record bound to `countryCode` on the shared [[TaskPipelineSeries]]. Wiring: [[MeteredTaskQueue]] feeds
 *  `recordEnqueue`; the [[TaskWorker]] reports claims and outcomes to it as its [[TaskObserver]]. */
final class TaskPipelineBinder(countryCode: String, series: TaskPipelineSeries)
  extends TaskObserver with RatingLatencyMetrics with ChunkPageMemoMetrics {
  def recordEnqueue(taskType: TaskType, result: String): Unit           = series.recordEnqueue(countryCode, taskType, result)
  def onStarted(task: Task): Unit                                       = series.onStarted(countryCode, task)
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = series.onFinished(countryCode, task, outcome, handleMillis)
  def recordFirstRatingDelay(site: String, seconds: Double): Unit       = series.recordFirstRatingDelay(countryCode, site, seconds)
  def recordPage(outcome: String): Unit                                 = series.recordChunkPage(countryCode, outcome)
}
