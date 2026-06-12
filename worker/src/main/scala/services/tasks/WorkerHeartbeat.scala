package services.tasks

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.util.Try

/**
 * Periodic one-line log of the task-queue depth — the worker's activity signal.
 *
 * When the worker's CPU credit balance collapses and steal spikes (see the
 * Grafana `kinowo-worker-cpu-starved` alert), the question is always *what was
 * it doing?* — and a CPU-credit drain is almost always a scrape/enrich BACKLOG
 * draining. The queue depth is the direct measure of that backlog, but until now
 * it was only visible live (the web queue-monitor page reading `queue.monitor()`)
 * and never landed in the logs, so a past episode couldn't be explained after the
 * fact. This drops `waiting`/`worked_on` counts into the worker log every minute
 * so a steal window can be correlated with the backlog that caused it (and, once
 * logs are shipped durably, answered post-hoc).
 */
class WorkerHeartbeat(queue: TaskQueue, intervalSeconds: Long = 60) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("worker-heartbeat")

  /** The one-line queue-depth summary. Package-private so the test can assert it
   *  without driving the scheduler. */
  private[tasks] def statusLine(): String = {
    val counts = queue.countByState()
    val waiting  = counts.getOrElse(TaskState.Waiting, 0L)
    val workedOn = counts.getOrElse(TaskState.WorkedOn, 0L)
    s"worker heartbeat: queue waiting=$waiting worked_on=$workedOn (backlog=${waiting + workedOn})"
  }

  def start(): Unit = {
    scheduler.scheduleAtFixedRate(
      () => Try(logger.info(statusLine())),
      intervalSeconds, intervalSeconds, TimeUnit.SECONDS)
    logger.info(s"WorkerHeartbeat started; logging queue depth every ${intervalSeconds}s.")
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
