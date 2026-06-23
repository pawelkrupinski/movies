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
class WorkerHeartbeat(
  queue:           TaskQueue,
  intervalSeconds: Long      = 60,
  now:             () => Long = () => System.currentTimeMillis()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("worker-heartbeat")

  // Wall-clock of the last scheduler firing — the worker's LIVENESS pulse. The
  // LivenessWatchdog reads it to tell a wedged JVM (heartbeat thread starved/dead,
  // e.g. the GC death-spiral that preceded the 2026-06-23 OOM, where the process
  // limped on but stopped doing anything) from a merely throttled one (the thread
  // keeps firing). Seeded to construction time so a freshly-built worker never
  // reads as stale before the first tick. Stamped at the TOP of each firing, ahead
  // of the queue-depth read, so a slow/hung Mongo `countByState` can't delay the
  // pulse — only an actually-stuck scheduler thread does.
  @volatile private var lastBeatMillis: Long = now()

  /** Wall-clock millis of the most recent heartbeat firing (or construction). */
  def lastTickMillis: Long = lastBeatMillis

  /** One heartbeat firing: stamp the liveness pulse, then log the queue depth.
   *  Package-private so the test can drive it without the scheduler. */
  private[tasks] def beat(): Unit = {
    lastBeatMillis = now()
    Try(logger.info(statusLine()))
    ()
  }

  /** The one-line queue-depth summary. Package-private so the test can assert it
   *  without driving the scheduler. */
  private[tasks] def statusLine(): String = {
    val counts = queue.countByState()
    val waiting  = counts.getOrElse(TaskState.Waiting, 0L)
    val workedOn = counts.getOrElse(TaskState.WorkedOn, 0L)
    s"worker heartbeat: queue waiting=$waiting worked_on=$workedOn (backlog=${waiting + workedOn})"
  }

  def start(): Unit = {
    lastBeatMillis = now()
    scheduler.scheduleAtFixedRate(
      () => beat(),
      intervalSeconds, intervalSeconds, TimeUnit.SECONDS)
    logger.info(s"WorkerHeartbeat started; logging queue depth every ${intervalSeconds}s.")
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
