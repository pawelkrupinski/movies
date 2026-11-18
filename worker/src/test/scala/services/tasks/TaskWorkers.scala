package services.tasks

import settings.WorkerPoolSize
import tools.{ThreadPools, TickRecorder}

import java.time.Clock
import scala.concurrent.duration._

/** A [[TaskWorker]] assembled from its parts — dispatch, settlement, lease reaper — the way the worker's wiring
 *  assembles it, every setting defaulted as the parts default it: what the specs build a pool with. */
object TaskWorkers {

  def apply(
    queue:             TaskConsumer,
    handlers:          Seq[TaskHandler],
    processingTimeout: TaskWorker.ProcessingTimeout = TaskWorker.ProcessingTimeout(5.minutes),
    retryBackoff:      TaskWorker.RetryBackoff = TaskWorker.RetryBackoff(2.seconds),
    idleBackstop:      TaskWorker.IdleBackstop = TaskWorker.IdleBackstop(30.seconds),
    poolSize:          WorkerPoolSize = WorkerPoolSize(TaskWorker.DefaultPoolSize),
    reapInterval:      TaskWorker.ReapInterval = TaskWorker.ReapInterval(30.seconds),
    maxAttempts:       TaskWorker.MaxAttempts = TaskWorker.MaxAttempts(TaskWorker.DefaultMaxAttempts),
    onCompleted:       Task => Unit = _ => (),
    observer:          TaskObserver = TaskObserver.NoOp,
    stopGrace:         FiniteDuration = TaskWorker.StopGrace,
    clock:             Clock,
    ticks:             TickRecorder = TickRecorder.Unrecorded,
    threadPools:       ThreadPools,
    sleep:             FiniteDuration => Unit,
    stopwatch:         tools.Stopwatch
  ): TaskWorker = {
    val settlement = new TaskSettlement(queue, observer, maxAttempts, onCompleted, clock)
    new TaskWorker(queue, new TaskDispatch(handlers, settlement, stopwatch),
      new TaskLeaseReaper(queue, reapInterval, clock, ticks, threadPools),
      processingTimeout, retryBackoff, idleBackstop, poolSize, observer, stopGrace, clock, sleep, stopwatch)
  }
}
