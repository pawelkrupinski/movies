package services.tasks

/** Lifecycle hook the [[services.tasks.TaskWorker]] calls so instrumentation
 *  stays out of the worker's hot path. Segregated to the two moments the worker
 *  knows about — a claim, and the outcome of running the handler — so a metrics
 *  sink (or a test spy) implements just these, not the whole queue contract. */
trait TaskObserver {
  /** A worker claimed `task` and is about to run its handler. */
  def onStarted(task: Task): Unit
  /** The handler returned: `outcome` is one of [[WorkerTaskMetrics.Outcomes]];
   *  `handleMillis` is the handler's wall-clock (0 for `no_handler`, which never
   *  ran one). */
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit
}

object TaskObserver {
  val NoOp: TaskObserver = new TaskObserver {
    def onStarted(task: Task): Unit                                       = ()
    def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = ()
  }

  /** Fan one worker's lifecycle callbacks out to several observers, so the
   *  [[TaskWorker]] still takes a single observer while every sink sees every
   *  task. */
  def composite(observers: TaskObserver*): TaskObserver = new TaskObserver {
    def onStarted(task: Task): Unit                                       = observers.foreach(_.onStarted(task))
    def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = observers.foreach(_.onFinished(task, outcome, handleMillis))
  }
}
