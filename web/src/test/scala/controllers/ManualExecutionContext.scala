package controllers

import scala.concurrent.ExecutionContext

/** An `ExecutionContext` that runs nothing until told to: submitted tasks queue up
 *  and [[runAll]] executes them on the calling thread. Lets a spec observe the
 *  moment BETWEEN "a background render was scheduled" and "it finished" — the
 *  window stale-while-revalidate exists to serve from — deterministically, with
 *  no thread and no sleep. */
final class ManualExecutionContext extends ExecutionContext {
  private val queued = scala.collection.mutable.Queue.empty[Runnable]

  override def execute(runnable: Runnable): Unit = synchronized(queued.enqueue(runnable))
  override def reportFailure(cause: Throwable): Unit = throw cause

  /** How many tasks are waiting to run. */
  def pending: Int = synchronized(queued.size)

  /** Run every queued task, including any a task queues while running. */
  def runAll(): Unit = {
    var next = synchronized(if (queued.isEmpty) None else Some(queued.dequeue()))
    while (next.isDefined) {
      next.foreach(_.run())
      next = synchronized(if (queued.isEmpty) None else Some(queued.dequeue()))
    }
  }
}
