package services

import scala.concurrent.duration.FiniteDuration

/** Lifecycle contract: every long-running service that owns a worker pool,
 *  scheduler, or other resource that must be drained at shutdown implements
 *  this. The composition root's `tools.ManagedResources` stops each one, newest
 *  first, before its Mongo connection closes. */
trait Stoppable {
  def stop(): Unit

  /** Stop, finishing within `budget` — what is left of the root's stop grace. A service whose
   *  stop drains a backlog overrides this to bound the drain; the rest need not. */
  def stopWithin(budget: FiniteDuration): Unit = stop()
}
