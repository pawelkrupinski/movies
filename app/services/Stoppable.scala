package services

/** Lifecycle contract: every long-running service that owns a worker pool,
 *  scheduler, or other resource that must be drained at shutdown implements
 *  this. `AppLoader` registers a shutdown hook that calls `stop()` on each
 *  one in the right order (publishers before subscribers, so in-flight
 *  events drain cleanly). */
trait Stoppable {
  def stop(): Unit
}
