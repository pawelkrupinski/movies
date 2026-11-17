package tools

import java.util.concurrent.{ExecutorService, ScheduledExecutorService}
import scala.concurrent.ExecutionContextExecutorService

/**
 * Builds the named threads a long-lived service runs on: its scheduler, its virtual-thread hand-off, its ordered
 * single thread. A service takes a `threadPools: ThreadPools` and asks it for each pool by name, so the composition
 * root decides what a pool is and who shuts it, and a spec hands in a [[ManualScheduler]] or a counting one.
 *
 * The class of bug this closes: fifteen services each built their own `DaemonExecutors.scheduler(…)` in a
 * `private val`, so a pool was the service's to shut and nothing else's — the one whose `stop()` no wiring called
 * ticked on past the root's stop, and no spec could swap in a stepped scheduler without a per-class seam.
 * `NoUnwiredThreadsAndWaitsSpec` fails the build on a `DaemonExecutors.…(` outside the wiring.
 *
 * A service still shuts what it was handed in its own `stop()` (it stops ticking when stopped, as before); the
 * managed pools are ALSO shut by the root's `ManagedResources.closeAll`, which is what makes a forgotten `stop()`
 * harmless. Build a service through it only once per root: each pool stays registered until the root stops.
 */
trait ThreadPools {
  /** A scheduler on `threads` daemon platform threads, each named `name` ([[DaemonExecutors.scheduler]]). */
  def scheduler(name: String, threads: Int = 1): ScheduledExecutorService

  /** Unbounded virtual threads named `name-N` ([[DaemonExecutors.virtualThreadEC]]). */
  def virtualThreadEC(name: String): ExecutionContextExecutorService

  /** One daemon platform thread named `name`, FIFO, `queueCapacity` deep ([[DaemonExecutors.singleThreadExecutor]]). */
  def singleThreadExecutor(name: String, queueCapacity: Int): ExecutorService
}

object ThreadPools {

  /** The process's daemon pools, unregistered: whoever asks shuts what it gets. For a spec, a program's `main`,
   *  and the roots' managed pools below. */
  val Unmanaged: ThreadPools = new ThreadPools {
    def scheduler(name: String, threads: Int): ScheduledExecutorService     = DaemonExecutors.scheduler(name, threads)
    def virtualThreadEC(name: String): ExecutionContextExecutorService       = DaemonExecutors.virtualThreadEC(name)
    def singleThreadExecutor(name: String, queueCapacity: Int): ExecutorService =
      DaemonExecutors.singleThreadExecutor(name, queueCapacity)
  }

  /** Each pool registered with `resources` as it is built, so the root's `closeAll` shuts it. */
  def managedBy(resources: ManagedResources): ThreadPools = new ThreadPools {
    def scheduler(name: String, threads: Int): ScheduledExecutorService =
      resources.executor(name)(Unmanaged.scheduler(name, threads))
    def virtualThreadEC(name: String): ExecutionContextExecutorService = resources.executor(name)(Unmanaged.virtualThreadEC(name))
    def singleThreadExecutor(name: String, queueCapacity: Int): ExecutorService =
      resources.executor(name)(Unmanaged.singleThreadExecutor(name, queueCapacity))
  }

}
