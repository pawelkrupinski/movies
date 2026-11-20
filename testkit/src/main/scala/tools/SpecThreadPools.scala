package tools

import java.util.concurrent.{ExecutorService, ScheduledExecutorService}

import scala.concurrent.ExecutionContextExecutorService

/** [[ThreadPools]] for a spec. */
object SpecThreadPools {

  /** Every scheduler asked for is `stepped` (a [[ManualScheduler]], say), whatever its name; the other pools are
   *  [[ThreadPools.Unmanaged]]. */
  def scheduling(stepped: ScheduledExecutorService): ThreadPools = new ThreadPools {
    def scheduler(name: String, threads: Int): ScheduledExecutorService = stepped
    def virtualThreadEC(name: String): ExecutionContextExecutorService = ThreadPools.Unmanaged.virtualThreadEC(name)
    def singleThreadExecutor(name: String, queueCapacity: Int): ExecutorService =
      ThreadPools.Unmanaged.singleThreadExecutor(name, queueCapacity)
  }
}
