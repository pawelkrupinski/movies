package tools

import java.util.concurrent.{AbstractExecutorService, TimeUnit}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

/** A deterministic, same-thread `ExecutionContextExecutorService`: every submitted
 *  task runs INLINE on the calling thread, to completion, before `execute` returns.
 *
 *  Used by the determinism harness so the boot-phase enrichment cascade is driven
 *  by ORDERING alone — the seeded cinema-arrival shuffle decides the order TMDB
 *  resolutions fire — with NO concurrency and NO `Thread.sleep` jitter to perturb a
 *  thread pool. A single thread means there is no scheduler race, so a given seed is
 *  perfectly reproducible (the real virtual-thread pool + sleep-jitter it replaces
 *  was neither: same seed, different interleaving run-to-run). Safe because the
 *  resolution path is self-contained synchronous work — it never awaits a sub-future
 *  on this same executor, so running inline can't deadlock.
 *
 *  Production keeps the real `DaemonExecutors.virtualThreadEC`; this is wired in only
 *  by `TestWiring.enrichmentEC` overrides (see `StagingOrderDeterminismSpec`). */
object SameThreadExecutorService {
  def newEC(): ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(new AbstractExecutorService {
      @volatile private var stopped = false
      override def execute(command: Runnable): Unit = command.run()
      override def shutdown(): Unit = stopped = true
      override def shutdownNow(): java.util.List[Runnable] = { stopped = true; java.util.Collections.emptyList() }
      override def isShutdown: Boolean = stopped
      override def isTerminated: Boolean = stopped
      override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = { stopped = true; true }
    })
}
