package tools

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService}

/**
 * Factories for daemon-thread executors. Every background pool/scheduler in
 * the app shared the same recipe — fixed thread pool or single-thread
 * scheduler, daemon-flagged threads, named for the logs. CLAUDE.md
 * threshold-2 rule: extracted on the second copy.
 */
object DaemonExecutors {
  /** Fixed-size pool of `workers` daemon threads named `${nameBase}-${i}`. */
  def fixedPool(nameBase: String, workers: Int): ExecutorService = {
    val counter = new AtomicInteger(0)
    Executors.newFixedThreadPool(workers, { r: Runnable =>
      val t = new Thread(r, s"$nameBase-${counter.incrementAndGet()}")
      t.setDaemon(true)
      t
    })
  }

  /** Single-thread scheduled executor with a daemon thread named `name`. */
  def scheduler(name: String): ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor { r =>
      val t = new Thread(r, name)
      t.setDaemon(true)
      t
    }
}
