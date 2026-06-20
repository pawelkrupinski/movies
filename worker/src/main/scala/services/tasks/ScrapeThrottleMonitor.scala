package services.tasks

import services.metrics.{TaskObserver, WorkerTaskMetrics}
import tools.Env

import java.util.concurrent.atomic.AtomicReference

/** What [[ScrapeReaper]] needs to know to back off: is the worker currently
 *  CPU-credit throttled, and (for the log) how slow are scrapes running. Kept a
 *  narrow trait so the reaper depends on the abstraction, not the EWMA monitor —
 *  and so tests can inject a fixed signal. */
trait ScrapeThrottleSignal {
  def isThrottled: Boolean
  def ewmaMillis:  Long
}

object ScrapeThrottleSignal {
  /** The do-nothing signal: never throttled. Default for callers (and tests)
   *  that don't wire the monitor — so the reaper behaves exactly as before. */
  val AlwaysHealthy: ScrapeThrottleSignal = new ScrapeThrottleSignal {
    def isThrottled: Boolean = false
    def ewmaMillis:  Long    = 0L
  }

  /** The per-tick enqueue cap a reaper should use: `normal` when healthy, trimmed
   *  to the `trickle` while the worker is CPU-credit throttled. ALL the reapers
   *  (scrape, detail, ratings, tmdb-retry) call this so the WHOLE task pipeline
   *  quiets under throttle — credit only rebuilds when the pool actually goes
   *  idle, which it can't if the scrape reaper backs off but the rating/detail
   *  reapers keep feeding it. */
  def cap(signal: ScrapeThrottleSignal, normal: Int, trickle: Int): Int =
    if (signal.isThrottled) trickle else normal

  /** Throttled when EITHER source says so — used to compose the externally-pushed
   *  credit gate (the intended control) with the in-process scrape-duration
   *  monitor (a fail-safe, in case the external pusher lags or stops). */
  def either(a: ScrapeThrottleSignal, b: ScrapeThrottleSignal): ScrapeThrottleSignal =
    new ScrapeThrottleSignal {
      def isThrottled: Boolean = a.isThrottled || b.isThrottled
      def ewmaMillis:  Long    = math.max(a.ewmaMillis, b.ewmaMillis)
    }
}

/**
 * Auto-recovery for the worker's shared-CPU-credit DEADLOCK. Once the credit
 * balance floors, every `ScrapeCinema` handler runs at the throttled baseline
 * (~20-70s vs ~3s healthy), the task queue never empties, so the pool is never
 * idle, so the box never earns the idle time it needs to rebuild credit — a
 * metastable pin that survives restarts (a restart's boot storm just re-drains
 * the freshly-rebuilt credit). The only escape is to do LESS work until credit
 * recovers; nothing does that automatically today (the standing "highest-value
 * fix" from the 2026-06-19 post-mortem — see project_worker_cpu_steal_boot_storm).
 *
 * This is that fix. It taps the [[TaskObserver]] lifecycle the [[TaskWorker]]
 * already calls, reads each finished `ScrapeCinema` handler's wall-clock as the
 * self-contained throttle SIGNAL (no Fly cpu_balance read needed), smooths it
 * into an EWMA, and exposes a HYSTERETIC `isThrottled`. `ScrapeReaper` consults
 * it and, while throttled, caps enqueue to a trickle so the backlog drains and
 * the pool earns idle; as credit rebuilds the handler durations fall, the EWMA
 * clears, and the full enqueue cap resumes. The trickle (not a hard stop) keeps
 * scrapes — hence this signal — alive so recovery is actually observed, instead
 * of the signal freezing high against an idle pool that never probes again.
 *
 * Updated from worker-pool threads (one call per finished task); a lock-free
 * `AtomicReference` keeps the hot path cheap and the reaper-thread read of
 * `isThrottled` consistent.
 */
class ScrapeThrottleMonitor(
  // Trip throttled when the duration EWMA climbs to/above this; clear it when the
  // EWMA falls below `recoverMillis`. The wide gap (≈3s healthy, 20s+ throttled)
  // is the hysteresis band — the reaper doesn't oscillate on per-cinema noise.
  throttleMillis: Long   = Env.positiveLong("KINOWO_SCRAPE_THROTTLE_ENTER_MS", 12000L),
  recoverMillis:  Long   = Env.positiveLong("KINOWO_SCRAPE_THROTTLE_EXIT_MS", 6000L),
  // EWMA smoothing — weight on the newest sample. 0.2 ≈ the last ~5 scrapes
  // dominate, enough to ride out one slow cinema but react within a few ticks.
  alpha:          Double = 0.2
) extends TaskObserver with ScrapeThrottleSignal {

  private case class State(ewmaMillis: Double, throttled: Boolean, samples: Long)
  private val state = new AtomicReference(State(0.0, throttled = false, samples = 0L))

  def onStarted(task: Task): Unit = ()

  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit =
    if (task.taskType == TaskType.ScrapeCinema && outcome == WorkerTaskMetrics.Outcome.Done)
      state.updateAndGet { s =>
        val ewma =
          if (s.samples == 0L) handleMillis.toDouble
          else alpha * handleMillis + (1 - alpha) * s.ewmaMillis
        // Hysteresis: while throttled, hold until the EWMA drops below the LOWER
        // exit line; while healthy, trip only when it crosses the HIGHER enter line.
        val throttled = if (s.throttled) ewma >= recoverMillis else ewma >= throttleMillis
        State(ewma, throttled, s.samples + 1)
      }

  def isThrottled: Boolean = state.get().throttled
  def ewmaMillis:  Long    = state.get().ewmaMillis.toLong
}
