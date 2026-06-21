package services.tasks

import services.metrics.{TaskObserver, WorkerTaskMetrics}
import tools.Env

import java.util.concurrent.atomic.AtomicReference

/** What [[ScrapeReaper]] needs to know to back off: is the worker currently
 *  CPU-credit throttled, and (for the log) how slow are scrapes running. Kept a
 *  narrow trait so the reaper depends on the abstraction, not the monitor — and
 *  so tests can inject a fixed signal. */
trait ScrapeThrottleSignal {
  def isThrottled: Boolean
  /** A representative "how slow are scrapes right now" figure, purely for the
   *  backoff log. 0 when the signal isn't duration-derived (the external credit
   *  gate, which knows credit directly and reads no scrape timings). */
  def slowScrapeMillis: Long
}

object ScrapeThrottleSignal {
  /** The do-nothing signal: never throttled. Default for callers (and tests)
   *  that don't wire the monitor — so the reaper behaves exactly as before. */
  val AlwaysHealthy: ScrapeThrottleSignal = new ScrapeThrottleSignal {
    def isThrottled: Boolean = false
    def slowScrapeMillis: Long = 0L
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
      def slowScrapeMillis: Long = math.max(a.slowScrapeMillis, b.slowScrapeMillis)
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
 * This is that fix's in-process FAIL-SAFE: the authoritative control is the
 * external credit gate ([[ExternalThrottleGate]], driven by a Grafana alert on
 * the real `fly_instance_cpu_balance`); this monitor backstops the window where
 * that pusher is silent — the first minutes after a reboot (the gate resets to
 * off, and a still-firing alert won't re-POST until its repeat interval), or a
 * full Grafana outage. It taps the [[TaskObserver]] lifecycle the [[TaskWorker]]
 * already calls, reads each finished `ScrapeCinema` handler's wall-clock as a
 * self-contained throttle SIGNAL (no Fly cpu_balance read needed), and exposes a
 * HYSTERETIC `isThrottled` the reapers consult to trickle their enqueue.
 *
 * Why a windowed POPULATION, not an EWMA of all durations: scrape wall-clock is
 * an ambiguous proxy. Real credit throttle slows the WHOLE fleet at once — most
 * recent scrapes are slow together. A single slow/stalled remote host (slow TLS,
 * a hung detail fetch) slows only ITS scrape while the rest stay ~3s — pure
 * network wait, not CPU starvation. An EWMA blends them, so one 57s outlier among
 * fast scrapes alone crossed the old enter line and tripped a needless backoff
 * while real credit sat at ~48k (observed 2026-06-21). Instead we count how many
 * of the last `windowSize` scrapes were slow and trip only when a MAJORITY are:
 * an isolated outlier can never reach the enter count, but a credit-throttled
 * fleet trips within a few ticks. Hysteresis (enter high, exit low) keeps the
 * reaper from oscillating on the boundary.
 *
 * Updated from worker-pool threads (one call per finished task); a lock-free
 * `AtomicReference` keeps the hot path cheap and the reaper-thread read of
 * `isThrottled` consistent.
 */
class ScrapeThrottleMonitor(
  // A single scrape is "slow" at/above this — well past the ~3s healthy baseline
  // (even a slow-TLS host rarely exceeds it) but far under the 20-70s throttled
  // baseline, so genuine credit-throttle scrapes all count as slow.
  slowMillis: Long = Env.positiveLong("KINOWO_SCRAPE_THROTTLE_SLOW_MS", 12000L),
  // How many recent ScrapeCinema durations to weigh. Small enough to react within
  // a few ticks, large enough that one outlier is a minority.
  windowSize: Int = Env.positiveInt("KINOWO_SCRAPE_THROTTLE_WINDOW", 6),
  // Trip throttled when at least this many of the last `windowSize` scrapes were
  // slow (a majority → the fleet is broadly slow, not one bad host).
  enterCount: Int = Env.positiveInt("KINOWO_SCRAPE_THROTTLE_ENTER_COUNT", 4),
  // While throttled, HOLD until the slow count drops to/below this (hysteresis —
  // the gap to `enterCount` is the band the reaper won't oscillate within).
  exitCount: Int = Env.positiveInt("KINOWO_SCRAPE_THROTTLE_EXIT_COUNT", 1)
) extends TaskObserver with ScrapeThrottleSignal {

  private case class State(window: Vector[Long], throttled: Boolean)
  private val state = new AtomicReference(State(Vector.empty, throttled = false))

  def onStarted(task: Task): Unit = ()

  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit =
    if (task.taskType == TaskType.ScrapeCinema && outcome == WorkerTaskMetrics.Outcome.Done)
      state.updateAndGet { s =>
        val window = (s.window :+ handleMillis).takeRight(windowSize)
        val slow   = window.count(_ >= slowMillis)
        // Hysteresis: while throttled, hold until the slow count falls to/below the
        // LOWER exit count; while healthy, trip only when it reaches the HIGHER enter.
        val throttled = if (s.throttled) slow > exitCount else slow >= enterCount
        State(window, throttled)
      }

  def isThrottled: Boolean = state.get().throttled
  // The slowest scrape currently in the window — a representative "how slow" for the log.
  def slowScrapeMillis: Long = { val w = state.get().window; if (w.isEmpty) 0L else w.max }
}
