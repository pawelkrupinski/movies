package services.tasks

/** What [[ScrapeReaper]] (and the other reapers) need to know to back off: is the
 *  worker currently CPU-credit throttled, and (for the log) how slow are scrapes
 *  running. Kept a narrow trait so the reapers depend on the abstraction, not the
 *  concrete signal source — and so tests can inject a fixed signal. */
trait ScrapeThrottleSignal {
  def isThrottled: Boolean
  /** A representative "how slow are scrapes right now" figure, purely for the
   *  backoff log. 0 when the signal isn't duration-derived (the credit-balance
   *  sources, which know credit directly and read no scrape timings). */
  def slowScrapeMillis: Long
}

object ScrapeThrottleSignal {
  /** The do-nothing signal: never throttled. Default for callers (and tests)
   *  that don't wire a real source — so the reaper behaves exactly as before. */
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
   *  credit gate (the Grafana-driven backstop) with the in-process
   *  [[CpuCreditPoller]] (the authoritative source, reading the real credit
   *  balance), so a real crunch trips even if one source is silent. */
  def either(a: ScrapeThrottleSignal, b: ScrapeThrottleSignal): ScrapeThrottleSignal =
    new ScrapeThrottleSignal {
      def isThrottled: Boolean = a.isThrottled || b.isThrottled
      def slowScrapeMillis: Long = math.max(a.slowScrapeMillis, b.slowScrapeMillis)
    }
}
