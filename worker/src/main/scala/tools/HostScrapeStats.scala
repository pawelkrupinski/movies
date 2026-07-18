package tools

import scala.collection.mutable
import scala.concurrent.duration._

/**
 * In-memory, thread-safe per-host scrape-duration stats that back the adaptive
 * scrape timeout ([[services.cinemas.common.AdaptiveTimeoutScraper]]).
 *
 * For each host key we keep a bounded window of the most recent *successful*
 * scrape durations and derive a per-host budget: `multiplier × the rolling
 * median`, clamped to `[floor, ceiling]`. Until a host has `minSamples`
 * observations we hand back the `ceiling` alone — so a brand-new host (or one
 * whose stats were lost to a reboot, since this is in-memory by design — it
 * adds zero Mongo write load, which is the very thing the throttle fix is
 * protecting) is still bounded, just generously, during its short warm-up.
 *
 * Median, not mean, so one anomalous slow scrape (the exact thing we want to
 * cut) doesn't drag the baseline up and immunise the host against its own
 * cutoff. Only completed scrapes are recorded — a cut scrape never feeds the
 * baseline, so a host that stalls can't slowly normalise its own stalls.
 *
 * Keyed by HOST, not cinema, so a whole chain (Helios, Cinema City, Multikino)
 * whose venues all share one `scrapeHosts` set pools into one robust baseline
 * and a single ballooning venue is caught against the chain's normal speed.
 */
class HostScrapeStats(
  window:     Int            = HostScrapeStats.DefaultWindow,
  minSamples: Int            = HostScrapeStats.DefaultMinSamples,
  multiplier: Double         = HostScrapeStats.DefaultMultiplier,
  floor:      FiniteDuration = HostScrapeStats.DefaultFloor,
  ceiling:    FiniteDuration = HostScrapeStats.DefaultCeiling
) {
  require(window >= 1, s"window must be ≥ 1, was $window")
  require(minSamples >= 1, s"minSamples must be ≥ 1, was $minSamples")
  require(multiplier > 0, s"multiplier must be > 0, was $multiplier")
  require(floor <= ceiling, s"floor ($floor) must be ≤ ceiling ($ceiling)")

  // host key -> recent successful scrape durations in ms (oldest first), bounded
  // to `window`. The ConcurrentHashMap guards the map; each deque is guarded by
  // synchronising on itself (concurrent scrapes can record/read the same host).
  private val samplesByHost = new java.util.concurrent.ConcurrentHashMap[String, mutable.ArrayDeque[Long]]()

  /** Record a completed scrape's wall-clock for `host`. Negative values (a
   *  clock going backwards) are ignored rather than poisoning the median. */
  def record(host: String, durationMs: Long): Unit = {
    if (durationMs < 0) return
    val deque = samplesByHost.computeIfAbsent(host, _ => mutable.ArrayDeque.empty[Long])
    deque.synchronized {
      deque.append(durationMs)
      while (deque.size > window) deque.removeHead()
    }
  }

  /** The adaptive budget a single scrape of `host` is allowed: `multiplier ×`
   *  the rolling median once we have `≥ minSamples`, clamped to `[floor,
   *  ceiling]`; the `ceiling` alone during warm-up. */
  def deadlineFor(host: String): FiniteDuration = {
    val snapshot = Option(samplesByHost.get(host)) match {
      case Some(deque) => deque.synchronized(deque.toArray)
      case None        => Array.empty[Long]
    }
    if (snapshot.length < minSamples) ceiling
    else {
      val raw     = (HostScrapeStats.median(snapshot) * multiplier).round
      val clamped = math.max(floor.toMillis, math.min(ceiling.toMillis, raw))
      clamped.millis
    }
  }
}

object HostScrapeStats {
  /** Rolling window of recent scrapes per host — enough to smooth out jitter,
   *  short enough that a host genuinely getting faster/slower is reflected
   *  within a few refresh ticks. */
  val DefaultWindow: Int = 20

  /** Below this many observations a host stays on the `ceiling` budget — a
   *  median over one or two samples is too noisy to cut against. */
  val DefaultMinSamples: Int = 5

  /** "2× the usual time": a scrape running past double its host's median is the
   *  anomaly we cut. */
  val DefaultMultiplier: Double = 2.0

  /** Lower bound on the budget so a genuinely fast host (sub-second scrapes)
   *  isn't cut at 2× on ordinary network/GC jitter — the smallest stall worth
   *  reacting to. */
  val DefaultFloor: FiniteDuration = 8.seconds

  /** Hard upper bound regardless of how slow a host's median is. This is what
   *  caps a CHRONICALLY slow host (whose own 2× would be huge) and bounds the
   *  worst-case slot-pin that drains the worker's shared-cpu credit. Sized above
   *  the slowest healthy scrape we see so a legitimately-slow venue still
   *  completes, but well under the ~100s stalls that triggered the throttle. */
  val DefaultCeiling: FiniteDuration = 45.seconds

  private[tools] def median(xs: Array[Long]): Double = {
    val sorted = xs.sorted
    val n      = sorted.length
    if (n == 0) 0.0
    else if (n % 2 == 1) sorted(n / 2).toDouble
    else (sorted(n / 2 - 1) + sorted(n / 2)) / 2.0
  }
}
