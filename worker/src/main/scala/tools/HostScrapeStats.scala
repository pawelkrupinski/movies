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
 * Keyed by HOST, so a whole chain (Helios, Cinema City, Multikino) whose venues
 * all share one `scrapeHosts` set pools into one robust baseline and a single
 * ballooning venue is caught against the chain's normal speed — AND by venue,
 * because one host's venues are not all the same size. Cineamo serves ~800
 * German venues ten showings a page: a village cinema is one page, a multiplex
 * forty-odd, so the host's median is a small venue's scrape and its budget (the
 * 8s floor) cut every multiplex on it, every run (2026-10-12: 29 German venues,
 * UCI Bochum and Kinopolis Bad Godesberg never once scraped). So a venue is held
 * to the LARGER of its host's usual time and its own, and a venue that has not
 * yet completed a scrape gets the ceiling: the host's median says nothing about
 * how big it is. A venue that stalls is still cut, at twice that larger usual
 * time — so a small venue on a slow host gets the host's slack, not twice its own.
 */
class HostScrapeStats(
  window:     Int            = HostScrapeStats.DefaultWindow,
  /** How many of a venue's own scrapes are kept — few, as a venue scrapes a handful of
   *  times a day and the per-venue map spans the whole roster. */
  venueWindow: Int           = HostScrapeStats.DefaultVenueWindow,
  minSamples: Int            = HostScrapeStats.DefaultMinSamples,
  multiplier: Double         = HostScrapeStats.DefaultMultiplier,
  floor:      FiniteDuration = HostScrapeStats.DefaultFloor,
  ceiling:    FiniteDuration = HostScrapeStats.DefaultCeiling
) {
  require(window >= 1, s"window must be ≥ 1, was $window")
  require(venueWindow >= 1, s"venueWindow must be ≥ 1, was $venueWindow")
  require(minSamples >= 1, s"minSamples must be ≥ 1, was $minSamples")
  require(multiplier > 0, s"multiplier must be > 0, was $multiplier")
  require(floor <= ceiling, s"floor ($floor) must be ≤ ceiling ($ceiling)")

  // host key -> recent successful scrape durations in ms (oldest first), bounded
  // to `window`. The ConcurrentHashMap guards the map; each deque is guarded by
  // synchronising on itself (concurrent scrapes can record/read the same host).
  private val samplesByHost = new java.util.concurrent.ConcurrentHashMap[String, mutable.ArrayDeque[Long]]()

  // venue key -> that venue's own recent successful durations, bounded to `venueWindow`.
  private val samplesByVenue = new java.util.concurrent.ConcurrentHashMap[String, mutable.ArrayDeque[Long]]()

  /** Record a completed scrape of `venue` on `host`. Negative values (a clock
   *  going backwards) are ignored rather than poisoning the median. */
  def record(host: String, venue: String, durationMs: Long): Unit = {
    if (durationMs < 0) return
    append(samplesByHost, host, durationMs, window)
    append(samplesByVenue, venue, durationMs, venueWindow)
  }

  /** The adaptive budget a single scrape of `venue` on `host` is allowed:
   *  `multiplier ×` the larger of the host's rolling median and the venue's own,
   *  clamped to `[floor, ceiling]` — or the `ceiling` alone while the host has
   *  fewer than `minSamples`, or the venue has never completed a scrape. */
  def deadlineFor(host: String, venue: String): FiniteDuration = {
    val hostSamples  = snapshot(samplesByHost, host)
    val venueSamples = snapshot(samplesByVenue, venue)
    if (hostSamples.length < minSamples || venueSamples.isEmpty) ceiling
    else {
      val usual   = math.max(HostScrapeStats.median(hostSamples), HostScrapeStats.median(venueSamples))
      val raw     = (usual * multiplier).round
      math.max(floor.toMillis, math.min(ceiling.toMillis, raw)).millis
    }
  }

  private def append(by: java.util.concurrent.ConcurrentHashMap[String, mutable.ArrayDeque[Long]],
                     key: String, durationMs: Long, bound: Int): Unit = {
    val deque = by.computeIfAbsent(key, _ => mutable.ArrayDeque.empty[Long])
    deque.synchronized {
      deque.append(durationMs)
      while (deque.size > bound) deque.removeHead()
    }
  }

  private def snapshot(by: java.util.concurrent.ConcurrentHashMap[String, mutable.ArrayDeque[Long]], key: String): Array[Long] =
    Option(by.get(key)) match {
      case Some(deque) => deque.synchronized(deque.toArray)
      case None        => Array.empty[Long]
    }
}

object HostScrapeStats {
  /** Rolling window of recent scrapes per host — enough to smooth out jitter,
   *  short enough that a host genuinely getting faster/slower is reflected
   *  within a few refresh ticks. */
  val DefaultWindow: Int = 20

  /** A venue's own window: its last few scrapes. */
  val DefaultVenueWindow: Int = 5

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
