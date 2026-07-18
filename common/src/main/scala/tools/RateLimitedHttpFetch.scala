package tools

import play.api.Logging

import java.net.URI
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._

/**
 * A PROACTIVE per-host request pacer.
 *
 * [[ThrottledHttpFetch]] is the reactive half of rate-limit handling: it waits
 * only once a host has already answered 429. That is the right behaviour for a
 * host we normally stay well under (TMDB), but it is the wrong shape for one we
 * out-run structurally — there, every sweep discovers the limit by tripping it,
 * the fleet stands down for the pause, and the scrape budget upstream
 * ([[services.cinemas.AdaptiveTimeoutScraper]]) cuts the venue before the retry
 * lands. The 429s are then not an anomaly to back off from but our steady state.
 *
 * This decorator instead spaces requests to a host so the limit is not reached:
 * each configured host gets a single fleet-wide slot queue, and a caller claims
 * the next free slot and parks until it. Hosts WITHOUT a configured interval
 * pass straight through — the common case pays nothing but a map miss.
 *
 * Pacing is global per host, not per thread, so it holds regardless of how many
 * worker threads or `ParallelDetailFetch` slots fan out onto the same origin —
 * the property the burst-y `maxConcurrent` caps alone cannot give.
 *
 * Wire it INSIDE [[HostCircuitBreakerHttpFetch]] so a fast-failed call (breaker
 * open) neither consumes nor waits for a slot, and inside [[ThrottledHttpFetch]]
 * so a 429 gate still overrides the steady-state pace. `now`/`sleep` are
 * injectable for tests.
 */
class RateLimitedHttpFetch(
  delegate:    HttpFetch,
  intervalFor: String => Option[FiniteDuration] = RateLimitedHttpFetch.configuredInterval,
  now:         () => Instant = () => Instant.now(),
  sleep:       Long => Unit  = Thread.sleep
) extends HttpFetch with Logging {

  /** host -> the next free slot. Holding the NEXT slot (rather than the last
   *  used one) makes the claim a single atomic `compute`. */
  private val nextSlot = new ConcurrentHashMap[String, Instant]()

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase)

  /** Claim this host's next slot and park until it comes round. */
  private def paced[T](url: String)(block: => T): T = {
    for {
      interval <- intervalFor(url)
      host     <- hostOf(url)
    } {
      val claimed = nextSlot.compute(host, (_, previous) => {
        val earliest = now()
        val slot =
          if (previous == null || previous.isBefore(earliest)) earliest else previous
        slot.plusMillis(interval.toMillis)
      }).minusMillis(interval.toMillis)

      val waitMs = java.time.Duration.between(now(), claimed).toMillis
      if (waitMs > 0) sleep(waitMs)
    }
    block
  }

  override def get(url: String): String = paced(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String =
    paced(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = paced(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    paced(url)(delegate.post(url, body, contentType))

  /** Delegated UNPACED, matching [[ThrottledHttpFetch]]'s treatment of the same
   *  method. Pacing here would mean blocking a pool thread on `sleep` before
   *  handing off, or degrading `RealHttpFetch`'s genuine async path into
   *  `supplyAsync(get)` on the common pool — the exact bounce HeliosClient was
   *  moved off getAsync to avoid. No production caller uses this today; the
   *  warning fires if one ever routes a PACED host through it, so the hole
   *  surfaces loudly instead of silently unpacing that host. */
  override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] = {
    if (intervalFor(url).isDefined)
      logger.warn(s"getAsync bypasses the pace configured for $url — use get to stay paced.")
    delegate.getAsync(url)
  }
}

object RateLimitedHttpFetch {

  /** The pacing interval for `url`'s host, read off the one per-host policy table
   *  ([[RealHttpFetch.HostPolicies]]) so a paced host is a DATA row like any
   *  other host override — never an if-branch here. */
  def configuredInterval(url: String): Option[FiniteDuration] =
    RealHttpFetch.requestIntervalFor(url).map(_.toMillis.millis)
}
