package tools

import play.api.Logging

import java.net.URI
import java.util.Locale
import scala.concurrent.duration._

/**
 * A PROACTIVE per-host request pacer: each paced host's requests are spaced `intervalFor(url)` apart by the slots
 * `pace` hands out ([[FleetHostPace]]), and a caller parks until its slot comes round. Hosts WITHOUT an interval pass
 * straight through — the common case pays nothing but a lookup miss.
 *
 * [[ThrottledHttpFetch]] is the reactive half of rate-limit handling: it waits only once a host has already answered
 * 429. That is the right behaviour for a host we normally stay well under (TMDB), but the wrong shape for one we
 * out-run structurally — there, every sweep discovers the limit by tripping it, the fleet stands down for the pause,
 * and the scrape budget upstream ([[services.cinemas.common.AdaptiveTimeoutScraper]]) cuts the venue before the retry
 * lands. Pacing is global per host, not per thread, so it holds however many worker threads or `ParallelDetailFetch`
 * slots fan out onto the same origin — the property the burst-y `maxConcurrent` caps alone cannot give.
 *
 * Two shapes, one class, told apart only by their `pace` and `horizon`:
 *  - one process ([[PacedHttpFetch.perProcess]]: an [[InMemoryFleetHostPace]], no horizon) — the `HostPolicies`
 *    `minRequestInterval`, every slot waited for however far off. Wired INSIDE [[HostCircuitBreakerHttpFetch]], so a
 *    fast-failed call neither takes nor waits for a slot, and inside [[ThrottledHttpFetch]], so a 429 gate still
 *    overrides the steady-state pace.
 *  - the whole fleet (a [[MongoFleetHostPace]], [[PacedHttpFetch.FleetHorizon]]) — a host every country's worker shares
 *    from one egress, Wikidata ([[HostPolicies.fleetIntervalFor]]). A slot within `horizon` is waited for; one further
 *    off is not: the request fails fast as the host's breaker does, a [[CircuitOpenException]] naming when to come
 *    back, so a queue task gives its attempt back and waits until then (`HandlerOutcome.Deferred`) while every other
 *    task goes on — backpressure, never a pool thread parked behind a backlog. Wired outside the breaker and the 429
 *    gate: a call it turns back never reaches them.
 *
 * `paceName` is how the logs and a turned-back request name this pace.
 */
final class PacedHttpFetch(delegate: HttpFetch, pace: FleetHostPace, intervalFor: String => Option[FiniteDuration],
                           horizon: Duration, clock: java.time.Clock, sleep: Long => Unit = Thread.sleep,
                           paceName: String = "the pace")
    extends HttpFetch with Logging {

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase(Locale.ROOT))

  /** Take this host's next slot and park until it comes round — or, past the horizon, turn the request back. */
  private def paced[T](url: String)(block: => T): T = {
    for { interval <- intervalFor(url); host <- hostOf(url) } {
      val at = clock.instant()
      pace.take(host, interval, horizon, at) match {
        case Right(slot) =>
          val waitMs = java.time.Duration.between(at, slot).toMillis
          if (waitMs > 0) sleep(waitMs)
        case Left(next) =>
          throw new CircuitOpenException(host, java.time.Duration.between(at, next).toMillis.max(1L), Some(s"$paceName for this host"))
      }
    }
    block
  }

  override def get(url: String): String = paced(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = paced(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = paced(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String = paced(url)(delegate.post(url, body, contentType))

  /** Delegated UNPACED, matching [[ThrottledHttpFetch]]'s treatment of the same method. Pacing here would mean blocking
   *  a pool thread on `sleep` before handing off, or degrading `RealHttpFetch`'s genuine async path into
   *  `supplyAsync(get)` on the common pool — the exact bounce HeliosClient was moved off getAsync to avoid. No
   *  production caller uses this today; the warning fires if one ever routes a PACED host through it, so the hole
   *  surfaces loudly instead of silently unpacing that host. */
  override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] = {
    if (intervalFor(url).isDefined) logger.warn(s"getAsync bypasses $paceName configured for $url — use get to stay paced.")
    delegate.getAsync(url)
  }
}

object PacedHttpFetch {

  /** How far off a fleet slot is still waited for: further, and the request comes back later instead of holding its thread. */
  val FleetHorizon: FiniteDuration = 2.seconds

  /** This process's own pace: its slots in memory, every one waited for however far off. */
  def perProcess(delegate: HttpFetch, intervalFor: String => Option[FiniteDuration], clock: java.time.Clock,
                 sleep: Long => Unit = Thread.sleep): PacedHttpFetch =
    new PacedHttpFetch(delegate, new InMemoryFleetHostPace, intervalFor, Duration.Inf, clock, sleep)

  /** The pacing interval for `url`'s host, read off the one per-host policy table
   *  ([[HostPolicies]]) so a paced host is a DATA row like any
   *  other host override — never an if-branch here. */
  def configuredInterval(configuration: settings.ProcessConfiguration): String => Option[FiniteDuration] =
    url => HostPolicies.requestIntervalFor(url, configuration).map(_.toMillis.millis)
}
