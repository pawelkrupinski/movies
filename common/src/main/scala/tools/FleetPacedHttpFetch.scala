package tools

import play.api.Logging

import java.net.URI
import java.time.Instant
import java.util.Locale
import scala.concurrent.duration.FiniteDuration

/**
 * Holds a host the whole fleet shares — Wikidata, asked by every country's worker from one egress — to one pace
 * ([[FleetHostPace]], `intervalFor`: [[HostPolicies.fleetIntervalFor]]). A slot within `horizon` is waited for; one
 * further off is not: the request fails fast as the host's breaker does, a [[CircuitOpenException]] naming when to come
 * back, so a queue task gives its attempt back and waits until then (`HandlerOutcome.Deferred`) while every other task
 * goes on — backpressure, never a pool thread parked behind a backlog. Wired outside the breaker and the 429 gate: a call
 * it turns back never reaches them.
 */
final class FleetPacedHttpFetch(delegate: HttpFetch, pace: FleetHostPace, intervalFor: String => Option[FiniteDuration],
                                horizon: FiniteDuration, now: () => Instant = () => Instant.now(), sleep: Long => Unit = Thread.sleep)
    extends HttpFetch with Logging {

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase(Locale.ROOT))

  private def paced[T](url: String)(block: => T): T = {
    for { interval <- intervalFor(url); host <- hostOf(url) } {
      val at = now()
      pace.take(host, interval, horizon, at) match {
        case Right(slot) =>
          val waitMs = java.time.Duration.between(at, slot).toMillis
          if (waitMs > 0) sleep(waitMs)
        case Left(next) =>
          throw new CircuitOpenException(host, java.time.Duration.between(at, next).toMillis.max(1L), Some("the fleet's pace for this host"))
      }
    }
    block
  }

  override def get(url: String): String = paced(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = paced(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = paced(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String = paced(url)(delegate.post(url, body, contentType))
  override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] = {
    if (intervalFor(url).isDefined) logger.warn(s"getAsync bypasses the fleet's pace configured for $url — use get to stay paced.")
    delegate.getAsync(url)
  }
}

object FleetPacedHttpFetch {
  /** How far off a slot is still waited for: further, and the request comes back later instead of holding its thread. */
  val Horizon: FiniteDuration = scala.concurrent.duration.Duration(2, java.util.concurrent.TimeUnit.SECONDS)
}
