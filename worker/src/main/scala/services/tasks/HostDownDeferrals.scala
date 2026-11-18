package services.tasks

import play.api.Logging
import tools.{AllBackendsFailedException, CircuitOpenException}

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap

/**
 * A task whose fetch the host's circuit breaker refused never reached the wire: it is `Deferred` — the
 * attempt given back and the wait the breaker's own remaining block — instead of charged a doubling backoff
 * for work refused locally, which retries the whole estate out of existence while the host is still down
 * (see `HandlerOutcome.Deferred` for the 2026-07-28 Odeon numbers). A chain of egress routes (proxy ->
 * direct) that failed on every route with one of them circuit-open is the same host-down, wrapped
 * (`AllBackendsFailedException.hostDown`).
 */
final class HostDownDeferrals(clock: Clock) extends Logging {

  /** Per host, until when its deferrals have been said: the first task a block defers says so at INFO, the
   *  others of the same block at DEBUG — the breaker has announced the outage already, and a block deferring a
   *  venue's every chunk each minute was 6,243 INFO lines for Ocine Gavarres alone (2026-10-29..11-12). */
  private val saidUntil = new ConcurrentHashMap[String, Instant]()

  /** The deferral `failure` calls for, when it is a host down; `label` names the task in what is logged. */
  def deferral(failure: Exception, label: String): Option[HandlerOutcome] = hostDown(failure).map { open =>
    val notBefore = clock.instant().plusMillis(open.openForMs)
    say(open.host, notBefore, s"$label deferred: ${open.getMessage}")
    HandlerOutcome.Deferred(Some(open.getMessage), Some(notBefore))
  }

  /** The block to wait out, when the task's host is down: its breaker open, bare or behind every egress route. */
  private def hostDown(e: Exception): Option[CircuitOpenException] = e match {
    case open: CircuitOpenException => Some(open)
    case other                      => AllBackendsFailedException.hostDown(other)
  }

  private def say(host: String, notBefore: Instant, line: String): Unit = {
    val now  = clock.instant()
    val said = saidUntil.get(host)
    if (said == null || !now.isBefore(said)) { saidUntil.put(host, notBefore); logger.info(line) }
    else logger.debug(line)
  }
}
