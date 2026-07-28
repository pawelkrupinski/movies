package tools

import play.api.Logging

import java.net.URI
import java.time.{Duration => JDuration, Instant}
import java.util.concurrent.{CompletableFuture, CompletionException, ConcurrentHashMap}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.concurrent.duration._

/** Thrown — fast, with NO wire call — when a host's breaker is OPEN. An
 *  `IOException` so every caller that already degrades on a fetch failure (cinema
 *  scrapers wrap detail fetches in `Try(...).toOption`; `MonitoringHttpFetch`
 *  records a failure) treats a circuit-broken host exactly like an unreachable
 *  one — which, for us, it effectively is. */
class CircuitOpenException(host: String, openForMs: Long)
  extends java.io.IOException(s"circuit open for $host (${openForMs}ms before half-open)")

/**
 * A per-host circuit breaker wrapping any [[HttpFetch]]. When a host racks up
 * `failureThreshold` consecutive trip-worthy failures (request/connect timeouts,
 * 5xx, other IO errors), the breaker OPENS for `openDuration`: every further call
 * to that host fails FAST ([[CircuitOpenException]], ~0ms) instead of hitting the
 * wire and waiting out its own per-request timeout. After the cooldown ONE trial
 * call is allowed (half-open); a success closes the breaker, a fresh failure
 * re-opens it. Claiming that trial re-arms the window in the same atomic step
 * (see `admit`), so the queued fleet doesn't all rush the wire the instant the
 * cooldown elapses — and a trial that never returns can't wedge the host shut,
 * since the next cooldown admits a fresh one.
 *
 * Every open/re-open/close transition is announced, because a block that outlives
 * one cooldown is exactly the case the log has to carry: Filmstarts' ~5min 429
 * blocks (2026-07-28) re-opened this breaker silently every 60s, so a five-minute
 * DE-wide scrape blackout read as a single 60-second one.
 *
 * Why this exists: a slow/hanging host (Helios's `restapi.helios.pl`, 2026-06-23)
 * otherwise pins the [[ParallelDetailFetch]] slots (cap 2) for the FULL per-host
 * timeout on EVERY call — across many venues × screens — ballooning scrapes and
 * draining the worker's shared-cpu credit into a sustained throttle spiral. The
 * static per-host timeout policy in [[RealHttpFetch]] ([[RealHttpFetch.HostPolicies]])
 * shortens each individual hang; this generalises it to ANY host with no allowlist: after a few
 * hangs the host is skipped outright for a cooldown, so the worker stops paying
 * even the short timeout and the slot is freed for hosts that ARE answering.
 * Per-host, so one bad host never blocks the others.
 *
 * Other 4xx and non-failure outcomes never trip it — 404 is a real "not found"
 * signal for the MC/RT slug probes, not host trouble; sustained 429 is the one
 * 4xx that does (see `isTripWorthy`). `now` is injectable for tests. Wire it CLOSE
 * to the wire (inside `ThrottledHttpFetch`/`MonitoringHttpFetch`) so a fast-fail
 * still surfaces to uptime as the genuine unavailability it is.
 */
class HostCircuitBreakerHttpFetch(
  delegate:         HttpFetch,
  failureThreshold: Int            = 4,
  openDuration:     FiniteDuration = 60.seconds,
  now:              () => Instant  = () => Instant.now(),
  // Where the open/re-open/close transitions go. Defaults to this class's logger;
  // injected in tests so the transitions can be asserted on as data instead of
  // scraped out of captured log output (same seam as ThrottledHttpFetch's report).
  report:           Option[String => Unit] = None
) extends HttpFetch with Logging {
  import HostCircuitBreakerHttpFetch.Breaker

  private val breakers = new ConcurrentHashMap[String, Breaker]()

  private def hostOf(url: String): Option[String] =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.map(_.toLowerCase)

  /** Millis until this host's open breaker goes half-open, or 0 if it isn't open. */
  private[tools] def openRemainingMillis(host: String): Long =
    Option(breakers.get(host)).flatMap(_.openUntil)
      .map(until => JDuration.between(now(), until).toMillis)
      .filter(_ > 0L).getOrElse(0L)

  /** Route one transition to the injected sink, or to `level` on this class's
   *  logger in production — the sink is level-blind, so the call site picks. */
  private def announce(level: String => Unit, message: String): Unit =
    report.fold(level(message))(_(message))

  /** A clean round-trip — the host is alive, so drop any accrued failure state.
   *  Only a breaker that was actually OPEN announces its close: below the
   *  threshold nothing was announced, so the recovery isn't news either. */
  private def onSuccess(host: String): Unit =
    if (Option(breakers.remove(host)).exists(_.openUntil.isDefined))
      announce(logger.info(_), s"Circuit CLOSED for $host — its half-open probe succeeded.")

  /** A trip-worthy failure — count it; open (or re-open) at the threshold, and say
   *  which of the two it was. */
  private def onFailure(host: String): Unit = {
    val opened   = new AtomicBoolean(false)
    val reopened = new AtomicBoolean(false)
    breakers.compute(host, (_, prev) => {
      val wasOpen  = prev != null && prev.openUntil.isDefined
      val failures = (if (prev == null) 0 else prev.failures) + 1
      if (failures >= failureThreshold) {
        (if (wasOpen) reopened else opened).set(true)
        Breaker(failures, Some(now().plusMillis(openDuration.toMillis)))
      } else Breaker(failures, None)
    })
    if (opened.get())
      announce(logger.warn(_), s"Circuit OPEN for $host after $failureThreshold consecutive failures — " +
        s"skipping all calls to it for ${openDuration.toSeconds}s.")
    else if (reopened.get())
      announce(logger.warn(_), s"Circuit for $host STILL open — its half-open probe failed too; " +
        s"skipping all calls to it for another ${openDuration.toSeconds}s.")
  }

  /** Trip-worthy = the host is failing to serve us: a timeout (request OR connect —
   *  HttpConnectTimeoutException is a subtype), a 5xx, a 429, or a lower-level IO
   *  error (connection refused/reset).
   *
   *  429 is the one 4xx that counts. Every other 4xx is the host ANSWERING (404 is
   *  a real "not found" for the MC/RT slug probes) — but a 429 is it declining to,
   *  and `failureThreshold` CONSECUTIVE ones (any success resets the count) mean it
   *  is refusing us outright rather than shaping a burst. Without this the breaker
   *  could not open on the case that most needs it: Filmstarts 429'd every request
   *  for hours on 2026-07-18 while the worker kept firing ~14k of them an hour,
   *  since ThrottledHttpFetch's gate only paces retries and never gives up. */
  private def isTripWorthy(e: Throwable): Boolean = e match {
    case s: HttpStatusException                 => s.code >= 500 || s.code == 429
    case _: java.net.http.HttpTimeoutException  => true
    case _: java.io.IOException                 => true
    case _                                      => false
  }

  private def unwrap(e: Throwable): Throwable = e match {
    case ce: CompletionException if ce.getCause != null => ce.getCause
    case other                                          => other
  }

  /** Claim the right to hit the wire for `host`, or report the millis left on its
   *  open window. Admitting the half-open probe RE-ARMS that window in the same
   *  atomic step, so exactly one caller probes per `openDuration` — without it,
   *  the open check was a plain read and every caller that arrived once the window
   *  elapsed went to the wire together, which is not the "one trial call" this
   *  breaker promises. Re-arming also means a probe that never returns can't wedge
   *  the host shut: the next cooldown admits a fresh one. */
  private def admit(host: String): Long = {
    val remaining = new AtomicLong(0L)
    breakers.compute(host, (_, prev) =>
      if (prev == null) null // no breaker for this host — closed, nothing to claim
      else prev.openUntil match {
        case None => prev // accruing failures but closed
        case Some(until) =>
          val left = JDuration.between(now(), until).toMillis
          if (left > 0L) { remaining.set(left); prev }
          else Breaker(prev.failures, Some(now().plusMillis(openDuration.toMillis)))
      })
    remaining.get()
  }

  private def guarded[T](url: String)(block: => T): T = hostOf(url) match {
    case None => block // unparseable host — nothing to key a breaker on
    case Some(host) =>
      val remaining = admit(host)
      if (remaining > 0L) throw new CircuitOpenException(host, remaining)
      try { val result = block; onSuccess(host); result }
      // A NON-trip-worthy throw — a 404, a parse error — means the host ANSWERED, so it is
      // alive and the breaker closes exactly as a 200 would close it. Letting it fall
      // through unrecorded used to be harmless when the open check was a pure read, but
      // `admit` now re-arms the window as it hands out the half-open probe: a probe that
      // came back 404 would leave the host blocked for another full `openDuration` on the
      // strength of a reply that proves it recovered.
      catch {
        case e: Throwable if isTripWorthy(e) => onFailure(host); throw e
        case e: Throwable                    => onSuccess(host); throw e
      }
  }

  override def get(url: String): String = guarded(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = guarded(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = guarded(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    guarded(url)(delegate.post(url, body, contentType))

  override def getAsync(url: String): CompletableFuture[String] = hostOf(url) match {
    case None => delegate.getAsync(url)
    case Some(host) =>
      val remaining = admit(host)
      if (remaining > 0L) CompletableFuture.failedFuture(new CircuitOpenException(host, remaining))
      else delegate.getAsync(url).handle[String]((result, throwable) => {
        if (throwable == null) { onSuccess(host); result }
        // Same rule as `guarded`: a non-trip-worthy failure is the host answering, which
        // closes the breaker rather than leaving a re-armed window in place.
        else {
          if (isTripWorthy(unwrap(throwable))) onFailure(host) else onSuccess(host)
          throw throwable
        }
      })
  }
}

object HostCircuitBreakerHttpFetch {
  /** Per-host state: consecutive trip-worthy failures, and (once tripped) the
   *  instant the breaker goes half-open. `openUntil = None` ⇒ closed. */
  private[tools] case class Breaker(failures: Int, openUntil: Option[Instant])
}
