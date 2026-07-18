package tools

import play.api.Logging

import java.net.URI
import java.time.{Duration => JDuration, Instant}
import java.util.concurrent.{CompletableFuture, CompletionException, ConcurrentHashMap}
import java.util.concurrent.atomic.AtomicBoolean
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
 * wire and waiting out its own per-request timeout. After the cooldown one trial
 * call is allowed (half-open); a success closes the breaker, a fresh failure
 * re-opens it.
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
  now:              () => Instant  = () => Instant.now()
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

  /** A clean round-trip — the host is alive, so drop any accrued failure state. */
  private def onSuccess(host: String): Unit = breakers.remove(host)

  /** A trip-worthy failure — count it; open (and log the transition) at the threshold. */
  private def onFailure(host: String): Unit = {
    val opened = new AtomicBoolean(false)
    breakers.compute(host, (_, prev) => {
      val wasOpen  = prev != null && prev.openUntil.isDefined
      val failures = (if (prev == null) 0 else prev.failures) + 1
      if (failures >= failureThreshold) {
        if (!wasOpen) opened.set(true)
        Breaker(failures, Some(now().plusMillis(openDuration.toMillis)))
      } else Breaker(failures, None)
    })
    if (opened.get())
      logger.warn(s"Circuit OPEN for $host after $failureThreshold consecutive failures — " +
        s"skipping all calls to it for ${openDuration.toSeconds}s.")
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

  private def guarded[T](url: String)(block: => T): T = hostOf(url) match {
    case None => block // unparseable host — nothing to key a breaker on
    case Some(host) =>
      val remaining = openRemainingMillis(host)
      if (remaining > 0L) throw new CircuitOpenException(host, remaining)
      try { val result = block; onSuccess(host); result }
      catch { case e: Throwable if isTripWorthy(e) => onFailure(host); throw e }
  }

  override def get(url: String): String = guarded(url)(delegate.get(url))
  override def get(url: String, headers: Map[String, String]): String = guarded(url)(delegate.get(url, headers))
  override def getBytes(url: String): Array[Byte] = guarded(url)(delegate.getBytes(url))
  override def post(url: String, body: String, contentType: String): String =
    guarded(url)(delegate.post(url, body, contentType))

  override def getAsync(url: String): CompletableFuture[String] = hostOf(url) match {
    case None => delegate.getAsync(url)
    case Some(host) =>
      val remaining = openRemainingMillis(host)
      if (remaining > 0L) CompletableFuture.failedFuture(new CircuitOpenException(host, remaining))
      else delegate.getAsync(url).handle[String]((result, throwable) => {
        if (throwable == null) { onSuccess(host); result }
        else { if (isTripWorthy(unwrap(throwable))) onFailure(host); throw throwable }
      })
  }
}

object HostCircuitBreakerHttpFetch {
  /** Per-host state: consecutive trip-worthy failures, and (once tripped) the
   *  instant the breaker goes half-open. `openUntil = None` ⇒ closed. */
  private[tools] case class Breaker(failures: Int, openUntil: Option[Instant])
}
